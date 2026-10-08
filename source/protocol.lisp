;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; The smart protocol, version 0, as git speaks it over HTTP
;; (stateless-rpc): the refs advertisement, upload-pack (a fetch or a
;; clone) and receive-pack (a push).  Each is a function from the
;; request's octets to the response's; the HTTP layer above decides who
;; may ask and sets the content types.
;;
;; What is advertised: for upload-pack `multi_ack_detailed',
;; `side-band-64k', `thin-pack' (we send none, but a client may ask),
;; `ofs-delta' (deltas copied from our own packs go as ofs-deltas when
;; the client takes them), `no-progress', `include-tag',
;; `allow-tip-sha1-in-want' and `symref' for HEAD; for receive-pack `report-status', `delete-refs',
;; `side-band-64k', `quiet' and `ofs-delta' (we accept them).  Shallow
;; clones are not offered (a client asking --depth is told so by its
;; own git).
;;

(defparameter *agent* "agent=ingot/0.1")

(defparameter *zero-oid* (make-string 40 :initial-element #\0))

(defun capabilities-of (service repository)
  (ecase service
    (:upload-pack
     (format nil "multi_ack_detailed side-band-64k thin-pack ofs-delta no-progress include-tag allow-tip-sha1-in-want allow-reachable-sha1-in-want object-format=sha1 ~a~@[ symref=HEAD:~a~]"
             *agent* (head-ref repository)))
    (:receive-pack
     (format nil "report-status delete-refs side-band-64k quiet ofs-delta object-format=sha1 ~a" *agent*))))

(defun advertise-refs (repository service &key (smart-http t))
  "The refs advertisement for SERVICE (:upload-pack or :receive-pack), as
octets: for smart HTTP, with the service header first."
  (let ((buffer (make-buffer 4096))
        (refs (refs repository))
        (head (head-oid repository))
        (first? t))
    (when smart-http
      (write-pkt-line buffer (format nil "# service=git-~(~a~)~%" service))
      (write-pkt-flush buffer))
    (flet ((line (oid name)
             (if first?
                 (progn (write-pkt-line buffer (format nil "~a ~a~a~a~%" oid name (code-char 0) (capabilities-of service repository)))
                        (setq first? nil))
                 (write-pkt-line buffer (format nil "~a ~a~%" oid name)))))
      (when (and head (eq service :upload-pack)) (line head "HEAD"))
      (loop for (name . oid) in refs
            do (line oid name)
               ;; an annotated tag is also advertised peeled
               (when (and (> (length name) 10) (string= "refs/tags/" name :end2 10))
                 (let ((object (find-object repository oid)))
                   (when (and object (eq (object-type object) :tag))
                     (let ((peeled (peel-to-commit repository oid)))
                       (when peeled (write-pkt-line buffer (format nil "~a ~a^{}~%" peeled name))))))))
      (when first?
        ;; nothing to advertise: the capabilities ride on a line of their own
        (write-pkt-line buffer (format nil "~a capabilities^{}~a~a~%" *zero-oid* (code-char 0) (capabilities-of service repository)))))
    (write-pkt-flush buffer)
    (buffer-octets buffer)))

;;; upload-pack

(defun parse-upload-request (body)
  "A plist of what the client asked: :wants :haves :done? :capabilities
(strings) :shallows :deepen."
  (let ((wants nil) (haves nil) (done? nil) (caps nil) (shallows nil) (deepen nil) (at 0))
    (loop
      (multiple-value-bind (line next) (read-pkt-line body at)
        (setq at next)
        (cond ((null line) (return))
              ((eq line :flush))
              ((eq line :delim))
              (t (let* ((text (pkt-text line))
                        (space (position #\Space text))
                        (word (subseq text 0 (or space (length text))))
                        (rest (if space (subseq text (1+ space)) "")))
                   (cond ((string= word "want")
                          (let ((nul (position (code-char 0) rest)))
                            (when nul
                              (setq caps (uiop:split-string (subseq rest (1+ nul)) :separator " "))
                              (setq rest (subseq rest 0 nul))))
                          (let ((parts (uiop:split-string rest :separator " ")))
                            (push (first parts) wants)
                            (when (rest parts) (setq caps (append caps (rest parts))))))
                         ((string= word "have") (push rest haves))
                         ((string= word "done") (setq done? t))
                         ((string= word "shallow") (push rest shallows))
                         ((string= word "deepen") (setq deepen rest))
                         ((string= word "deepen-since") (setq deepen rest))
                         ((string= word "deepen-not") (setq deepen rest))
                         (t nil)))))))
    (list :wants (nreverse wants) :haves (nreverse haves) :done? done?
          :capabilities (remove "" caps :test #'string=) :shallows shallows :deepen deepen)))

(defun upload-pack (repository body &key (allowed-want? (constantly t)))
  "Answer a fetch: BODY is the client's request (wants, haves, done).
ALLOWED-WANT? says whether an id may be asked for that is not a ref's
tip.  Answers the response octets.  Without `done' the response is the
negotiation's ACKs and a NAK; with it, the ACK or NAK and the pack."
  (let* ((request (parse-upload-request body))
         (caps (getf request :capabilities))
         (side-band? (member "side-band-64k" caps :test #'string=))
         (multi-ack? (member "multi_ack_detailed" caps :test #'string=))
         (wants (getf request :wants))
         (haves (getf request :haves))
         (buffer (make-buffer (* 1024 1024)))
         (tips (mapcar #'cdr (refs repository))))
    (when (getf request :deepen)
      (return-from upload-pack
        (protocol-error buffer side-band? "Shallow clones are not offered here; fetch without --depth.")))
    (dolist (want wants)
      (unless (oid? want) (return-from upload-pack (protocol-error buffer side-band? (format nil "not a valid object id: ~a" want))))
      (unless (object-exists? repository want)
        (return-from upload-pack (protocol-error buffer side-band? (format nil "not our ref ~a" want))))
      (unless (or (member want tips :test #'string=) (funcall allowed-want? want))
        (return-from upload-pack (protocol-error buffer side-band? (format nil "not our ref ~a" want)))))
    (let ((common (remove-if-not (lambda (have) (and (oid? have) (object-exists? repository have))) haves)))
      (cond
        ((not (getf request :done?))
         ;; a round of negotiation
         (cond (multi-ack?
                (dolist (have common) (write-pkt-line buffer (format nil "ACK ~a common~%" have)))
                (when common (write-pkt-line buffer (format nil "ACK ~a ready~%" (car (last common)))))
                (write-pkt-line buffer (format nil "NAK~%")))
               (common (write-pkt-line buffer (format nil "ACK ~a~%" (first common))))
               (t (write-pkt-line buffer (format nil "NAK~%"))))
         (buffer-octets buffer))
        (t
         (if common
             (write-pkt-line buffer (format nil "ACK ~a~%" (car (last common))))
             (write-pkt-line buffer (format nil "NAK~%")))
         (let* ((oids (reachable-objects repository wants common))
                (oids (if (member "include-tag" caps :test #'string=)
                          (append oids (tags-pointing-into repository oids))
                          oids))
                (pack (buffer-octets (write-pack repository (remove-duplicates oids :test #'string= :from-end t)
                                                 :ofs-delta? (and (member "ofs-delta" caps :test #'string=) t)))))
           (cond (side-band?
                  (write-side-band buffer 1 pack)
                  (unless (member "no-progress" caps :test #'string=)
                    (write-side-band buffer 2 (string-to-octets (format nil "anvil: ~d object~:p sent~%" (length oids)))))
                  (write-pkt-flush buffer))
                 (t (buffer-write buffer pack)))
           (buffer-octets buffer)))))))

(defun tags-pointing-into (repository oids)
  "The ids of the annotated tags whose object is among OIDS and are not
themselves there: what include-tag adds."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (oid oids) (setf (gethash oid table) t))
    (loop for (nil . oid) in (tags repository)
          for object = (find-object repository oid)
          when (and object (eq (object-type object) :tag)
                    (not (gethash oid table))
                    (gethash (tag-object (parse-tag (object-data object) oid)) table))
            collect oid)))

(defun protocol-error (buffer side-band? text)
  "An error the client prints: an ERR pkt-line (git understands it on
either side of the band)."
  (declare (ignore side-band?))
  (write-pkt-line buffer (format nil "ERR ~a~%" text))
  (buffer-octets buffer))

;;; receive-pack

(defstruct ref-command
  old new name (status nil) (reason nil))

(defun parse-receive-commands (body)
  "Two values: the ref-commands the client sent (with the capabilities
it named as a third value), and the index in BODY where the pack
begins (the end of BODY when there is none)."
  (let ((commands nil) (caps nil) (at 0) (first? t))
    (loop
      (multiple-value-bind (line next) (read-pkt-line body at)
        (setq at next)
        (cond ((or (null line) (eq line :flush)) (return))
              ((eq line :delim))
              (t (let* ((text (pkt-text line))
                        (nul (position (code-char 0) text)))
                   (when (and first? nul)
                     (setq caps (remove "" (uiop:split-string (subseq text (1+ nul)) :separator " ") :test #'string=))
                     (setq text (subseq text 0 nul)))
                   (setq first? nil)
                   (let ((parts (uiop:split-string text :separator " ")))
                     (when (>= (length parts) 3)
                       (push (make-ref-command :old (first parts) :new (second parts)
                                               :name (format nil "~{~a~^ ~}" (cddr parts)))
                             commands))))))))
    (values (nreverse commands) at caps)))

(defun receive-pack (repository body &key (check (constantly nil)))
  "Take a push: BODY is the commands and the pack.  CHECK is a function
of a ref-command answering nil when it may happen, else a reason (a
string) for refusing it.  Values: the response octets (report-status),
and the list of ref-commands with their :status (:ok or :ng) and
:reason, so that the caller can run its hooks for what changed."
  (multiple-value-bind (commands pack-at caps) (parse-receive-commands body)
    (let* ((side-band? (member "side-band-64k" caps :test #'string=))
           (report? (member "report-status" caps :test #'string=))
           (quiet? (member "quiet" caps :test #'string=))
           (report (make-buffer 1024))
           (unpack-ok? t)
           (unpack-error nil))
      ;; the pack
      (when (< pack-at (length body))
        (handler-case
            (with-repository-lock (repository)
              (store-pack! repository (suboctets body pack-at)))
          (error (e)
            (setq unpack-ok? nil unpack-error (princ-to-string e)))))
      (write-pkt-line report (format nil "unpack ~a~%" (if unpack-ok? "ok" (or unpack-error "failed"))))
      ;; the commands
      (dolist (command commands)
        (let ((reason
                (cond ((not unpack-ok?) "unpacker error")
                      ((not (valid-ref-name? (ref-command-name command))) "funny refname")
                      ((and (not (string= (ref-command-new command) *zero-oid*))
                            (not (object-exists? repository (ref-command-new command))))
                       "missing necessary objects")
                      (t (funcall check command)))))
          (unless reason
            (handler-case
                (let ((old (ref-command-old command))
                      (new (ref-command-new command))
                      (name (ref-command-name command)))
                  (if (string= new *zero-oid*)
                      (delete-ref! repository name :old (if (string= old *zero-oid*) nil old))
                      (update-ref! repository name new :old (if (string= old *zero-oid*) nil old))))
              (ref-conflict () (setq reason "failed to update ref"))
              (error (e) (setq reason (princ-to-string e)))))
          (setf (ref-command-status command) (if reason :ng :ok)
                (ref-command-reason command) reason)
          (if reason
              (write-pkt-line report (format nil "ng ~a ~a~%" (ref-command-name command) reason))
              (write-pkt-line report (format nil "ok ~a~%" (ref-command-name command))))))
      (write-pkt-flush report)
      (let ((buffer (make-buffer 2048)))
        (cond ((not report?) nil)
              (side-band?
               (unless quiet?
                 (write-side-band buffer 2 (string-to-octets (format nil "anvil: ~d ref~:p updated~%"
                                                                     (count :ok commands :key #'ref-command-status)))))
               (write-side-band buffer 1 (buffer-octets report))
               (write-pkt-flush buffer))
              (t (buffer-write buffer (buffer-octets report))))
        (values (buffer-octets buffer) commands)))))

(defun fast-forward? (repository old new)
  "True when moving a ref from OLD to NEW loses nothing: OLD is absent,
or an ancestor of NEW."
  (or (null old) (string= old *zero-oid*) (ancestor? repository old new)))
