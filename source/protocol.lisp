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

;;; The client side: a fetch (a clone, or bringing a repository up to
;;; date) from another server over a transport the caller provides --
;;; a function of (SERVICE BODY) answering octets: with BODY nil, the
;;; refs advertisement (GET .../info/refs?service=git-SERVICE); with
;;; BODY, the service's answer to it (POST .../git-SERVICE).  Ingot
;;; knows no HTTP; the forge above lends it a client.

(defun parse-advertisement (octets &key (smart-http t))
  "Values: an alist of (name . oid) of the refs a server advertises
(peeled tag entries and the capabilities line left out), the
capability strings, and the ref HEAD names there when the server says
(symref=HEAD:...)."
  (let ((at 0) (refs nil) (caps nil) (head nil))
    (when smart-http
      ;; "# service=git-upload-pack" and a flush come first
      (multiple-value-bind (line next) (read-pkt-line octets 0)
        (when (and (vectorp line) (plusp (length line)) (= (aref line 0) (char-code #\#)))
          (setq at next)
          (multiple-value-bind (flush next2) (read-pkt-line octets at)
            (when (eq flush :flush) (setq at next2))))))
    (dolist (line (read-pkt-lines octets :start at))
      (unless (eq line :delim)
        (let* ((text (pkt-text line))
               (nul (position (code-char 0) text)))
          (when nul
            (setq caps (remove "" (uiop:split-string (subseq text (1+ nul)) :separator " ") :test #'string=)
                  text (subseq text 0 nul)))
          (when (and (> (length text) 41) (char= (char text 40) #\Space) (oid? (subseq text 0 40)))
            (let ((name (subseq text 41)))
              (unless (or (string= name "capabilities^{}")
                          (and (> (length name) 3) (string= "^{}" name :start2 (- (length name) 3))))
                (push (cons name (subseq text 0 40)) refs)))))))
    (dolist (cap caps)
      (when (and (> (length cap) 12) (string= "symref=HEAD:" cap :end2 12))
        (setq head (subseq cap 12))))
    (values (nreverse refs) caps head)))

(defparameter *fetch-capabilities* '("multi_ack_detailed" "side-band-64k" "ofs-delta" "no-progress" "include-tag")
  "What a fetch of ours asks of the server.")

(defun fetch-request (wants haves &key (capabilities *fetch-capabilities*))
  "The body of an upload-pack request asking for WANTS with HAVES here
already, ending in done: the whole negotiation in one round, as
stateless HTTP allows."
  (let ((buffer (make-buffer 4096)) (first? t))
    (dolist (want wants)
      (write-pkt-line buffer (if first?
                                 (format nil "want ~a ~{~a~^ ~} ~a~%" want capabilities *agent*)
                                 (format nil "want ~a~%" want)))
      (setq first? nil))
    (write-pkt-flush buffer)
    (dolist (have haves) (write-pkt-line buffer (format nil "have ~a~%" have)))
    (write-pkt-line buffer (format nil "done~%"))
    (buffer-octets buffer)))

(defun parse-fetch-response (octets)
  "What upload-pack answered: values the pack (octets; nil when none
came), the acknowledgement lines, and the progress text.  An error
band or an ERR line signals git-error."
  (let ((pack (make-buffer (max 1024 (length octets))))
        (acks nil) (progress (make-string-output-stream)) (at 0) (any-pack? nil))
    (loop
      ;; a server not asked for a side band sends the pack bare after its NAK
      (when (and (<= (+ at 4) (length octets)) (equalp (suboctets octets at (+ at 4)) (ascii "PACK")))
        (buffer-write pack octets :start at)
        (setq any-pack? t)
        (return))
      (multiple-value-bind (line next) (read-pkt-line octets at)
        (setq at next)
        (cond ((null line) (return))
              ((eq line :flush))
              ((eq line :delim))
              ((zerop (length line)))
              ((= (aref line 0) 1) (setq any-pack? t) (buffer-write pack line :start 1))
              ((= (aref line 0) 2) (write-string (octets-to-string line :start 1) progress))
              ((= (aref line 0) 3)
               (git-error "The server says: ~a" (string-trim '(#\Newline) (octets-to-string line :start 1))))
              (t (let ((text (pkt-text line)))
                   (cond ((and (>= (length text) 3) (string= "ERR" text :end2 3))
                          (git-error "The server says: ~a" (string-trim " " (subseq text 3))))
                         ((string= text "NAK"))
                         (t (push text acks))))))))
    (values (and any-pack? (buffer-octets pack)) (nreverse acks) (get-output-stream-string progress))))

(defun ref-under? (name prefixes)
  (some (lambda (prefix) (and (> (length name) (length prefix)) (string= prefix name :end2 (length prefix)))) prefixes))

(defun fetch! (repository transport &key (prefixes '("refs/heads/" "refs/tags/")) (prune? t) (set-head? t))
  "Bring REPOSITORY up to date with the server behind TRANSPORT (a
function of a service keyword and a body: nil asks for the refs
advertisement, octets for the service's answer to them): every ref
under PREFIXES fetched and set, those the server no longer has deleted
when PRUNE?, HEAD pointed where the server's points when SET-HEAD?.
What is here already is told to the server, so a second fetch brings
only what is new.  Values: the refs set (an alist), the refs deleted
(names), and the number of objects received."
  (multiple-value-bind (theirs caps head) (parse-advertisement (funcall transport :upload-pack nil))
    (declare (ignore caps))
    (let* ((wanted (remove-if-not (lambda (ref) (ref-under? (car ref) prefixes)) theirs))
           (ours (refs repository))
           (wants (remove-duplicates (remove-if (lambda (oid) (object-exists? repository oid)) (mapcar #'cdr wanted))
                                     :test #'string=))
           (haves (remove-duplicates (mapcar #'cdr ours) :test #'string=))
           (received 0))
      (when wants
        (let ((pack (parse-fetch-response (funcall transport :upload-pack (fetch-request wants haves)))))
          (when pack
            (with-repository-lock (repository)
              (setq received (length (store-pack! repository pack)))))))
      (dolist (ref wanted)
        (unless (object-exists? repository (cdr ref))
          (git-error "The server did not send ~a (~a)." (cdr ref) (car ref))))
      (let ((set nil) (deleted nil))
        (dolist (ref wanted)
          (unless (equal (cdr (assoc (car ref) ours :test #'string=)) (cdr ref))
            (update-ref! repository (car ref) (cdr ref))
            (push ref set)))
        (when prune?
          (dolist (ref ours)
            (when (and (ref-under? (car ref) prefixes) (not (assoc (car ref) wanted :test #'string=)))
              (delete-ref! repository (car ref))
              (push (car ref) deleted))))
        (when (and set-head? head (> (length head) 11) (string= "refs/heads/" head :end2 11)
                   (assoc head wanted :test #'string=))
          (set-default-branch! repository (subseq head 11)))
        (values (nreverse set) (nreverse deleted) received)))))
