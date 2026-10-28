;; Copyright © 2026 Genworks International
;;
;; SPDX-License-Identifier: MIT
;; Released under the MIT License; see the LICENSE file.

(in-package :ingot)

;;
;; What a commit says, read as git reads it: the subject and the body
;; (%s and %b), the trailers at the end of the message (Signed-off-by,
;; Co-Authored-By and their kin, by interpret-trailers' rules), whether
;; the commit carries a signature and of which kind, and an identity's
;; time written in its own zone.
;;

;;; Dates in the identity's zone

(defun zone-offset (zone)
  "The seconds east of UTC that ZONE (\"+0130\", \"-0400\") stands for; 0
when ZONE is malformed."
  (or (ignore-errors
       (let ((sign (if (char= (char zone 0) #\-) -1 1))
             (hours (parse-integer zone :start 1 :end 3))
             (minutes (parse-integer zone :start 3 :end 5)))
         (* sign (+ (* hours 3600) (* minutes 60)))))
      0))

(defun ident-decoded-time (ident)
  "IDENT's time decoded in IDENT's own zone: the values of
decode-universal-time (second minute hour day month year day-of-week)."
  (decode-universal-time (+ (universal-time (ident-time ident)) (zone-offset (ident-zone ident))) 0))

(defun ident-date-string (ident &key (style :iso))
  "IDENT's date as git prints it: :iso is %ai (2026-10-27 14:03:22 -0400),
:rfc2822 an email Date header (Tue, 27 Oct 2026 14:03:22 -0400)."
  (multiple-value-bind (s mi h d mo y dow) (ident-decoded-time ident)
    (ecase style
      (:iso (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d ~a" y mo d h mi s (ident-zone ident)))
      (:rfc2822 (format nil "~a, ~d ~a ~4,'0d ~2,'0d:~2,'0d:~2,'0d ~a"
                        (nth dow '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
                        d (nth (1- mo) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
                        y h mi s (ident-zone ident))))))

;;; Subject and body

(defun blank-line? (line)
  (zerop (length (string-trim '(#\Space #\Tab #\Return) line))))

(defun commit-subject-and-body (commit)
  "Two values, the message split as git's %s and %b split it: the
subject is the first paragraph with its lines joined by spaces, the body
everything after the blank line that ends it, trimmed."
  (let* ((lines (split-lines (commit-message commit)))
         (start (position-if-not #'blank-line? lines)))
    (if (null start)
        (values "" "")
        (let* ((rest (nthcdr start lines))
               (end (or (position-if #'blank-line? rest) (length rest)))
               (subject (format nil "~{~a~^ ~}"
                                (mapcar (lambda (line) (string-trim '(#\Space #\Tab #\Return) line))
                                        (subseq rest 0 end))))
               (body (format nil "~{~a~%~}" (nthcdr (1+ end) rest))))
          (values subject (string-trim '(#\Newline #\Space #\Return) body))))))

;;; Trailers

(defun trailer-line? (line)
  "Whether LINE is shaped like a trailer: a token of letters, digits and
dashes, a colon, a value; or git's own `(cherry picked from commit ...)`."
  (or (and (> (length line) 27) (string= "(cherry picked from commit " line :end2 27))
      (let ((colon (position #\: line)))
        (and colon (plusp colon)
             (every (lambda (c) (or (alphanumericp c) (char= c #\-)))
                    (string-right-trim " " (subseq line 0 colon)))))))

(defun trailer-block (message)
  "The trailer lines of MESSAGE as git's interpret-trailers finds them:
the last paragraph, after a blank line, when every line of it is a
trailer or a continuation -- or when it holds a Signed-off-by or a
cherry-pick line and at least a quarter of its lines are trailers."
  (let* ((lines (split-lines (string-right-trim '(#\Newline #\Space) message)))
         (blank (position-if #'blank-line? lines :from-end t)))
    (when (and blank (< (1+ blank) (length lines)))
      (let ((block (subseq lines (1+ blank)))
            (trailers 0) (others 0) (recognized nil))
        (dolist (line block)
          (cond ((trailer-line? line)
                 (incf trailers)
                 (when (or (and (>= (length line) 14) (string= "Signed-off-by:" line :end2 14))
                           (and (> (length line) 27) (string= "(cherry picked from commit " line :end2 27)))
                   (setq recognized t)))
                ((member (char line 0) '(#\Space #\Tab)))   ; a continuation of the line before
                (t (incf others))))
        (when (or (and (plusp trailers) (zerop others))
                  (and recognized (>= (* 3 trailers) others)))
          (remove-if-not #'trailer-line? block))))))

(defun commit-trailers (commit)
  "An alist of (token . value) of the trailers of COMMIT's message:
((\"Signed-off-by\" . \"A Person <a@example.org>\") ...)."
  (let ((result nil))
    (dolist (line (trailer-block (commit-message commit)) (nreverse result))
      (let ((colon (position #\: line)))
        (when (and colon (plusp colon))
          (push (cons (string-trim " " (subseq line 0 colon))
                      (string-trim " " (subseq line (1+ colon))))
                result))))))

;;; Signatures

(defun commit-signature (repository commit-oid)
  "What signature the commit carries: :ssh, :gpg, or nil.  Read from
the gpgsig header of the raw object, before the blank line that ends
the headers, so that a signature block quoted in the message is not
one."
  (let ((object (find-object repository commit-oid)))
    (when (and object (eq (object-type object) :commit))
      (let* ((text (octets-to-string (object-data object)))
             (sep (search (format nil "~%~%") text))
             (headers (if sep (subseq text 0 sep) text)))
        (cond ((search "BEGIN SSH SIGNATURE" headers) :ssh)
              ((search "BEGIN PGP SIGNATURE" headers) :gpg)
              (t nil))))))
