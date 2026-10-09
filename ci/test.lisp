;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; ci/test.lisp: load Ingot on a COLD Lisp image and run its checks.
;;;
;;; Run by the pipeline inside genworks/gendl:devo-ccl or devo-sbcl, an
;;; image with Quicklisp that has never seen this checkout; any Lisp
;;; with ASDF and Quicklisp does.  It loads the system from the
;;; checkout, where a warning in Ingot's own files fails the job, then
;;; runs test/test.lisp, and leaves the Lisp with status 0 or 1.
;;;
;;; Locally, in the image, with INGOT_DIR naming a checkout:
;;;   cd /opt/gendl && INGOT_DIR=/path/to/ingot \
;;;     ./gdl/program/gdl-ccl -n -b --load /path/to/ingot/ci/test.lisp
;;;

(in-package :cl-user)

(unless (find-package :quicklisp)
  (let ((loader (find-symbol "LOAD-QUICKLISP" "GDL-USER")))
    (if (and loader (fboundp loader))
        (funcall loader)
        (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))))

(defparameter *ingot-dir*
  (uiop:ensure-directory-pathname
   (or (uiop:getenv "INGOT_DIR") (uiop:getenv "CI_PROJECT_DIR")
       (error "Set INGOT_DIR or CI_PROJECT_DIR to the ingot checkout."))))

(defvar *problems* nil)

(defun problem (control &rest arguments)
  (let ((text (apply #'format nil control arguments)))
    (push text *problems*)
    (format t "~&FAIL  ~a~%" text)
    nil))

(defun finish ()
  (finish-output)
  (format t "~&~%~d problem~:p in the load and the checks.~%" (length *problems*))
  (dolist (text (reverse *problems*)) (format t "  ~a~%" text))
  (finish-output)
  (uiop:quit (if *problems* 1 0)))

(format t "~&Cold load of ingot from ~a~%Image: ~a ~a~%"
        *ingot-dir* (lisp-implementation-type) (lisp-implementation-version))

;;; The load: the libraries first with their warnings muffled, then
;;; Ingot, where every warning counts.

(handler-case
    (progn
      (uiop:enable-deferred-warnings-check)
      (asdf:load-asd (merge-pathnames "ingot.asd" *ingot-dir*))
      (handler-bind ((warning #'muffle-warning))
        (ql:quickload (mapcar (lambda (d) (if (symbolp d) (string-downcase d) d))
                              (asdf:system-depends-on (asdf:find-system :ingot)))
                      :silent t))
      (let* ((*compile-verbose* nil) (*load-verbose* nil)
             (report (make-string-output-stream))
             (*error-output* (make-broadcast-stream *error-output* report)))
        (handler-bind ((warning (lambda (w)
                                  (problem "loading: ~a: ~a" (type-of w) w)
                                  (muffle-warning w))))
          (asdf:load-system :ingot))
        (dolist (line (uiop:split-string (get-output-stream-string report) :separator '(#\Newline)))
          (let ((text (string-trim "; " line)))
            (when (and (plusp (length text))
                       (or (search "undefined" text :test #'char-equal)
                           (search "undeclared" text :test #'char-equal)
                           (search "unused" text :test #'char-equal)
                           (search "warning" text :test #'char-equal))
                       (not (search "compiler warnings" text :test #'char-equal)))
              (problem "compiling: ~a" text))))))
  (error (e)
    (problem "loading: ERROR ~a: ~a" (type-of e) e)
    (finish)))

(format t "~&ingot loaded~:[~; with warnings~].~%" *problems*)

;;; The checks: test/test.lisp signals when any fail

(handler-case (load (merge-pathnames "test/test.lisp" *ingot-dir*))
  (error (e) (problem "checks: ~a" e)))

(finish)
