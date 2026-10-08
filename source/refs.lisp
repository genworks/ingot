;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Refs: the loose ones under refs/, packed-refs, and HEAD.  A ref's
;; value is a full id; HEAD is a symbolic ref ("ref: refs/heads/x") or,
;; detached, an id.  Updates write a loose ref through a temporary file
;; and a rename; a delete removes the loose file and the packed line.
;;

(defun ref-file (gitdir name)
  (merge-pathnames name gitdir))

(defun read-text-file-trimmed (file)
  (string-trim '(#\Space #\Newline #\Return #\Tab) (octets-to-string (read-file-octets file))))

(defun read-packed-refs (gitdir)
  "Two values: an alist of (name . oid) from packed-refs, and an alist of
(name . peeled-oid) for the annotated tags it peels."
  (let ((file (probe-file (ref-file gitdir "packed-refs")))
        (refs nil) (peeled nil) (last nil))
    (when file
      (dolist (line (uiop:split-string (octets-to-string (read-file-octets file)) :separator '(#\Newline)))
        (let ((line (string-right-trim '(#\Return) line)))
          (cond ((zerop (length line)))
                ((char= (char line 0) #\#))
                ((char= (char line 0) #\^)
                 (when last (push (cons (car last) (subseq line 1 41)) peeled)))
                ((> (length line) 41)
                 (setq last (cons (subseq line 41) (subseq line 0 40)))
                 (push last refs))))))
    (values (nreverse refs) peeled)))

(defun loose-ref-value (gitdir name)
  "The id in the loose ref NAME, nil when there is none; a symbolic ref
answers (:ref . target)."
  ;; probe-file signals on some Lisps when a component of the path is
  ;; a file (refs/heads/master/src, asked while resolving a tree path)
  (let ((file (ignore-errors (probe-file (ref-file gitdir name)))))
    (when (and file (not (uiop:directory-pathname-p file)))
      (let ((text (read-text-file-trimmed file)))
        (cond ((and (> (length text) 5) (string= "ref: " text :end2 5)) (cons :ref (subseq text 5)))
              ((oid? text) text)
              (t nil))))))

(defun ref-value (gitdir name &optional (depth 0))
  "The id REF NAME points at (symbolic refs followed), or nil."
  (when (> depth 10) (git-error "~a: a symbolic ref loop." name))
  (let ((value (loose-ref-value gitdir name)))
    (cond ((consp value) (ref-value gitdir (cdr value) (1+ depth)))
          (value value)
          (t (cdr (assoc name (read-packed-refs gitdir) :test #'string=))))))

(defun loose-refs (gitdir &optional (under "refs/"))
  "An alist of (name . oid) for every loose ref under UNDER."
  (let ((root (merge-pathnames under gitdir))
        (found nil))
    (when (uiop:directory-exists-p root)
      (uiop:collect-sub*directories
       root t t
       (lambda (dir)
         (dolist (file (uiop:directory-files dir))
           (let* ((name (enough-namestring file gitdir))
                  (name (substitute #\/ #\\ name)))
             (unless (or (search ".lock" name) (search ".tmp" name))
               (let ((value (loose-ref-value gitdir name)))
                 (when (stringp value) (push (cons name value) found)))))))))
    found))

(defun all-refs (gitdir)
  "An alist of (name . oid) of every ref, loose ones shadowing packed
ones, sorted by name."
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name . oid) in (read-packed-refs gitdir) do (setf (gethash name table) oid))
    (loop for (name . oid) in (loose-refs gitdir) do (setf (gethash name table) oid))
    (let ((refs nil))
      (maphash (lambda (name oid) (push (cons name oid) refs)) table)
      (sort refs #'string< :key #'car))))

(defun head-target (gitdir)
  "Two values: what HEAD names (\"refs/heads/master\", or an id when
detached), and whether it is symbolic."
  (let ((value (loose-ref-value gitdir "HEAD")))
    (cond ((consp value) (values (cdr value) t))
          (value (values value nil))
          (t (values "refs/heads/master" t)))))

(defun write-ref-file (gitdir name text)
  (write-file-octets (ref-file gitdir name) (string-to-octets (format nil "~a~%" text))))

(defun set-head! (gitdir target)
  "Point HEAD at TARGET, a ref name (\"refs/heads/main\")."
  (write-ref-file gitdir "HEAD" (format nil "ref: ~a" target)))

(defun write-ref! (gitdir name oid)
  (write-ref-file gitdir name oid))

(defun remove-ref! (gitdir name)
  "Remove the ref NAME, loose and packed."
  (let ((file (probe-file (ref-file gitdir name))))
    (when file (delete-file file)))
  (multiple-value-bind (refs peeled) (read-packed-refs gitdir)
    (when (assoc name refs :test #'string=)
      (write-file-octets
       (ref-file gitdir "packed-refs")
       (string-to-octets
        (with-output-to-string (out)
          (format out "# pack-refs with: peeled fully-peeled sorted ~%")
          (loop for (ref . oid) in refs
                unless (string= ref name)
                  do (format out "~a ~a~%" oid ref)
                     (let ((p (cdr (assoc ref peeled :test #'string=))))
                       (when p (format out "^~a~%" p))))))))))

(defun valid-ref-name? (name)
  "git check-ref-format, the parts that matter: no '..', no control
characters, no '~^:?*[\\', no component starting with '.', no trailing
'/', '.lock' or '.'."
  (and (stringp name) (plusp (length name))
       (not (search ".." name)) (not (search "@{" name)) (not (search "//" name))
       (every (lambda (c) (and (> (char-code c) 31) (not (find c "~^:?*[\\ ")))) name)
       (char/= (char name (1- (length name))) #\/)
       (char/= (char name (1- (length name))) #\.)
       (not (and (> (length name) 5) (string= ".lock" name :start2 (- (length name) 5))))
       (notany (lambda (part) (or (zerop (length part)) (char= (char part 0) #\.)))
               (uiop:split-string name :separator "/"))))
