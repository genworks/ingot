;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; The Lisp half of differential.sh: Ingot answers the questions git
;;; answered into /facts, and the answers are compared.  Runs in a Lisp
;;; container; /repo is the repository, /anvil this checkout.
;;;

(in-package :cl-user)

(when (fboundp (find-symbol "LOAD-QUICKLISP" :gdl-user)) (funcall (find-symbol "LOAD-QUICKLISP" :gdl-user)))
(asdf:load-asd "/anvil/ingot/ingot.asd")
(funcall (read-from-string "ql:quickload") :ingot :silent t)

(defvar *failures* nil)
(defvar *checks* 0)

(defun fact (name)
  (string-trim '(#\Newline #\Space) (uiop:read-file-string (format nil "/facts/~a" name))))

(defun fact-lines (name)
  (remove "" (uiop:split-string (uiop:read-file-string (format nil "/facts/~a" name)) :separator '(#\Newline)) :test #'string=))

(defun check (name ok? &optional detail)
  (incf *checks*)
  (format t "~&~a ~a~@[  ~a~]~%" (if ok? "ok  " "FAIL") name (and (not ok?) detail))
  (unless ok? (push name *failures*)))

(let* ((repo (ingot:open-repository "/repo/"))
       (head (ingot:head-oid repo)))
  (check "HEAD" (equal head (fact "head")) (list head (fact "head")))
  (let ((branch (fact "branch")))
    (when (plusp (length branch))
      (check "the default branch" (equal (ingot:default-branch repo) branch))))
  (check "every ref"
         (let ((theirs (mapcar (lambda (line) (let ((space (position #\Space line))) (cons (subseq line 0 space) (subseq line (1+ space)))))
                               (fact-lines "refs")))
               (ours (ingot:refs repo)))
           (and (= (length theirs) (length ours))
                (every (lambda (pair) (equal (cdr (assoc (car pair) ours :test #'string=)) (cdr pair))) theirs)))
         (format nil "~d refs here, ~d there" (length (ingot:refs repo)) (length (fact-lines "refs"))))
  (check "the log, fifty commits newest first"
         (equal (mapcar #'ingot:commit-oid (ingot:log-commits repo head :limit 50)) (fact-lines "log")))
  (check "the commit count" (= (ingot:count-commits repo head) (parse-integer (fact "count"))))
  (let* ((commit (ingot:read-commit repo head))
         (ours (ingot:tree-paths repo (ingot:commit-tree commit)))
         (theirs (mapcar (lambda (line)
                           (let ((parts (uiop:split-string line :separator " ")))
                             (list (first parts) (second parts) (third parts) (format nil "~{~a~^ ~}" (cdddr parts)))))
                         (fact-lines "tree"))))
    (check "the tree: the same paths, ids and modes"
           (and (= (length ours) (length theirs))
                (every (lambda (entry)
                         (destructuring-bind (mode type oid path) entry
                           (declare (ignore type))
                           (let ((mine (cdr (assoc path ours :test #'string=))))
                             (and mine (string= (ingot:tree-entry-oid mine) oid)
                                  (string= (string-left-trim "0" (ingot:tree-entry-mode mine)) (string-left-trim "0" mode))))))
                       theirs))
           (format nil "~d files here, ~d there" (length ours) (length theirs))))
  (check "blobs: the same sizes, and ids that hash back"
         (every (lambda (oid size)
                  (let ((object (ingot:find-object repo oid)))
                    (and object (= (ingot:object-size object) (parse-integer size))
                         (string= (ingot:hash-object :blob (ingot:object-data object)) oid))))
                (fact-lines "blobs") (fact-lines "blob-sizes")))
  (check "the files the last commit changed"
         (let ((theirs (mapcar (lambda (line) (let ((tab (position #\Tab line))) (cons (subseq line 0 1) (subseq line (1+ tab)))))
                               (fact-lines "changes")))
               (ours (mapcar (lambda (change) (cons (ecase (ingot:change-status change) (:added "A") (:deleted "D") (:modified "M")) (ingot:change-path change)))
                             (ingot:commit-diff repo head))))
           (and (= (length theirs) (length ours))
                (every (lambda (pair) (member pair ours :test #'equal)) theirs)))
         (format nil "~d changes here, ~d there" (length (ingot:commit-diff repo head)) (length (fact-lines "changes"))))
  (when (probe-file "/facts/merge-base-pair")
    (destructuring-bind (first second) (uiop:split-string (fact "merge-base-pair") :separator " ")
      (let ((theirs (fact "merge-base")) (ours (ingot:merge-base repo first second)))
        ;; git picks one best base; any common ancestor both reach that is
        ;; not an ancestor of another such is as good, so compare by
        ;; reachability rather than identity when they differ
        (check "a merge base both sides accept"
               (or (equal ours theirs)
                   (and ours (plusp (length theirs))
                        (ingot:ancestor? repo ours first) (ingot:ancestor? repo ours second)
                        (not (ingot:ancestor? repo ours theirs))))
               (list ours theirs))))))

(format t "~&~%~d check~:p, ~d failure~:p.~%" *checks* (length *failures*))
(uiop:quit (if *failures* 1 0))
