;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Making commits without a work tree: a base tree and a list of
;; actions become a new tree, and a commit on a branch.  This is what
;; the web editor, the API's commits endpoint and a merge use.
;;
;; An action is a plist:
;;   (:action :create :path "a/b.lisp" :content <string or octets> [:mode "100755"])
;;   (:action :update :path ... :content ...)
;;   (:action :delete :path ...)
;;   (:action :move :previous-path ... :path ... [:content ...])
;;   (:action :chmod :path ... :mode "100755")
;;

(defun actions-to-changes (repository actions)
  "An alist of (path . tree-entry-or-nil): what each path becomes (nil
removes it), with a :move expanded into a delete and a create."
  (let ((changes nil))
    (dolist (action actions (nreverse changes))
      (let ((kind (getf action :action))
            (path (getf action :path)))
        (flet ((content-oid ()
                 (let ((content (getf action :content)))
                   (write-blob repository (cond ((null content) "")
                                                ((stringp content) content)
                                                (t (as-octets content)))))))
          (ecase kind
            ((:create :update)
             (push (cons path (make-tree-entry :mode (or (getf action :mode) "100644")
                                               :name (last-path-part path)
                                               :oid (content-oid)))
                   changes))
            (:delete (push (cons path nil) changes))
            (:move
             (let* ((from (getf action :previous-path))
                    (old (tree-entry-at repository (getf action :base-tree) from)))
               (unless old (not-found "No ~a to move." from))
               (push (cons from nil) changes)
               (push (cons path (make-tree-entry :mode (or (getf action :mode) (tree-entry-mode old))
                                                 :name (last-path-part path)
                                                 :oid (if (getf action :content) (content-oid) (tree-entry-oid old))))
                     changes)))
            (:chmod
             (let ((old (tree-entry-at repository (getf action :base-tree) path)))
               (unless old (not-found "No ~a to chmod." path))
               (push (cons path (make-tree-entry :mode (getf action :mode) :name (last-path-part path)
                                                 :oid (tree-entry-oid old)))
                     changes)))))))))

(defun last-path-part (path)
  (let ((parts (split-path path)))
    (or (car (last parts)) "")))

(defun apply-changes-to-tree (repository tree-oid changes)
  "The id of the tree TREE-OID (nil for an empty one) with CHANGES
applied: an alist of (path . entry-or-nil) as actions-to-changes makes.
Trees that become empty are dropped."
  ;; group the changes by their first path component
  (let ((here nil) (below (make-hash-table :test #'equal)))
    (loop for (path . entry) in changes
          do (let* ((parts (split-path path)))
               (cond ((null parts))
                     ((null (rest parts)) (push (cons (first parts) entry) here))
                     (t (push (cons (format nil "~{~a~^/~}" (rest parts)) entry)
                              (gethash (first parts) below))))))
    (let* ((entries (if tree-oid (read-tree repository tree-oid) nil))
           (result (remove-if (lambda (entry) (or (assoc (tree-entry-name entry) here :test #'string=)
                                                  (gethash (tree-entry-name entry) below)))
                              entries)))
      (loop for (nil . entry) in here
            when entry do (push entry result))
      (maphash (lambda (name sub-changes)
                 (let* ((old (find name entries :key #'tree-entry-name :test #'string=))
                        (old-tree (and old (tree-entry-tree? old) (tree-entry-oid old)))
                        (new-tree (apply-changes-to-tree repository old-tree (reverse sub-changes))))
                   (when new-tree
                     (push (make-tree-entry :mode "40000" :name name :oid new-tree) result))))
               below)
      (if (null result)
          nil
          (write-tree repository result)))))

(defun apply-actions (repository tree-oid actions)
  "The id of the tree TREE-OID with ACTIONS applied (see the top of this
file); nil for a tree left empty."
  (let ((actions (mapcar (lambda (action) (append action (list :base-tree tree-oid))) actions)))
    (apply-changes-to-tree repository tree-oid (actions-to-changes repository actions))))

(defun empty-tree-oid (repository)
  (write-tree repository nil))

(defun make-commit! (repository &key branch start-branch actions message author committer
                                    (expected-old nil expected-old?) parents tree)
  "Commit on BRANCH (a short name): ACTIONS applied to the branch's tree
(or START-BRANCH's, when BRANCH does not exist yet), with MESSAGE,
AUTHOR and COMMITTER (identity structures; the committer defaults to
the author).  TREE and PARENTS, when given, are used instead of the
actions and the branch's tip (a merge).  Answers the new commit's id.
The branch is updated only if it is still where it was read."
  (let* ((ref (format nil "refs/heads/~a" branch))
         (tip (ref-value* repository ref))
         (base (or tip (and start-branch (ref-value* repository (format nil "refs/heads/~a" start-branch)))))
         (base-commit (and base (read-commit repository base)))
         (new-tree (or tree
                       (apply-actions repository (and base-commit (commit-tree base-commit)) actions)
                       (empty-tree-oid repository)))
         (committer (or committer author))
         (commit (make-commit :tree new-tree
                              :parents (or parents (and base (list base)))
                              :author author :committer committer
                              :message (if (and (plusp (length message))
                                                (char/= (char message (1- (length message))) #\Newline))
                                           (format nil "~a~%" message)
                                           message))))
    (when (and base-commit (null tree) (equal new-tree (commit-tree base-commit)))
      (git-error "Nothing changed."))
    (let ((oid (write-commit repository commit)))
      (if expected-old?
          (update-ref! repository ref oid :old expected-old)
          (update-ref! repository ref oid :old tip))
      oid)))
