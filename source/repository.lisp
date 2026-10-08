;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; A repository: its git directory, the packs it has (re-read when the
;; pack directory changes), a cache of small objects, and a lock for
;; ref updates.  Everything above this file asks a repository; nothing
;; below it knows one.
;;

(defstruct (repository (:constructor %make-repository))
  (path nil)                            ; the git directory, a directory pathname
  (lock nil)
  (packs nil)                           ; list of pack-index
  (packs-stamp nil)                     ; the pack directory's write date when read
  (cache (make-hash-table :test #'equal))
  (cache-lock nil))

(defparameter *object-cache-limit* 4000
  "How many objects a repository keeps in memory; the cache is emptied
when it is full.")

(defparameter *object-cache-size-limit* 65536
  "Objects larger than this are not cached.")

(defvar *repositories* (make-hash-table :test #'equal)
  "Path -> repository: one structure per git directory in the image, so
that its lock is one lock.")

(defvar *repositories-lock* (bt:make-lock "ingot repositories"))

(defun gitdir-of (path)
  "The git directory of PATH: itself when bare, its .git when a work
tree, nil when neither."
  (let ((dir (uiop:ensure-directory-pathname path)))
    (cond ((probe-file (merge-pathnames "HEAD" dir))
           (and (uiop:directory-exists-p (merge-pathnames "objects/" dir)) dir))
          ((uiop:directory-exists-p (merge-pathnames ".git/" dir))
           (merge-pathnames ".git/" dir))
          (t nil))))

(defun repository-exists? (path)
  (and (gitdir-of path) t))

(defun open-repository (path)
  "The repository at PATH (bare, or a work tree), one structure per path."
  (let ((gitdir (gitdir-of path)))
    (unless gitdir (not-found "No git repository at ~a." path))
    (let ((key (namestring gitdir)))
      (bt:with-lock-held (*repositories-lock*)
        (or (gethash key *repositories*)
            (setf (gethash key *repositories*)
                  (%make-repository :path gitdir
                                    :lock (bt:make-lock (format nil "git ~a" key))
                                    :cache-lock (bt:make-lock (format nil "git cache ~a" key)))))))))

(defun init-repository (path &key (default-branch "master") (bare t) description)
  "Make an empty bare repository at PATH (a directory that need not
exist) and answer it."
  (let ((dir (uiop:ensure-directory-pathname path)))
    (when (gitdir-of dir) (git-error "~a is a repository already." path))
    (dolist (sub '("objects/info/" "objects/pack/" "refs/heads/" "refs/tags/" "hooks/" "info/"))
      (ensure-directories-exist (merge-pathnames sub dir)))
    (write-file-octets (merge-pathnames "HEAD" dir)
                       (string-to-octets (format nil "ref: refs/heads/~a~%" default-branch)))
    (write-file-octets (merge-pathnames "config" dir)
                       (string-to-octets (format nil "[core]~%	repositoryformatversion = 0~%	filemode = true~%	bare = ~:[false~;true~]~%" bare)))
    (write-file-octets (merge-pathnames "description" dir)
                       (string-to-octets (format nil "~a~%" (or description "Unnamed repository"))))
    (open-repository dir)))

(defmacro with-repository-lock ((repository) &body body)
  `(bt:with-lock-held ((repository-lock ,repository)) ,@body))

;;; Packs

(defun pack-directory (repository)
  (merge-pathnames "objects/pack/" (repository-path repository)))

(defun repository-pack-indexes (repository)
  "The pack indexes of REPOSITORY, re-read when the pack directory has
changed since."
  (let* ((dir (pack-directory repository))
         (stamp (and (uiop:directory-exists-p dir) (file-write-date dir))))
    (unless (and (repository-packs repository) (eql stamp (repository-packs-stamp repository))
                 (every (lambda (index) (probe-file (pack-index-path index))) (repository-packs repository)))
      (let ((indexes nil))
        (when stamp
          (dolist (idx (uiop:directory-files dir "*.idx"))
            (let ((pack (make-pathname :type "pack" :defaults idx)))
              (when (probe-file pack)
                (handler-case (push (read-pack-index idx pack) indexes)
                  (error (e) (warn "ingot: ~a: ~a" idx e)))))))
        (setf (repository-packs repository) indexes
              (repository-packs-stamp repository) stamp)))
    (repository-packs repository)))

(defun find-packed-object (repository oid)
  (dolist (index (repository-pack-indexes repository))
    (let ((i (index-find index oid)))
      (when i
        (multiple-value-bind (type data)
            (read-pack-entry index (index-offset-at index i)
                             (lambda (base-oid) (find-object repository base-oid)))
          (return (make-git-object type data oid)))))))

;;; Objects

(defun cache-get (repository oid)
  (bt:with-lock-held ((repository-cache-lock repository))
    (gethash oid (repository-cache repository))))

(defun cache-put (repository object)
  (when (<= (object-size object) *object-cache-size-limit*)
    (bt:with-lock-held ((repository-cache-lock repository))
      (let ((cache (repository-cache repository)))
        (when (>= (hash-table-count cache) *object-cache-limit*) (clrhash cache))
        (setf (gethash (object-oid object) cache) object))))
  object)

(defun find-object (repository oid)
  "The git-object OID in REPOSITORY, or nil."
  (or (cache-get repository oid)
      (let ((object (or (read-loose-object (repository-path repository) oid)
                        (find-packed-object repository oid))))
        (when object (cache-put repository object))
        object)))

(defun object-exists? (repository oid)
  (or (cache-get repository oid)
      (loose-object-exists? (repository-path repository) oid)
      (some (lambda (index) (index-find index oid)) (repository-pack-indexes repository))))

(defun read-object (repository oid &optional type)
  "The git-object OID; signals not-found when it is missing, git-error
when it is not of TYPE."
  (let ((object (find-object repository oid)))
    (unless object (not-found "No object ~a." oid))
    (when (and type (not (eq type (object-type object))))
      (git-error "~a is a ~(~a~), not a ~(~a~)." oid (object-type object) type))
    object))

(defun read-blob (repository oid)
  (object-data (read-object repository oid :blob)))

(defun read-tree (repository oid)
  "The entries of tree OID."
  (parse-tree (object-data (read-object repository oid :tree))))

(defun read-commit (repository oid)
  (let ((object (read-object repository oid)))
    (case (object-type object)
      (:commit (parse-commit (object-data object) oid))
      (:tag (read-commit repository (tag-object (parse-tag (object-data object) oid))))
      (t (git-error "~a is a ~(~a~), not a commit." oid (object-type object))))))

(defun read-tag (repository oid)
  (parse-tag (object-data (read-object repository oid :tag)) oid))

(defun peel-to-commit (repository oid)
  "The id of the commit OID is or points at through tags, or nil."
  (let ((object (find-object repository oid)))
    (and object
         (case (object-type object)
           (:commit oid)
           (:tag (peel-to-commit repository (tag-object (parse-tag (object-data object) oid))))
           (t nil)))))

(defun write-object (repository type data)
  "Write DATA as an object of TYPE; answers the id."
  (write-loose-object (repository-path repository) type data))

(defun write-blob (repository data)
  (write-object repository :blob (if (stringp data) (string-to-octets data) data)))

(defun write-tree (repository entries)
  (write-object repository :tree (serialize-tree entries)))

(defun write-commit (repository commit)
  (write-object repository :commit (serialize-commit commit)))

;;; Refs through the repository

(defun refs (repository &optional prefix)
  "An alist (name . oid) of the refs, those under PREFIX when given."
  (let ((all (all-refs (repository-path repository))))
    (if prefix
        (remove-if-not (lambda (ref) (and (> (length (car ref)) (length prefix))
                                          (string= prefix (car ref) :end2 (length prefix))))
                       all)
        all)))

(defun branches (repository)
  "An alist (short-name . oid) of the branches."
  (loop for (name . oid) in (refs repository "refs/heads/")
        collect (cons (subseq name 11) oid)))

(defun tags (repository)
  "An alist (short-name . oid) of the tags (the tag object's id for an
annotated tag)."
  (loop for (name . oid) in (refs repository "refs/tags/")
        collect (cons (subseq name 10) oid)))

(defun head-ref (repository)
  "The ref HEAD names, or nil when detached."
  (multiple-value-bind (target symbolic?) (head-target (repository-path repository))
    (and symbolic? target)))

(defun default-branch (repository)
  "The short name of the branch HEAD names."
  (let ((ref (head-ref repository)))
    (if (and ref (> (length ref) 11) (string= "refs/heads/" ref :end2 11))
        (subseq ref 11)
        "master")))

(defun set-default-branch! (repository name)
  (set-head! (repository-path repository) (format nil "refs/heads/~a" name)))

(defun head-oid (repository)
  (ref-value (repository-path repository) "HEAD"))

(defun ref-value* (repository name)
  (ref-value (repository-path repository) name))

(defun update-ref! (repository name new &key (old nil old?))
  "Point NAME at NEW (an id).  With OLD given, the update happens only
when the ref is OLD now (nil for 'does not exist'); else ref-conflict.
Under the repository's lock."
  (unless (valid-ref-name? name) (git-error "~s is not a ref name." name))
  (with-repository-lock (repository)
    (let ((current (ref-value (repository-path repository) name)))
      (when (and old? (not (equal current old)))
        (error 'ref-conflict :text (format nil "~a is ~a now, not ~a." name (or current "absent") (or old "absent"))))
      (write-ref! (repository-path repository) name new)
      new)))

(defun delete-ref! (repository name &key (old nil old?))
  (with-repository-lock (repository)
    (let ((current (ref-value (repository-path repository) name)))
      (when (and old? (not (equal current old)))
        (error 'ref-conflict :text (format nil "~a is ~a now, not ~a." name (or current "absent") (or old "absent"))))
      (remove-ref! (repository-path repository) name)
      t)))

;;; Resolving what a person typed

(defun expand-short-oid (repository prefix)
  "The one id that begins with PREFIX, or nil when none or several do."
  (when (and (stringp prefix) (>= (length prefix) 4) (<= (length prefix) 40)
             (every (lambda (c) (find c *hex-digits*)) prefix))
    (let ((found (remove-duplicates
                  (append (loose-oids-with-prefix (repository-path repository) prefix)
                          (loop for index in (repository-pack-indexes repository)
                                append (index-oids-with-prefix index prefix)))
                  :test #'string=)))
      (and (= (length found) 1) (first found)))))

(defun resolve (repository revision)
  "The id REVISION names: an id, a short id, HEAD, a branch, a tag (its
own id, not peeled), a full ref name.  Nil when nothing does."
  (let ((gitdir (repository-path repository)))
    (cond ((or (null revision) (zerop (length revision))) (head-oid repository))
          ((oid? revision) revision)
          ((string= revision "HEAD") (head-oid repository))
          ((ref-value gitdir (format nil "refs/heads/~a" revision)))
          ((ref-value gitdir (format nil "refs/tags/~a" revision)))
          ((and (> (length revision) 5) (string= "refs/" revision :end2 5) (ref-value gitdir revision)))
          ((expand-short-oid repository revision))
          (t nil))))

(defun resolve-commit (repository revision)
  "The id of the commit REVISION names, peeled through tags, or nil."
  (let ((oid (resolve repository revision)))
    (and oid (peel-to-commit repository oid))))

;;; Trees

(defun split-path (path)
  (remove "" (uiop:split-string (or path "") :separator "/") :test #'string=))

(defun tree-entry-at (repository tree-oid path)
  "The tree-entry at PATH (\"a/b/c\") under the tree TREE-OID, or nil.
An empty PATH answers an entry standing for the root tree."
  (let ((parts (split-path path)))
    (if (null parts)
        (make-tree-entry :mode "40000" :name "" :oid tree-oid)
        (let ((entry nil) (current tree-oid))
          (dolist (part parts entry)
            (unless current (return nil))
            (setq entry (find part (read-tree repository current) :key #'tree-entry-name :test #'string=))
            (unless entry (return nil))
            (setq current (and (tree-entry-tree? entry) (tree-entry-oid entry))))))))

(defun tree-at-path (repository commit-oid path)
  "The entries of the tree at PATH in the commit COMMIT-OID, or nil when
PATH is not a directory there."
  (let* ((commit (read-commit repository commit-oid))
         (entry (tree-entry-at repository (commit-tree commit) path)))
    (and entry (tree-entry-tree? entry) (read-tree repository (tree-entry-oid entry)))))

(defun blob-at (repository commit-oid path)
  "Two values: the octets of the file at PATH in the commit, and its
tree-entry; nil when it is not a file there."
  (let* ((commit (read-commit repository commit-oid))
         (entry (tree-entry-at repository (commit-tree commit) path)))
    (and entry (not (tree-entry-tree? entry)) (not (eq (tree-entry-kind entry) :submodule))
         (values (read-blob repository (tree-entry-oid entry)) entry))))

(defun walk-tree (repository tree-oid function &key (prefix ""))
  "Call FUNCTION with the path and the tree-entry of everything under
tree TREE-OID, depth first, directories before their contents."
  (dolist (entry (read-tree repository tree-oid))
    (let ((path (if (string= prefix "") (tree-entry-name entry)
                    (concatenate 'string prefix "/" (tree-entry-name entry)))))
      (funcall function path entry)
      (when (tree-entry-tree? entry)
        (walk-tree repository (tree-entry-oid entry) function :prefix path)))))

(defun tree-paths (repository tree-oid &key (files-only t))
  "An alist of (path . tree-entry) of everything under TREE-OID."
  (let ((found nil))
    (walk-tree repository tree-oid
               (lambda (path entry)
                 (unless (and files-only (tree-entry-tree? entry))
                   (push (cons path entry) found))))
    (nreverse found)))

;;; History

(defun log-commits (repository start &key (limit 50) (skip 0) path since until)
  "The commits reachable from START (an id), newest committer date first,
LIMIT of them after SKIP; with PATH, only those that changed it against
their first parent.  Answers a list of commit structures."
  (let ((seen (make-hash-table :test #'equal))
        (frontier (list (read-commit repository start)))
        (out nil) (skipped 0))
    (setf (gethash (commit-oid (first frontier)) seen) t)
    (loop while (and frontier (< (length out) limit))
          do (setq frontier (sort frontier #'> :key #'commit-committer-time))
             (let* ((commit (pop frontier))
                    (time (commit-committer-time commit)))
               (dolist (parent (commit-parents commit))
                 (unless (gethash parent seen)
                   (setf (gethash parent seen) t)
                   (let ((object (ignore-errors (read-commit repository parent))))
                     (when object (push object frontier)))))
               (when (and (or (null since) (>= time since))
                          (or (null until) (<= time until))
                          (or (null path) (commit-changes-path? repository commit path)))
                 (if (< skipped skip)
                     (incf skipped)
                     (push commit out)))))
    (nreverse out)))

(defun commit-changes-path? (repository commit path)
  "True when PATH's entry in COMMIT differs from the one in its first
parent (or the commit has no parent and PATH is there)."
  (let* ((entry (tree-entry-at repository (commit-tree commit) path))
         (parent (first (commit-parents commit)))
         (parent-entry (and parent
                            (let ((pc (ignore-errors (read-commit repository parent))))
                              (and pc (tree-entry-at repository (commit-tree pc) path))))))
    (not (equal (and entry (tree-entry-oid entry)) (and parent-entry (tree-entry-oid parent-entry))))))

(defun count-commits (repository start &key (limit 100000))
  (let ((seen (make-hash-table :test #'equal))
        (stack (list start)) (count 0))
    (loop while (and stack (< count limit))
          do (let ((oid (pop stack)))
               (unless (gethash oid seen)
                 (setf (gethash oid seen) t)
                 (let ((commit (ignore-errors (read-commit repository oid))))
                   (when commit
                     (incf count)
                     (dolist (parent (commit-parents commit)) (push parent stack)))))))
    count))

(defun ancestors (repository oid &key (limit most-positive-fixnum))
  "A hash table of the ids of OID and every commit reachable from it."
  (let ((seen (make-hash-table :test #'equal))
        (stack (list oid)))
    (loop while (and stack (< (hash-table-count seen) limit))
          do (let ((oid (pop stack)))
               (unless (gethash oid seen)
                 (setf (gethash oid seen) t)
                 (let ((commit (ignore-errors (read-commit repository oid))))
                   (when commit (dolist (parent (commit-parents commit)) (push parent stack)))))))
    seen))

(defun ancestor? (repository ancestor descendant)
  "True when ANCESTOR is DESCENDANT or reachable from it."
  (and (gethash ancestor (ancestors repository descendant)) t))

(defun merge-base (repository a b)
  "The id of a best common ancestor of commits A and B, or nil: the
first commit reachable from B, newest first, that A also reaches."
  (let ((from-a (ancestors repository a))
        (seen (make-hash-table :test #'equal))
        (frontier (list (read-commit repository b))))
    (loop while frontier
          do (setq frontier (sort frontier #'> :key #'commit-committer-time))
             (let ((commit (pop frontier)))
               (when (gethash (commit-oid commit) from-a)
                 (return (commit-oid commit)))
               (dolist (parent (commit-parents commit))
                 (unless (gethash parent seen)
                   (setf (gethash parent seen) t)
                   (let ((object (ignore-errors (read-commit repository parent))))
                     (when object (push object frontier)))))))))

(defun reachable-objects (repository wants haves)
  "The ids of the objects reachable from the commits WANTS that are not
reachable from the commits HAVES: what a fetch has to send.  Answers a
list of ids, commits first."
  (let ((have-commits (make-hash-table :test #'equal))
        (known (make-hash-table :test #'equal))   ; trees and blobs the other side has
        (send (make-hash-table :test #'equal))
        (commits nil) (others nil))
    (dolist (have haves)
      (when (object-exists? repository have)
        (maphash (lambda (k v) (declare (ignore v)) (setf (gethash k have-commits) t))
                 (ancestors repository have))))
    (labels ((mark-known-tree (tree-oid)
               (unless (gethash tree-oid known)
                 (setf (gethash tree-oid known) t)
                 (dolist (entry (ignore-errors (read-tree repository tree-oid)))
                   (if (tree-entry-tree? entry)
                       (mark-known-tree (tree-entry-oid entry))
                       (setf (gethash (tree-entry-oid entry) known) t)))))
             (send-tree (tree-oid)
               (unless (or (gethash tree-oid known) (gethash tree-oid send))
                 (setf (gethash tree-oid send) t)
                 (push tree-oid others)
                 (dolist (entry (read-tree repository tree-oid))
                   (case (tree-entry-kind entry)
                     (:tree (send-tree (tree-entry-oid entry)))
                     (:submodule nil)
                     (t (let ((oid (tree-entry-oid entry)))
                          (unless (or (gethash oid known) (gethash oid send))
                            (setf (gethash oid send) t)
                            (push oid others)))))))))
      ;; the commits to send, and the boundary (parents the other side has)
      (let ((stack (copy-list wants)) (boundary nil))
        (loop while stack
              do (let ((oid (pop stack)))
                   (unless (or (gethash oid send) (gethash oid have-commits))
                     (let ((object (find-object repository oid)))
                       (when object
                         (setf (gethash oid send) t)
                         (case (object-type object)
                           (:commit
                            (push oid commits)
                            (dolist (parent (commit-parents (parse-commit (object-data object) oid)))
                              (if (gethash parent have-commits) (pushnew parent boundary) (push parent stack))))
                           (:tag (push oid others)
                            (push (tag-object (parse-tag (object-data object) oid)) stack))
                           (t (push oid others))))))))
        ;; what the other side has: the trees of the boundary commits
        (dolist (oid boundary)
          (let ((commit (ignore-errors (read-commit repository oid))))
            (when commit (mark-known-tree (commit-tree commit)))))
        ;; the trees and blobs of the commits sent
        (dolist (oid (reverse commits))
          (let ((commit (read-commit repository oid)))
            (send-tree (commit-tree commit))))))
    (append (reverse commits) (reverse others))))
