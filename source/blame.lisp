;; Copyright © 2026 Genworks International
;;
;; SPDX-License-Identifier: MIT
;; Released under the MIT License; see the LICENSE file.

(in-package :ingot)

;;
;; Blame: for each line of a file at a commit, the commit that last
;; changed it.  The history is walked newest first; at each commit the
;; file is diffed against each parent's copy in turn, the lines a parent
;; already has pass to that parent, and what no parent has was written
;; here.  Renames are not followed.
;;

(defun line-parents (old-lines new-lines)
  "A vector, one slot per line of NEW-LINES: the index of the same line
in OLD-LINES, or nil where the line is new."
  (let ((map (make-array (length new-lines) :initial-element nil))
        (i 0) (j 0))
    (dolist (edit (diff-lines old-lines new-lines) map)
      (ecase (first edit)
        (:equal (setf (aref map j) i) (incf i) (incf j))
        (:delete (incf i))
        (:insert (incf j))))))

(defun file-lines-at (repository commit path)
  "The lines of PATH in the commit structure COMMIT, each keeping its
newline (a last line without one differs from the same line with one,
as it does for git), or nil when it is not a text file there."
  (let ((entry (tree-entry-at repository (commit-tree commit) path)))
    (and entry (member (tree-entry-kind entry) '(:blob :executable :symlink))
         (let ((octets (read-blob repository (tree-entry-oid entry))))
           (and (not (binary-octets? octets))
                (lines-keeping-ends (octets-to-string octets)))))))

(defun blame (repository commit-oid path &key (limit 5000))
  "The lines of PATH at COMMIT-OID, each with the id of the commit that
last changed it: a list of (oid . line), in file order.  Nil when PATH is
not a text file there.  At most LIMIT commits are walked; lines older
than that are given to the oldest commit reached."
  (let* ((start (read-commit repository commit-oid))
         (lines (file-lines-at repository start path)))
    (when lines
      (let* ((n (length lines))
             (owner (make-array n :initial-element nil))   ; final line index -> oid
             ;; the work: commit oid -> (commit . pending), pending a hash
             ;; of final-line index -> that line's index in the commit's copy
             (work (make-hash-table :test #'equal))
             (seen 0))
        (let ((pending (make-hash-table)))
          (dotimes (i n) (setf (gethash i pending) i))
          (setf (gethash (commit-oid start) work) (cons start pending)))
        (loop while (plusp (hash-table-count work))
              do (let* ((newest (let ((best nil))
                                  (maphash (lambda (oid item)
                                             (declare (ignore oid))
                                             (when (or (null best) (> (commit-committer-time (car item)) (commit-committer-time (car best))))
                                               (setq best item)))
                                           work)
                                  best))
                        (commit (car newest))
                        (pending (cdr newest))
                        (here (or (file-lines-at repository commit path) nil)))
                   (remhash (commit-oid commit) work)
                   (incf seen)
                   (if (or (null here) (>= seen limit))
                       ;; the file appears here (or the walk is over): these lines are this commit's
                       (maphash (lambda (final at) (declare (ignore at)) (setf (aref owner final) (commit-oid commit))) pending)
                       (progn
                         (dolist (parent-oid (commit-parents commit))
                           (when (plusp (hash-table-count pending))
                             (let* ((parent (ignore-errors (read-commit repository parent-oid)))
                                    (there (and parent (file-lines-at repository parent path))))
                               (when there
                                 (let ((map (line-parents there here))
                                       (passed (make-hash-table)))
                                   (maphash (lambda (final at)
                                              (let ((in-parent (aref map at)))
                                                (when in-parent (setf (gethash final passed) in-parent))))
                                            pending)
                                   (when (plusp (hash-table-count passed))
                                     (maphash (lambda (final at) (declare (ignore at)) (remhash final pending)) passed)
                                     (let ((item (gethash parent-oid work)))
                                       (if item
                                           (maphash (lambda (final at) (setf (gethash final (cdr item)) at)) passed)
                                           (setf (gethash parent-oid work) (cons parent passed))))))))))
                         ;; what no parent has was written here
                         (maphash (lambda (final at) (declare (ignore at)) (setf (aref owner final) (commit-oid commit))) pending)))))
        (loop for line in lines
              for i from 0
              collect (cons (or (aref owner i) (commit-oid start))
                            (if (and (plusp (length line)) (char= (char line (1- (length line))) #\Newline))
                                (subseq line 0 (1- (length line)))
                                line)))))))
