;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Merging: a fast-forward when one side already holds the other, else
;; a three-way merge of the trees from their merge base, with diff3 on
;; the text files both sides changed.  A conflict stops the merge and
;; names the paths; nothing is written half way.
;;

(define-condition merge-conflict (git-error)
  ((paths :initarg :paths :reader merge-conflict-paths)))

;;; Text, three ways

(defun matching (base other)
  "A vector, one slot per line of BASE: the index of the line of OTHER
it is matched with, or nil."
  (let ((map (make-array (length base) :initial-element nil))
        (i 0) (j 0))
    (dolist (edit (diff-lines base other) map)
      (ecase (first edit)
        (:equal (setf (aref map i) j) (incf i) (incf j))
        (:delete (incf i))
        (:insert (incf j))))))

(defun merge-text (base ours theirs &key (ours-label "ours") (theirs-label "theirs"))
  "Two values: the merge of OURS and THEIRS from BASE (strings), and
whether it has conflicts (marked in the text the way git marks them)."
  (let* ((b (split-lines base)) (o (split-lines ours)) (th (split-lines theirs))
         (bv (coerce b 'simple-vector)) (ov (coerce o 'simple-vector)) (tv (coerce th 'simple-vector))
         (o-of-b (matching b o))
         (t-of-b (matching b th))
         (n (length bv))
         (out nil) (conflicts? nil)
         ;; the last stable point: base, ours, theirs indices
         (bi -1) (oi -1) (ti -1))
    (flet ((region (b-end o-end t-end)
             ;; what lies strictly between the last stable point and the next
             (let ((b-lines (coerce (subseq bv (1+ bi) b-end) 'list))
                   (o-lines (coerce (subseq ov (1+ oi) o-end) 'list))
                   (t-lines (coerce (subseq tv (1+ ti) t-end) 'list)))
               (cond ((and (null b-lines) (null o-lines) (null t-lines)))
                     ((equal o-lines b-lines) (dolist (l t-lines) (push l out)))
                     ((equal t-lines b-lines) (dolist (l o-lines) (push l out)))
                     ((equal o-lines t-lines) (dolist (l o-lines) (push l out)))
                     (t (setq conflicts? t)
                        (push (format nil "<<<<<<< ~a" ours-label) out)
                        (dolist (l o-lines) (push l out))
                        (push "=======" out)
                        (dolist (l t-lines) (push l out))
                        (push (format nil ">>>>>>> ~a" theirs-label) out))))))
      (loop for i from 0 below n
            do (let ((j (aref o-of-b i)) (k (aref t-of-b i)))
                 (when (and j k (> j oi) (> k ti))
                   (region i j k)
                   (push (svref bv i) out)
                   (setq bi i oi j ti k))))
      (region n (length ov) (length tv)))
    (values (format nil "~{~a~%~}" (nreverse out)) conflicts?)))

;;; Trees, three ways

(defun merge-trees (repository base-tree ours-tree theirs-tree &key (prefix "") (ours-label "ours") (theirs-label "theirs"))
  "Two values: the id of the tree that merges OURS-TREE and THEIRS-TREE
from BASE-TREE (any may be nil for an empty tree), and the list of
conflicting paths (the tree then holds conflict markers in text files
and ours for the rest)."
  (let* ((base (and base-tree (read-tree repository base-tree)))
         (ours (and ours-tree (read-tree repository ours-tree)))
         (theirs (and theirs-tree (read-tree repository theirs-tree)))
         (names (remove-duplicates (mapcar #'tree-entry-name (append base ours theirs)) :test #'string=))
         (result nil) (conflicts nil))
    (flet ((entry-oid (entry) (and entry (tree-entry-oid entry)))
           (path-of (name) (if (string= prefix "") name (concatenate 'string prefix "/" name))))
      (dolist (name names)
        (let* ((b (find name base :key #'tree-entry-name :test #'string=))
               (o (find name ours :key #'tree-entry-name :test #'string=))
               (th (find name theirs :key #'tree-entry-name :test #'string=))
               (bo (entry-oid b)) (oo (entry-oid o)) (to (entry-oid th)))
          (cond
            ((equal oo to) (when o (push o result)))                       ; same on both sides (or both gone)
            ((equal oo bo) (when th (push th result)))                     ; only theirs changed
            ((equal to bo) (when o (push o result)))                       ; only ours changed
            ((and o th (tree-entry-tree? o) (tree-entry-tree? th))
             (multiple-value-bind (merged sub-conflicts)
                 (merge-trees repository (and b (tree-entry-tree? b) bo) oo to
                              :prefix (path-of name) :ours-label ours-label :theirs-label theirs-label)
               (setq conflicts (append sub-conflicts conflicts))
               (when merged (push (make-tree-entry :mode "40000" :name name :oid merged) result))))
            ((and o th (member (tree-entry-kind o) '(:blob :executable)) (member (tree-entry-kind th) '(:blob :executable)))
             (let ((base-data (and b (member (tree-entry-kind b) '(:blob :executable)) (read-blob repository bo)))
                   (ours-data (read-blob repository oo))
                   (theirs-data (read-blob repository to)))
               (if (or (binary-octets? ours-data) (binary-octets? theirs-data) (and base-data (binary-octets? base-data)))
                   (progn (push (path-of name) conflicts) (push o result))
                   (multiple-value-bind (text conflict?)
                       (merge-text (if base-data (octets-to-string base-data) "")
                                   (octets-to-string ours-data) (octets-to-string theirs-data)
                                   :ours-label ours-label :theirs-label theirs-label)
                     (when conflict? (push (path-of name) conflicts))
                     (push (make-tree-entry :mode (if (string= (tree-entry-mode o) (tree-entry-mode th))
                                                      (tree-entry-mode o)
                                                      (if b (tree-entry-mode b) (tree-entry-mode o)))
                                            :name name
                                            :oid (write-blob repository text))
                           result)))))
            (t
             ;; one side deleted or changed kind while the other changed: a conflict, ours kept
             (push (path-of name) conflicts)
             (when o (push o result)))))))
    (values (and result (write-tree repository result)) conflicts)))

;;; Branches

(defun can-merge? (repository source target)
  "What merging commit SOURCE into commit TARGET would be: :already (the
target holds the source), :fast-forward, :merge, or :conflict."
  (cond ((equal source target) :already)
        ((ancestor? repository source target) :already)
        ((ancestor? repository target source) :fast-forward)
        (t (let* ((base (merge-base repository source target))
                  (base-tree (and base (commit-tree (read-commit repository base)))))
             (multiple-value-bind (tree conflicts)
                 (merge-trees repository base-tree
                              (commit-tree (read-commit repository target))
                              (commit-tree (read-commit repository source)))
               (declare (ignore tree))
               (if conflicts :conflict :merge))))))

(defun merge-branches! (repository source-branch target-branch &key author committer message
                                                                    (allow-fast-forward t) ours-label theirs-label)
  "Merge SOURCE-BRANCH into TARGET-BRANCH (short names).  A fast-forward
when ALLOW-FAST-FORWARD and the target holds nothing the source lacks;
else a merge commit by AUTHOR with MESSAGE.  Answers the id of the
target's new tip, or the old one when there was nothing to merge.
Signals merge-conflict with the paths when the texts disagree."
  (let* ((target-ref (format nil "refs/heads/~a" target-branch))
         (source (ref-value* repository (format nil "refs/heads/~a" source-branch)))
         (target (ref-value* repository target-ref)))
    (unless source (not-found "No branch ~a." source-branch))
    (unless target (not-found "No branch ~a." target-branch))
    (cond
      ((or (equal source target) (ancestor? repository source target)) target)
      ((and allow-fast-forward (ancestor? repository target source))
       (update-ref! repository target-ref source :old target))
      (t
       (let* ((base (merge-base repository source target))
              (base-tree (and base (commit-tree (read-commit repository base)))))
         (multiple-value-bind (tree conflicts)
             (merge-trees repository base-tree
                          (commit-tree (read-commit repository target))
                          (commit-tree (read-commit repository source))
                          :ours-label (or ours-label target-branch) :theirs-label (or theirs-label source-branch))
           (when conflicts
             (error 'merge-conflict :paths conflicts
                                    :text (format nil "~a cannot be merged into ~a: conflicts in ~{~a~^, ~}."
                                                  source-branch target-branch conflicts)))
           (let ((commit (make-commit :tree (or tree (empty-tree-oid repository))
                                      :parents (list target source)
                                      :author author :committer (or committer author)
                                      :message (let ((m (or message (format nil "Merge branch '~a' into ~a" source-branch target-branch))))
                                                 (if (char= (char m (1- (length m))) #\Newline) m (format nil "~a~%" m))))))
             (update-ref! repository target-ref (write-commit repository commit) :old target))))))))
