;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Differences: two trees by path, and two texts by line (Myers' O(ND)
;; algorithm, with a ceiling past which the diff degrades to 'everything
;; changed' rather than taking a minute on two unrelated files).
;;

;;; Trees

(defstruct change
  (status nil :type keyword)            ; :added :deleted :modified :renamed(not detected) :type-changed
  (path nil :type string)
  old-oid new-oid old-mode new-mode)

(defun tree-diff (repository old-tree new-tree &key (prefix ""))
  "The changes between tree OLD-TREE and tree NEW-TREE (either may be
nil for an empty tree), as a list of change structures sorted by path."
  (let ((old-entries (and old-tree (read-tree repository old-tree)))
        (new-entries (and new-tree (read-tree repository new-tree)))
        (changes nil))
    (flet ((path-of (name) (if (string= prefix "") name (concatenate 'string prefix "/" name))))
      (dolist (old old-entries)
        (let ((new (find (tree-entry-name old) new-entries :key #'tree-entry-name :test #'string=)))
          (cond
            ((null new)
             (if (tree-entry-tree? old)
                 (setq changes (append (tree-diff repository (tree-entry-oid old) nil
                                                  :prefix (path-of (tree-entry-name old)))
                                       changes))
                 (push (make-change :status :deleted :path (path-of (tree-entry-name old))
                                    :old-oid (tree-entry-oid old) :old-mode (tree-entry-mode old))
                       changes)))
            ((string= (tree-entry-oid old) (tree-entry-oid new))
             (unless (string= (tree-entry-mode old) (tree-entry-mode new))
               (push (make-change :status :modified :path (path-of (tree-entry-name old))
                                  :old-oid (tree-entry-oid old) :new-oid (tree-entry-oid new)
                                  :old-mode (tree-entry-mode old) :new-mode (tree-entry-mode new))
                     changes)))
            ((and (tree-entry-tree? old) (tree-entry-tree? new))
             (setq changes (append (tree-diff repository (tree-entry-oid old) (tree-entry-oid new)
                                              :prefix (path-of (tree-entry-name old)))
                                   changes)))
            ((or (tree-entry-tree? old) (tree-entry-tree? new))
             ;; a file became a directory or the other way round
             (if (tree-entry-tree? old)
                 (setq changes (append (tree-diff repository (tree-entry-oid old) nil
                                                  :prefix (path-of (tree-entry-name old)))
                                       changes))
                 (push (make-change :status :deleted :path (path-of (tree-entry-name old))
                                    :old-oid (tree-entry-oid old) :old-mode (tree-entry-mode old))
                       changes))
             (if (tree-entry-tree? new)
                 (setq changes (append (tree-diff repository nil (tree-entry-oid new)
                                                  :prefix (path-of (tree-entry-name new)))
                                       changes))
                 (push (make-change :status :added :path (path-of (tree-entry-name new))
                                    :new-oid (tree-entry-oid new) :new-mode (tree-entry-mode new))
                       changes)))
            (t
             (push (make-change :status :modified :path (path-of (tree-entry-name old))
                                :old-oid (tree-entry-oid old) :new-oid (tree-entry-oid new)
                                :old-mode (tree-entry-mode old) :new-mode (tree-entry-mode new))
                   changes)))))
      (dolist (new new-entries)
        (unless (find (tree-entry-name new) old-entries :key #'tree-entry-name :test #'string=)
          (if (tree-entry-tree? new)
              (setq changes (append (tree-diff repository nil (tree-entry-oid new)
                                               :prefix (path-of (tree-entry-name new)))
                                    changes))
              (push (make-change :status :added :path (path-of (tree-entry-name new))
                                 :new-oid (tree-entry-oid new) :new-mode (tree-entry-mode new))
                    changes)))))
    (sort changes #'string< :key #'change-path)))

(defun commit-diff (repository commit-oid &optional parent-oid)
  "The changes a commit makes against PARENT-OID (its first parent when
not given; an empty tree for a root commit)."
  (let* ((commit (read-commit repository commit-oid))
         (parent (or parent-oid (first (commit-parents commit))))
         (parent-tree (and parent (commit-tree (read-commit repository parent)))))
    (tree-diff repository parent-tree (commit-tree commit))))

;;; Text

(defun binary-octets? (octets &optional (sample 8000))
  "True when OCTETS look binary: a NUL in the first SAMPLE bytes."
  (and (position 0 octets :end (min sample (length octets))) t))

(defun split-lines (text)
  "The lines of TEXT, each without its newline; a text ending in a
newline has no empty last line."
  (let ((lines nil) (start 0) (n (length text)))
    (loop
      (when (>= start n) (return))
      (let ((eol (or (position #\Newline text :start start) n)))
        (push (subseq text start eol) lines)
        (setq start (1+ eol))))
    (nreverse lines)))

(defparameter *diff-effort-limit* 2000000
  "When the Myers search would visit more than this many points the
diff gives up and reports the texts as wholly replaced.")

(defun diff-lines (old-lines new-lines)
  "An edit script between the two lists of lines: a list of (op line)
with op :equal, :delete or :insert, in order.  Myers' algorithm with
linear space for the trace of the search is not attempted; the trace is
kept whole, which is fine for files of ordinary size."
  (let* ((a (coerce old-lines 'simple-vector))
         (b (coerce new-lines 'simple-vector))
         (n (length a)) (m (length b))
         (max (+ n m)))
    (when (zerop max) (return-from diff-lines nil))
    ;; strip the common prefix and suffix first
    (let ((prefix 0) (suffix 0))
      (loop while (and (< prefix n) (< prefix m) (string= (svref a prefix) (svref b prefix)))
            do (incf prefix))
      (loop while (and (< (+ prefix suffix) n) (< (+ prefix suffix) m)
                       (string= (svref a (- n suffix 1)) (svref b (- m suffix 1))))
            do (incf suffix))
      (let* ((a2 (subseq a prefix (- n suffix)))
             (b2 (subseq b prefix (- m suffix)))
             (middle (myers a2 b2)))
        (append (loop for i below prefix collect (list :equal (svref a i)))
                middle
                (loop for i from (- n suffix) below n collect (list :equal (svref a i))))))))

(defun myers (a b)
  "The edit script between vectors A and B, by Myers."
  (let* ((n (length a)) (m (length b)) (max (+ n m)))
    (cond ((zerop n) (loop for line across b collect (list :insert line)))
          ((zerop m) (loop for line across a collect (list :delete line)))
          ((> (* n m) *diff-effort-limit*)
           (append (loop for line across a collect (list :delete line))
                   (loop for line across b collect (list :insert line))))
          (t
           (let ((v (make-array (+ (* 2 max) 2) :initial-element 0))
                 (trace nil)
                 (offset (1+ max)))
             (flet ((vref (k) (aref v (+ k offset)))
                    (vset (k x) (setf (aref v (+ k offset)) x)))
               (block search
                 (loop for d from 0 to max
                       do (push (copy-seq v) trace)
                          (loop for k from (- d) to d by 2
                                do (let ((x (if (or (= k (- d))
                                                    (and (/= k d) (< (vref (1- k)) (vref (1+ k)))))
                                                (vref (1+ k))
                                                (1+ (vref (1- k))))))
                                     (let ((y (- x k)))
                                       (loop while (and (< x n) (< y m) (string= (svref a x) (svref b y)))
                                             do (incf x) (incf y))
                                       (vset k x)
                                       (when (and (>= x n) (>= y m))
                                         (return-from search)))))))
               ;; walk the trace back
               (let ((x n) (y m) (script nil))
                 (loop for d from (1- (length trace)) downto 0
                       for vd in trace
                       do (let* ((k (- x y))
                                 (prev-k (if (or (= k (- d))
                                                 (and (/= k d) (< (aref vd (+ (1- k) offset)) (aref vd (+ (1+ k) offset)))))
                                             (1+ k) (1- k)))
                                 (prev-x (aref vd (+ prev-k offset)))
                                 (prev-y (- prev-x prev-k)))
                            (loop while (and (> x prev-x) (> y prev-y))
                                  do (push (list :equal (svref a (1- x))) script)
                                     (decf x) (decf y))
                            (when (plusp d)
                              (if (= x prev-x)
                                  (push (list :insert (svref b (1- y))) script)
                                  (push (list :delete (svref a (1- x))) script))
                              (setq x prev-x y prev-y))))
                 script)))))))

(defun unified-diff (old-text new-text &key (context 3) (old-name "a") (new-name "b"))
  "A unified diff of the two texts, as a string (headers, hunks)."
  (let* ((old-lines (split-lines old-text))
         (new-lines (split-lines new-text))
         (script (diff-lines old-lines new-lines))
         (hunks (hunks-of script context)))
    (with-output-to-string (out)
      (format out "--- ~a~%+++ ~a~%" old-name new-name)
      (dolist (hunk hunks)
        (destructuring-bind (old-start old-count new-start new-count lines) hunk
          (format out "@@ -~d~:[,~d~;~*~] +~d~:[,~d~;~*~] @@~%"
                  old-start (= old-count 1) old-count new-start (= new-count 1) new-count)
          (dolist (line lines)
            (format out "~a~a~%" (ecase (first line) (:equal " ") (:delete "-") (:insert "+")) (second line))))))))

(defun hunks-of (script context)
  "Group SCRIPT into hunks with CONTEXT lines around each change: a list
of (old-start old-count new-start new-count lines), 1-based.  Each edit
is given its old and new line number first; changes closer than twice
the context share a hunk."
  (let* ((edits (coerce script 'simple-vector))
         (n (length edits))
         (old-at (make-array n)) (new-at (make-array n))
         (old 1) (new 1))
    ;; the line numbers each edit stands at
    (dotimes (i n)
      (setf (aref old-at i) old (aref new-at i) new)
      (ecase (first (svref edits i))
        (:equal (incf old) (incf new))
        (:delete (incf old))
        (:insert (incf new))))
    (let ((changes (loop for i below n unless (eq (first (svref edits i)) :equal) collect i))
          (hunks nil))
      (loop while changes
            do (let* ((first-change (pop changes))
                      (last-change first-change))
                 ;; take in every later change within reach
                 (loop while (and changes (<= (- (first changes) last-change) (* 2 context)))
                       do (setq last-change (pop changes)))
                 (let* ((start (max 0 (- first-change context)))
                        (end (min n (+ last-change context 1)))
                        (lines (loop for i from start below end collect (svref edits i))))
                   (push (list (aref old-at start)
                               (count-if (lambda (l) (member (first l) '(:equal :delete))) lines)
                               (aref new-at start)
                               (count-if (lambda (l) (member (first l) '(:equal :insert))) lines)
                               lines)
                         hunks))))
      (nreverse hunks))))

(defun diff-stat (changes)
  "A plist: how many files were added, deleted and modified in CHANGES."
  (list :added (count :added changes :key #'change-status)
        :deleted (count :deleted changes :key #'change-status)
        :modified (count :modified changes :key #'change-status)
        :total (length changes)))
