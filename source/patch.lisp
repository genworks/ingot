;; Copyright © 2026 Genworks International
;;
;; SPDX-License-Identifier: MIT
;; Released under the MIT License; see the LICENSE file.

(in-package :ingot)

;;
;; The text git prints for a change: `diff --git` headers, mode and
;; index lines, hunks with the "\ No newline at end of file" marker, the
;; line that says two binaries differ; a commit's patch, its --stat, and
;; the email-shaped patch of format-patch.  The one thing git does that
;; these do not is rename detection: a renamed file is a deletion and a
;; creation here.
;;

;;; Lines and hunks

(defun lines-keeping-ends (text)
  "TEXT split into lines that keep their newline, so that a last line
without one differs from the same line with one, as it does for git."
  (let ((lines nil) (start 0) (n (length text)))
    (loop while (< start n)
          do (let ((eol (position #\Newline text :start start)))
               (push (subseq text start (if eol (1+ eol) n)) lines)
               (setq start (if eol (1+ eol) n))))
    (nreverse lines)))

(defun patch-hunks (old-text new-text &key (context 3))
  "The hunks of a unified diff of the two texts: a list of
(old-start old-count new-start new-count edits), each edit (op line)
with op :equal, :delete or :insert and the line keeping its newline
when it had one."
  (let* ((edits (coerce (diff-lines (lines-keeping-ends old-text) (lines-keeping-ends new-text)) 'simple-vector))
         (n (length edits))
         (old-at (make-array n)) (new-at (make-array n))
         (old 1) (new 1))
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
                 (loop while (and changes (<= (- (first changes) last-change) (* 2 context)))
                       do (setq last-change (pop changes)))
                 (let* ((start (max 0 (- first-change context)))
                        (end (min n (+ last-change context 1)))
                        (lines (loop for i from start below end collect (svref edits i))))
                   (push (list (aref old-at start)
                               (count-if (lambda (e) (member (first e) '(:equal :delete))) lines)
                               (aref new-at start)
                               (count-if (lambda (e) (member (first e) '(:equal :insert))) lines)
                               lines)
                         hunks))))
      (nreverse hunks))))

(defun write-patch-hunks (hunks out)
  (dolist (hunk hunks)
    (destructuring-bind (old-start old-count new-start new-count edits) hunk
      (flet ((range (start count)
               ;; git names the line before the hunk when it holds no lines
               (cond ((= count 1) (format nil "~d" start))
                     ((zerop count) (format nil "~d,0" (1- start)))
                     (t (format nil "~d,~d" start count)))))
        (format out "@@ -~a +~a @@~%" (range old-start old-count) (range new-start new-count)))
      (dolist (edit edits)
        (let* ((line (second edit))
               (ends? (and (plusp (length line)) (char= (char line (1- (length line))) #\Newline))))
          (write-string (ecase (first edit) (:equal " ") (:delete "-") (:insert "+")) out)
          (write-string (if ends? (subseq line 0 (1- (length line))) line) out)
          (terpri out)
          (unless ends? (format out "\\ No newline at end of file~%")))))))

;;; One change

(defun six-digit-mode (mode)
  "A tree entry's mode as diffs print it: six digits (\"40000\" -> \"040000\")."
  (if (and mode (< (length mode) 6))
      (concatenate 'string (make-string (- 6 (length mode)) :initial-element #\0) mode)
      mode))

(defun change-content (repository oid mode)
  "What a diff compares for the entry OID of MODE: the blob's text,
:binary for a blob that looks binary, the pointer line of a submodule,
\"\" for no entry."
  (cond ((null oid) "")
        ((string= (six-digit-mode mode) "160000") (format nil "Subproject commit ~a~%" oid))
        (t (let ((octets (read-blob repository oid)))
             (if (binary-octets? octets) :binary (octets-to-string octets))))))

(defun change-patch (repository change &key full-binary-index)
  "The text `git diff` prints for CHANGE: the diff --git header, the
mode and index lines, and the hunks (or the one line that says two
binaries differ).  FULL-BINARY-INDEX writes a binary change's index line
with whole ids, as format-patch does."
  (let* ((path (change-path change))
         (status (change-status change))
         (old-oid (change-old-oid change))
         (new-oid (change-new-oid change))
         (old-mode (six-digit-mode (change-old-mode change)))
         (new-mode (six-digit-mode (change-new-mode change))))
    (with-output-to-string (out)
      (format out "diff --git a/~a b/~a~%" path path)
      (ecase status
        (:added (format out "new file mode ~a~%" new-mode))
        (:deleted (format out "deleted file mode ~a~%" old-mode))
        ((:modified :type-changed :renamed)
         (unless (equal old-mode new-mode)
           (format out "old mode ~a~%new mode ~a~%" old-mode new-mode))))
      (unless (equal old-oid new-oid)
        (let* ((old (change-content repository old-oid old-mode))
               (new (change-content repository new-oid new-mode))
               (binary? (or (eq old :binary) (eq new :binary)))
               (whole? (and binary? full-binary-index))
               (old-name (if old-oid (format nil "a/~a" path) "/dev/null"))
               (new-name (if new-oid (format nil "b/~a" path) "/dev/null")))
          (flet ((id (oid) (let ((oid (or oid *zero-oid*))) (if whole? oid (short-oid oid 7)))))
            (format out "index ~a..~a~@[ ~a~]~%" (id old-oid) (id new-oid)
                    (and old-oid new-oid (equal old-mode new-mode) new-mode)))
          (if binary?
              (format out "Binary files ~a and ~a differ~%" old-name new-name)
              (let ((hunks (patch-hunks old new)))
                (when hunks
                  (format out "--- ~a~%+++ ~a~%" old-name new-name)
                  (write-patch-hunks hunks out)))))))))

(defun patch-text (repository changes &key full-binary-index)
  "The patch of CHANGES (change structures, as tree-diff and commit-diff
answer), one change-patch after another."
  (with-output-to-string (out)
    (dolist (change changes)
      (write-string (change-patch repository change :full-binary-index full-binary-index) out))))

(defun commit-patch (repository commit-oid)
  "The patch of the commit against its first parent (an empty tree for a
root commit): what a forge serves as <sha>.diff."
  (patch-text repository (commit-diff repository commit-oid)))

;;; --stat

(defun change-counts (repository change)
  "Three values for CHANGE: lines added, lines deleted, and whether it
is binary (then the two counts are the sizes before and after)."
  (let* ((old-oid (change-old-oid change))
         (new-oid (change-new-oid change))
         (old (change-content repository old-oid (change-old-mode change)))
         (new (change-content repository new-oid (change-new-mode change))))
    (cond ((equal old-oid new-oid) (values 0 0 nil))
          ((or (eq old :binary) (eq new :binary))
           (values (if old-oid (object-size (read-object repository old-oid)) 0)
                   (if new-oid (object-size (read-object repository new-oid)) 0)
                   t))
          (t (let ((added 0) (deleted 0))
               (dolist (edit (diff-lines (lines-keeping-ends old) (lines-keeping-ends new)))
                 (case (first edit) (:insert (incf added)) (:delete (incf deleted))))
               (values added deleted nil))))))

(defun stat-text (repository changes &key summary?)
  "The --stat text for CHANGES: a line per file and the summary line, as
git prints them (the +/- graph is not scaled to a terminal width); with
SUMMARY?, the create/delete/mode-change lines format-patch adds."
  (let ((rows (mapcar (lambda (change)
                        (multiple-value-bind (added deleted binary?) (change-counts repository change)
                          (list (change-path change) added deleted binary?)))
                      changes)))
    (with-output-to-string (out)
      (when rows
        (let ((name-width (reduce #'max rows :key (lambda (row) (length (first row)))))
              (count-width (reduce #'max rows
                                   :key (lambda (row) (if (fourth row) 3 (length (princ-to-string (+ (second row) (third row))))))))
              (files 0) (insertions 0) (deletions 0))
          (dolist (row rows)
            (destructuring-bind (path added deleted binary?) row
              (incf files)
              (if binary?
                  (format out " ~va | Bin ~d -> ~d bytes~%" name-width path added deleted)
                  (progn
                    (incf insertions added) (incf deletions deleted)
                    (format out " ~va | ~vd~@[ ~a~]~%" name-width path count-width (+ added deleted)
                            (and (plusp (+ added deleted))
                                 (concatenate 'string
                                              (make-string added :initial-element #\+)
                                              (make-string deleted :initial-element #\-))))))))
          (format out " ~d file~:p changed" files)
          (when (plusp insertions) (format out ", ~d insertion~:p(+)" insertions))
          (when (plusp deletions) (format out ", ~d deletion~:p(-)" deletions))
          (terpri out)
          (when summary?
            (dolist (change changes)
              (let ((old-mode (six-digit-mode (change-old-mode change)))
                    (new-mode (six-digit-mode (change-new-mode change))))
                (case (change-status change)
                  (:added (format out " create mode ~a ~a~%" new-mode (change-path change)))
                  (:deleted (format out " delete mode ~a ~a~%" old-mode (change-path change)))
                  (t (unless (equal old-mode new-mode)
                       (format out " mode change ~a => ~a ~a~%" old-mode new-mode (change-path change)))))))))))))

;;; format-patch

(defun format-patch (repository commit-oid &key (signature "ingot"))
  "The commit as `git format-patch` prints it: the mbox From line, the
headers, the message, the stat and the patch against the first parent,
SIGNATURE under the closing `-- `."
  (let* ((commit (read-commit repository commit-oid))
         (changes (commit-diff repository (commit-oid commit))))
    (multiple-value-bind (subject body) (commit-subject-and-body commit)
      (with-output-to-string (out)
        (format out "From ~a Mon Sep 17 00:00:00 2001~%" (commit-oid commit))
        (format out "From: ~a <~a>~%" (commit-author-name commit) (commit-author-email commit))
        (format out "Date: ~a~%" (ident-date-string (commit-author commit) :style :rfc2822))
        (format out "Subject: [PATCH] ~a~%~%" subject)
        (when (plusp (length body)) (format out "~a~%" body))
        (format out "---~%~a~%" (stat-text repository changes :summary? t))
        (write-string (patch-text repository changes :full-binary-index t) out)
        (format out "-- ~%~a~%~%" signature)))))
