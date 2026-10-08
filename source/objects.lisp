;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; The four objects: blob, tree, commit, tag.  A `git-object' is a type,
;; its data (the bytes after the header) and its id.  Trees, commits and
;; tags are parsed into structures on request; a blob is its data.
;;

(defstruct (git-object (:constructor %make-git-object))
  (type nil :type keyword)              ; :blob :tree :commit :tag
  (data nil :type octets)
  (oid nil))

(defun object-type (object) (git-object-type object))
(defun object-data (object) (git-object-data object))
(defun object-size (object) (length (git-object-data object)))

(defun object-header (type size)
  (ascii (format nil "~(~a~) ~d~a" type size (code-char 0))))

(defun hash-object (type data)
  "The id of an object of TYPE with DATA."
  (sha1-hex (object-header type (length data)) data))

(defun make-git-object (type data &optional oid)
  (%make-git-object :type type :data (as-octets data) :oid (or oid (hash-object type data))))

(defun object-oid (object) (git-object-oid object))

(defun type-keyword (name)
  (cond ((string= name "blob") :blob)
        ((string= name "tree") :tree)
        ((string= name "commit") :commit)
        ((string= name "tag") :tag)
        (t (git-error "~a is not an object type." name))))

(defun parse-object-with-header (octets &optional oid)
  "A git-object from OCTETS, which begin with the \"type size\\0\" header
(a loose object, inflated)."
  (let* ((space (position-of 32 octets))
         (nul (position-of 0 octets :start space))
         (type (type-keyword (octets-to-string octets :end space)))
         (size (parse-integer (octets-to-string octets :start (1+ space) :end nul)))
         (data (suboctets octets (1+ nul))))
    (unless (= size (length data))
      (git-error "An object's header says ~d bytes; ~d follow." size (length data)))
    (make-git-object type data oid)))

;;; Trees

(defstruct tree-entry
  (mode nil :type string)               ; "100644" "100755" "120000" "40000" "160000"
  (name nil :type string)
  (oid nil :type string))

(defun tree-entry-kind (entry)
  "What the entry is: :tree, :blob, :symlink, :submodule, :executable."
  (let ((mode (tree-entry-mode entry)))
    (cond ((string= mode "40000") :tree)
          ((string= mode "040000") :tree)
          ((string= mode "160000") :submodule)
          ((string= mode "120000") :symlink)
          ((string= mode "100755") :executable)
          (t :blob))))

(defun tree-entry-tree? (entry) (eq (tree-entry-kind entry) :tree))

(defun parse-tree (data)
  "The entries of a tree object's DATA, in the order written."
  (let ((entries nil) (at 0) (n (length data)))
    (loop while (< at n)
          do (let* ((space (position-of 32 data :start at))
                    (nul (position-of 0 data :start space))
                    (mode (octets-to-string data :start at :end space))
                    (name (octets-to-string data :start (1+ space) :end nul))
                    (oid (hex data :start (1+ nul) :end (+ nul 21))))
               (push (make-tree-entry :mode mode :name name :oid oid) entries)
               (setq at (+ nul 21))))
    (nreverse entries)))

(defun tree-sort-key (entry)
  "Git sorts a tree's entries as if a subtree's name ended in a slash."
  (if (tree-entry-tree? entry)
      (concatenate 'string (tree-entry-name entry) "/")
      (tree-entry-name entry)))

(defun sort-tree-entries (entries)
  (sort (copy-list entries) (lambda (a b) (octets< (string-to-octets (tree-sort-key a))
                                                   (string-to-octets (tree-sort-key b))))))

(defun octets< (a b)
  (let ((n (min (length a) (length b))))
    (dotimes (i n (< (length a) (length b)))
      (when (/= (aref a i) (aref b i))
        (return (< (aref a i) (aref b i)))))))

(defun serialize-tree (entries)
  "A tree object's data from ENTRIES (sorted here as git sorts them)."
  (let ((buffer (make-buffer)))
    (dolist (entry (sort-tree-entries entries))
      (buffer-write buffer (ascii (string-left-trim "0" (tree-entry-mode entry))))
      (buffer-byte buffer 32)
      (buffer-write buffer (string-to-octets (tree-entry-name entry)))
      (buffer-byte buffer 0)
      (buffer-write buffer (unhex (tree-entry-oid entry))))
    (buffer-octets buffer)))

;;; Identities: "Name <email> 1700000000 +0100"

;; (Not named `identity': that is a function of the standard.)
(defstruct ident
  (name "" :type string)
  (email "" :type string)
  (time 0 :type integer)                ; unix seconds
  (zone "+0000" :type string))

(defun parse-identity (text)
  "An ident from TEXT as a commit writes it."
  (let* ((lt (position #\< text))
         (gt (and lt (position #\> text :start lt)))
         (rest (and gt (string-trim " " (subseq text (1+ gt)))))
         (space (and rest (position #\Space rest))))
    (make-ident :name (string-trim " " (subseq text 0 (or lt (length text))))
                :email (if gt (subseq text (1+ lt) gt) "")
                :time (or (and rest (plusp (length rest))
                               (ignore-errors (parse-integer rest :end space)))
                          0)
                :zone (if (and space (< (1+ space) (length rest))) (subseq rest (1+ space)) "+0000"))))

(defun identity-string (ident)
  (format nil "~a <~a> ~d ~a" (ident-name ident) (ident-email ident)
          (ident-time ident) (ident-zone ident)))

(defun make-identity (name email &key (zone "+0000") (time (unix-time)))
  "An ident for NAME and EMAIL, now unless TIME (unix seconds) says."
  (make-ident :name name :email email :time time :zone zone))

;;; Commits

(defstruct commit
  (oid nil)
  (tree nil :type string)
  (parents nil :type list)
  (author nil)
  (committer nil)
  (message "" :type string)
  (headers nil :type list))             ; the other headers, (name . value), gpgsig among them

(defun commit-author-name (commit) (ident-name (commit-author commit)))
(defun commit-author-email (commit) (ident-email (commit-author commit)))
(defun commit-author-time (commit) (ident-time (commit-author commit)))
(defun commit-committer-time (commit) (ident-time (commit-committer commit)))

(defun commit-subject (commit)
  "The first line of the message."
  (let* ((message (commit-message commit))
         (end (or (position #\Newline message) (length message))))
    (string-trim " " (subseq message 0 end))))

(defun split-headers (text)
  "Two values: the header lines (name . value) of a commit or tag TEXT,
continuation lines joined with a newline, and the message after the
blank line."
  (let ((headers nil) (at 0) (n (length text)) (last nil))
    (loop
      (when (>= at n) (return))
      (let* ((eol (or (position #\Newline text :start at) n))
             (line (subseq text at eol)))
        (setq at (1+ eol))
        (cond ((zerop (length line)) (return))
              ((char= (char line 0) #\Space)
               (when last (setf (cdr last) (concatenate 'string (cdr last) (string #\Newline) (subseq line 1)))))
              (t (let ((space (or (position #\Space line) (length line))))
                   (setq last (cons (subseq line 0 space) (subseq line (min (length line) (1+ space)))))
                   (push last headers))))))
    (values (nreverse headers) (if (<= at n) (subseq text at) ""))))

(defun parse-commit (data &optional oid)
  (multiple-value-bind (headers message) (split-headers (octets-to-string data))
    (let ((commit (make-commit :oid oid :tree "" :message message)))
      (dolist (header headers)
        (let ((name (car header)) (value (cdr header)))
          (cond ((string= name "tree") (setf (commit-tree commit) value))
                ((string= name "parent") (setf (commit-parents commit) (append (commit-parents commit) (list value))))
                ((string= name "author") (setf (commit-author commit) (parse-identity value)))
                ((string= name "committer") (setf (commit-committer commit) (parse-identity value)))
                (t (push header (commit-headers commit))))))
      (setf (commit-headers commit) (nreverse (commit-headers commit)))
      (unless (commit-committer commit) (setf (commit-committer commit) (or (commit-author commit) (make-ident))))
      (unless (commit-author commit) (setf (commit-author commit) (commit-committer commit)))
      commit)))

(defun serialize-commit (commit)
  (string-to-octets
   (with-output-to-string (out)
     (format out "tree ~a~%" (commit-tree commit))
     (dolist (parent (commit-parents commit)) (format out "parent ~a~%" parent))
     (format out "author ~a~%" (identity-string (commit-author commit)))
     (format out "committer ~a~%" (identity-string (commit-committer commit)))
     (dolist (header (commit-headers commit))
       (format out "~a ~a~%" (car header) (substitute-continuations (cdr header))))
     (terpri out)
     (write-string (commit-message commit) out))))

(defun substitute-continuations (value)
  "A multi-line header value with each later line indented by a space."
  (with-output-to-string (out)
    (loop for c across value
          do (write-char c out)
             (when (char= c #\Newline) (write-char #\Space out)))))

;;; Tags

(defstruct tag
  (oid nil)
  (object nil :type string)
  (type nil :type keyword)
  (name "" :type string)
  (tagger nil)
  (message "" :type string))

(defun parse-tag (data &optional oid)
  (multiple-value-bind (headers message) (split-headers (octets-to-string data))
    (let ((tag (make-tag :oid oid :object "" :type :commit :message message)))
      (dolist (header headers)
        (let ((name (car header)) (value (cdr header)))
          (cond ((string= name "object") (setf (tag-object tag) value))
                ((string= name "type") (setf (tag-type tag) (type-keyword value)))
                ((string= name "tag") (setf (tag-name tag) value))
                ((string= name "tagger") (setf (tag-tagger tag) (parse-identity value))))))
      tag)))

(defun serialize-tag (tag)
  (string-to-octets
   (with-output-to-string (out)
     (format out "object ~a~%type ~(~a~)~%tag ~a~%" (tag-object tag) (tag-type tag) (tag-name tag))
     (when (tag-tagger tag) (format out "tagger ~a~%" (identity-string (tag-tagger tag))))
     (terpri out)
     (write-string (tag-message tag) out))))
