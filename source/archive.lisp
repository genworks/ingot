;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; A tree as an archive: tar (ustar), tar.gz, and zip (stored, no
;; compression of the entries).  The tar is also how the CI runner
;; hands a job its working tree.
;;

(defun octal-field (value width)
  "VALUE in octal, zero-padded to WIDTH-1 digits and a NUL."
  (let ((text (format nil "~v,'0o" (1- width) value)))
    (octets-concat (ascii text) (octets 0))))

(defun tar-header (name size &key (mode #o644) (type #\0) (mtime 0) link)
  "A 512-byte ustar header for NAME.  A name longer than 100 characters
is split at a slash into the prefix field."
  (let* ((header (make-octets 512))
         (name-octets (string-to-octets name))
         (prefix-octets nil))
    (when (> (length name-octets) 100)
      (let ((slash (position (char-code #\/) name-octets :from-end t :end (min 155 (length name-octets)))))
        (when slash
          (setq prefix-octets (suboctets name-octets 0 slash)
                name-octets (suboctets name-octets (1+ slash))))))
    (when (> (length name-octets) 100) (git-error "~a is too long a name for a tar." name))
    (replace header name-octets)
    (replace header (octal-field mode 8) :start1 100)
    (replace header (octal-field 0 8) :start1 108)          ; uid
    (replace header (octal-field 0 8) :start1 116)          ; gid
    (replace header (octal-field size 12) :start1 124)
    (replace header (octal-field mtime 12) :start1 136)
    (fill header 32 :start 148 :end 156)                     ; checksum blank while summing
    (setf (aref header 156) (char-code type))
    (when link (replace header (string-to-octets link) :start1 157))
    (replace header (ascii "ustar") :start1 257)
    (setf (aref header 262) 0)
    (replace header (ascii "00") :start1 263)
    (replace header (ascii "root") :start1 265)
    (replace header (ascii "root") :start1 297)
    (when prefix-octets (replace header prefix-octets :start1 345))
    (let ((sum (reduce #'+ header)))
      (replace header (ascii (format nil "~6,'0o" sum)) :start1 148)
      (setf (aref header 154) 0 (aref header 155) 32))
    header))

(defun tar-pad (buffer size)
  (let ((rest (mod size 512)))
    (unless (zerop rest)
      (buffer-write buffer (make-octets (- 512 rest))))))

(defun tar-tree (repository tree-oid &key (prefix "") (mtime 0) (buffer (make-buffer (* 256 1024))))
  "A tar of the tree TREE-OID appended to BUFFER, every path under
PREFIX (\"project-1.0/\"); answers the buffer."
  (when (plusp (length prefix))
    (buffer-write buffer (tar-header prefix 0 :mode #o755 :type #\5 :mtime mtime)))
  (walk-tree repository tree-oid
             (lambda (path entry)
               (let ((name (concatenate 'string prefix path)))
                 (case (tree-entry-kind entry)
                   (:tree (buffer-write buffer (tar-header (concatenate 'string name "/") 0
                                                           :mode #o755 :type #\5 :mtime mtime)))
                   (:submodule (buffer-write buffer (tar-header (concatenate 'string name "/") 0
                                                                :mode #o755 :type #\5 :mtime mtime)))
                   (:symlink
                    (let ((target (octets-to-string (read-blob repository (tree-entry-oid entry)))))
                      (buffer-write buffer (tar-header name 0 :mode #o777 :type #\2 :mtime mtime :link target))))
                   (t (let ((data (read-blob repository (tree-entry-oid entry))))
                        (buffer-write buffer (tar-header name (length data)
                                                         :mode (if (eq (tree-entry-kind entry) :executable) #o755 #o644)
                                                         :mtime mtime))
                        (buffer-write buffer data)
                        (tar-pad buffer (length data))))))))
  (buffer-write buffer (make-octets 1024))
  buffer)

(defun tar-gz-tree (repository tree-oid &rest keys)
  "A tar.gz of the tree, as octets."
  (gzip (buffer-octets (apply #'tar-tree repository tree-oid keys))))

;;; Reading a tar (what the Docker API hands back for a path)

(defun tar-field-string (header start end)
  (let ((nul (or (position 0 header :start start :end end) end)))
    (octets-to-string header :start start :end nul)))

(defun tar-field-octal (header start end)
  (let ((text (string-trim '(#\Space #\Nul) (tar-field-string header start end))))
    (if (zerop (length text)) 0 (parse-integer text :radix 8))))

(defun tar-entries (tar)
  "The entries of TAR (octets): a list of plists (:name :size :type
:mode :mtime :link :data), the data an octet vector for a file.  Long
names given with a prefix field or a GNU ././@LongLink entry are
honored."
  (let ((entries nil) (at 0) (n (length tar)) (pending-long-name nil))
    (loop while (<= (+ at 512) n)
          do (let ((header (suboctets tar at (+ at 512))))
               (when (every #'zerop header) (return))
               (let* ((name (tar-field-string header 0 100))
                      (mode (tar-field-octal header 100 108))
                      (size (tar-field-octal header 124 136))
                      (mtime (tar-field-octal header 136 148))
                      (type (code-char (aref header 156)))
                      (link (tar-field-string header 157 257))
                      (magic (tar-field-string header 257 262))
                      (prefix (if (string= magic "ustar") (tar-field-string header 345 500) ""))
                      (data-start (+ at 512))
                      (data-end (min n (+ data-start size)))
                      (full-name (cond (pending-long-name (prog1 pending-long-name (setq pending-long-name nil)))
                                       ((plusp (length prefix)) (concatenate 'string prefix "/" name))
                                       (t name))))
                 (cond ((char= type #\L)
                        (setq pending-long-name (string-right-trim (string (code-char 0)) (octets-to-string tar :start data-start :end data-end))))
                       ((member type '(#\x #\g)))  ; pax headers: skipped
                       (t (push (list :name full-name :size size :type type :mode mode :mtime mtime :link link
                                      :data (and (member type '(#\0 #\Nul #\7)) (suboctets tar data-start data-end)))
                                entries)))
                 (setq at (+ data-start (* 512 (ceiling size 512)))))))
    (nreverse entries)))

(defun tar-append-entry (buffer name entry)
  "Append ENTRY (as tar-entries makes one) to BUFFER under NAME."
  (let ((type (getf entry :type)) (data (getf entry :data)))
    (cond ((char= type #\5)
           (buffer-write buffer (tar-header (if (char= (char name (1- (length name))) #\/) name (concatenate 'string name "/")) 0
                                            :mode (getf entry :mode) :type #\5 :mtime (getf entry :mtime))))
          ((char= type #\2)
           (buffer-write buffer (tar-header name 0 :mode #o777 :type #\2 :mtime (getf entry :mtime) :link (getf entry :link))))
          (data
           (buffer-write buffer (tar-header name (length data) :mode (getf entry :mode) :mtime (getf entry :mtime)))
           (buffer-write buffer data)
           (tar-pad buffer (length data)))
          (t nil))
    buffer))

(defun tar-find (tar path)
  "The data (octets) of PATH in TAR, or nil."
  (let ((entry (find path (tar-entries tar) :key (lambda (e) (string-left-trim "./" (getf e :name))) :test #'string=)))
    (and entry (getf entry :data))))

(defun untar (tar directory)
  "Unpack TAR under DIRECTORY (files and directories; links skipped).
Entries that would leave the directory are refused."
  (let ((root (uiop:ensure-directory-pathname directory)))
    (dolist (entry (tar-entries tar))
      (let ((name (string-left-trim "./" (getf entry :name))))
        (when (or (search ".." name) (and (plusp (length name)) (char= (char name 0) #\/)))
          (git-error "~a would leave the directory." name))
        (cond ((char= (getf entry :type) #\5)
               (ensure-directories-exist (merge-pathnames (uiop:ensure-directory-pathname name) root)))
              ((getf entry :data)
               (let ((file (merge-pathnames name root)))
                 (ensure-directories-exist file)
                 (write-file-octets file (getf entry :data)))))))
    root))

;;; zip, stored entries, with the data descriptor left out (sizes known)

(defun le16 (value) (octets (ldb (byte 8 0) value) (ldb (byte 8 8) value)))
(defun le32 (value) (octets (ldb (byte 8 0) value) (ldb (byte 8 8) value) (ldb (byte 8 16) value) (ldb (byte 8 24) value)))

(defun zip-tree (repository tree-oid &key (prefix ""))
  "A zip of the tree, as octets; every path under PREFIX."
  (let ((buffer (make-buffer (* 256 1024)))
        (central (make-buffer (* 16 1024)))
        (count 0))
    (flet ((entry (name data &key directory?)
             (let* ((name-octets (string-to-octets name))
                    (crc (if directory? 0 (crc32 data)))
                    (size (length data))
                    (offset (fill-pointer buffer))
                    (attributes (if directory? (ash #o40755 16) (ash #o100644 16))))
               ;; local header
               (buffer-write buffer (le32 #x04034b50))
               (buffer-write buffer (le16 20)) (buffer-write buffer (le16 #x0800))   ; version, utf-8 flag
               (buffer-write buffer (le16 0))                                         ; stored
               (buffer-write buffer (le16 0)) (buffer-write buffer (le16 0))          ; time, date
               (buffer-write buffer (le32 crc)) (buffer-write buffer (le32 size)) (buffer-write buffer (le32 size))
               (buffer-write buffer (le16 (length name-octets))) (buffer-write buffer (le16 0))
               (buffer-write buffer name-octets)
               (buffer-write buffer data)
               ;; central directory entry
               (buffer-write central (le32 #x02014b50))
               (buffer-write central (le16 (logior (ash 3 8) 20))) (buffer-write central (le16 20))
               (buffer-write central (le16 #x0800)) (buffer-write central (le16 0))
               (buffer-write central (le16 0)) (buffer-write central (le16 0))
               (buffer-write central (le32 crc)) (buffer-write central (le32 size)) (buffer-write central (le32 size))
               (buffer-write central (le16 (length name-octets)))
               (buffer-write central (le16 0)) (buffer-write central (le16 0))
               (buffer-write central (le16 0)) (buffer-write central (le16 0))
               (buffer-write central (le32 attributes))
               (buffer-write central (le32 offset))
               (buffer-write central name-octets)
               (incf count))))
      (walk-tree repository tree-oid
                 (lambda (path entry)
                   (let ((name (concatenate 'string prefix path)))
                     (case (tree-entry-kind entry)
                       ((:tree :submodule) (entry (concatenate 'string name "/") (make-octets 0) :directory? t))
                       (t (entry name (read-blob repository (tree-entry-oid entry))))))))
      (let ((central-offset (fill-pointer buffer))
            (central-octets (buffer-octets central)))
        (buffer-write buffer central-octets)
        (buffer-write buffer (le32 #x06054b50))
        (buffer-write buffer (le16 0)) (buffer-write buffer (le16 0))
        (buffer-write buffer (le16 count)) (buffer-write buffer (le16 count))
        (buffer-write buffer (le32 (length central-octets)))
        (buffer-write buffer (le32 central-offset))
        (buffer-write buffer (le16 0))
        (buffer-octets buffer)))))
