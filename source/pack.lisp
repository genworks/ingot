;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Packfiles.  Two ways in:
;;
;; - A pack on disk with its .idx (version 2): objects are found by a
;;   binary search of the index and read from the pack file at their
;;   offset, deltas resolved against their base (by offset or by id).
;;   The pack file is opened per read; a repository's object cache keeps
;;   the small objects that are asked for again and again.
;;
;; - A pack arriving from a client (a push), in memory and without an
;;   index: `parse-pack-entries' walks it entry by entry, finding where
;;   each zlib stream ends, and `resolve-pack-entries' applies every
;;   delta (a thin pack's bases come from the repository) so that each
;;   entry has its id.  `store-pack!' (packwriter.lisp) then keeps the
;;   pack as it came, with an index written here, or writes the objects
;;   loose.
;;
;; And one way out: `copy-pack-entries' copies entries of a pack on disk
;; into a pack being written, compressed as they lie, deltas included
;; when the base goes too; that is how a fetch answers with a pack the
;; size of the one that was pushed.
;;

(defconstant +obj-commit+ 1)
(defconstant +obj-tree+ 2)
(defconstant +obj-blob+ 3)
(defconstant +obj-tag+ 4)
(defconstant +obj-ofs-delta+ 6)
(defconstant +obj-ref-delta+ 7)

(defun pack-type-keyword (n)
  (ecase n (1 :commit) (2 :tree) (3 :blob) (4 :tag)))

(defun pack-type-number (keyword)
  (ecase keyword (:commit 1) (:tree 2) (:blob 3) (:tag 4)))

;;; The index

(defstruct (pack-index (:constructor %make-pack-index))
  path                                  ; the .pack file
  (count 0 :type fixnum)
  fanout                                ; 256 cumulative counts
  names                                 ; octets, 20 per object, sorted
  offsets                               ; 32-bit offsets, 4 per object
  large-offsets                         ; 64-bit offsets table, or nil
  mtime
  ends                                  ; offset -> where that entry ends; filled on first use
  oids-by-offset)                       ; offset -> id; the same

(defun read-pack-index (idx-file pack-file)
  (let ((octets (read-file-octets idx-file)))
    (unless (and (= (aref octets 0) 255) (= (aref octets 1) (char-code #\t))
                 (= (aref octets 2) (char-code #\O)) (= (aref octets 3) (char-code #\c))
                 (= (be32 octets 4) 2))
      (git-error "~a is not a version 2 pack index." idx-file))
    (let* ((fanout (make-array 256))
           (count (progn (dotimes (i 256) (setf (aref fanout i) (be32 octets (+ 8 (* 4 i)))))
                         (aref fanout 255)))
           (names-at (+ 8 1024))
           (crcs-at (+ names-at (* 20 count)))
           (offsets-at (+ crcs-at (* 4 count)))
           (large-at (+ offsets-at (* 4 count)))
           (large-count (loop for i below count
                              count (logbitp 31 (be32 octets (+ offsets-at (* 4 i)))))))
      (%make-pack-index :path pack-file :count count :fanout fanout
                        :names (suboctets octets names-at crcs-at)
                        :offsets (suboctets octets offsets-at large-at)
                        :large-offsets (and (plusp large-count)
                                            (suboctets octets large-at (+ large-at (* 8 large-count))))
                        :mtime (file-write-date pack-file)))))

(defun index-name-at (index i)
  (hex (pack-index-names index) :start (* 20 i) :end (* 20 (1+ i))))

(defun index-offset-at (index i)
  (let ((offset (be32 (pack-index-offsets index) (* 4 i))))
    (if (logbitp 31 offset)
        (be64 (pack-index-large-offsets index) (* 8 (logand offset #x7fffffff)))
        offset)))

(defun index-find (index oid)
  "The position of OID in INDEX, or nil."
  (let* ((raw (unhex oid))
         (first (aref raw 0))
         (low (if (zerop first) 0 (aref (pack-index-fanout index) (1- first))))
         (high (aref (pack-index-fanout index) first))
         (names (pack-index-names index)))
    (loop while (< low high)
          do (let* ((mid (floor (+ low high) 2))
                    (cmp (loop for k from 0 below 20
                               for a = (aref raw k) for b = (aref names (+ (* 20 mid) k))
                               unless (= a b) return (if (< a b) -1 1)
                               finally (return 0))))
               (cond ((zerop cmp) (return mid))
                     ((minusp cmp) (setq high mid))
                     (t (setq low (1+ mid))))))))

(defun index-oids-with-prefix (index prefix)
  "The ids in INDEX that begin with PREFIX (at least two hex characters)."
  (let* ((first (parse-integer prefix :end 2 :radix 16))
         (low (if (zerop first) 0 (aref (pack-index-fanout index) (1- first))))
         (high (aref (pack-index-fanout index) first)))
    (loop for i from low below high
          for name = (index-name-at index i)
          when (string= prefix name :end2 (length prefix)) collect name)))

;;; Reading an entry from a pack file

(defun read-varint-header (read-byte)
  "Two values, the type number and the size, of a pack entry header read
with READ-BYTE (a function answering the next byte)."
  (let* ((byte (funcall read-byte))
         (type (ldb (byte 3 4) byte))
         (size (ldb (byte 4 0) byte))
         (shift 4))
    (loop while (logbitp 7 byte)
          do (setq byte (funcall read-byte))
             (setq size (logior size (ash (ldb (byte 7 0) byte) shift)))
             (incf shift 7))
    (values type size)))

(defun read-ofs-delta-distance (read-byte)
  (let* ((byte (funcall read-byte))
         (distance (ldb (byte 7 0) byte)))
    (loop while (logbitp 7 byte)
          do (setq byte (funcall read-byte))
             (setq distance (logior (ash (1+ distance) 7) (ldb (byte 7 0) byte))))
    distance))

(defun apply-delta (base delta)
  "The object DELTA (the bytes of a delta object) makes from BASE."
  (let ((at 0))
    (flet ((varint ()
             (let ((value 0) (shift 0) (byte 0))
               (loop do (setq byte (aref delta at)) (incf at)
                        (setq value (logior value (ash (ldb (byte 7 0) byte) shift)))
                        (incf shift 7)
                     while (logbitp 7 byte))
               value)))
      (let* ((base-size (varint))
             (target-size (varint))
             (out (make-octets target-size))
             (filled 0))
        (unless (= base-size (length base))
          (git-error "A delta expects a base of ~d bytes; the base has ~d." base-size (length base)))
        (loop while (< at (length delta))
              do (let ((opcode (aref delta at)))
                   (incf at)
                   (cond
                     ((logbitp 7 opcode)
                      (let ((offset 0) (size 0))
                        (when (logbitp 0 opcode) (setq offset (aref delta at)) (incf at))
                        (when (logbitp 1 opcode) (setq offset (logior offset (ash (aref delta at) 8))) (incf at))
                        (when (logbitp 2 opcode) (setq offset (logior offset (ash (aref delta at) 16))) (incf at))
                        (when (logbitp 3 opcode) (setq offset (logior offset (ash (aref delta at) 24))) (incf at))
                        (when (logbitp 4 opcode) (setq size (aref delta at)) (incf at))
                        (when (logbitp 5 opcode) (setq size (logior size (ash (aref delta at) 8))) (incf at))
                        (when (logbitp 6 opcode) (setq size (logior size (ash (aref delta at) 16))) (incf at))
                        (when (zerop size) (setq size #x10000))
                        (replace out base :start1 filled :start2 offset :end2 (+ offset size))
                        (incf filled size)))
                     ((zerop opcode) (git-error "A delta holds the reserved opcode 0."))
                     (t (replace out delta :start1 filled :start2 at :end2 (+ at opcode))
                        (incf at opcode)
                        (incf filled opcode)))))
        (unless (= filled target-size)
          (git-error "A delta made ~d bytes of ~d." filled target-size))
        out))))

(defun read-pack-entry (index offset find-by-oid)
  "The git-object at OFFSET in INDEX's pack.  FIND-BY-OID is a function
of an id answering a git-object or nil, for a ref-delta whose base is
anywhere.  Values: the type and the data."
  (with-open-file (in (pack-index-path index) :element-type '(unsigned-byte 8))
    (labels ((entry (offset)
               (file-position in offset)
               (multiple-value-bind (type size) (read-varint-header (lambda () (read-byte in)))
                 (cond
                   ((= type +obj-ofs-delta+)
                    (let* ((distance (read-ofs-delta-distance (lambda () (read-byte in))))
                           (delta (inflate-stream in size)))
                      (multiple-value-bind (base-type base) (entry (- offset distance))
                        (values base-type (apply-delta base delta)))))
                   ((= type +obj-ref-delta+)
                    (let ((base-oid (make-octets 20)))
                      (read-sequence base-oid in)
                      (let* ((delta (inflate-stream in size))
                             (base (funcall find-by-oid (hex base-oid))))
                        (unless base (git-error "A delta's base ~a is missing." (hex base-oid)))
                        (values (object-type base) (apply-delta (object-data base) delta)))))
                   (t (values (pack-type-keyword type) (inflate-stream in size)))))))
      (entry offset))))

(defun inflate-stream (stream expected-size)
  "EXPECTED-SIZE octets inflated from the zlib stream at STREAM's
position.  A window of the file is read into memory first (zlib never
grows data by more than a fraction of a percent plus a few bytes, so
the window is bounded by the object's own size), and inflated from
there: chipz inflates from a vector where it balks at a file stream."
  (let* ((at (file-position stream))
         (window (min (- (file-length stream) at)
                      (+ expected-size (ceiling expected-size 1000) 64)))
         (compressed (make-octets window)))
    (read-sequence compressed stream)
    (values (inflate-at compressed 0 expected-size))))

;;; A pack in memory, without an index: a push

(defstruct raw-entry
  offset end type size data base-offset base-oid oid object)

(defun parse-pack-entries (pack)
  "The entries of PACK (octets: header, entries, checksum), each with its
inflated bytes (a delta's bytes for a delta), in pack order."
  (unless (and (>= (length pack) 32)
               (= (aref pack 0) (char-code #\P)) (= (aref pack 1) (char-code #\A))
               (= (aref pack 2) (char-code #\C)) (= (aref pack 3) (char-code #\K)))
    (git-error "Not a pack."))
  (let ((version (be32 pack 4))
        (count (be32 pack 8))
        (at 12)
        (entries nil))
    (unless (member version '(2 3)) (git-error "Pack version ~d." version))
    (let ((checksum (sha1-octets (suboctets pack 0 (- (length pack) 20)))))
      (unless (octets= checksum (suboctets pack (- (length pack) 20)))
        (git-error "The pack's checksum does not match.")))
    (dotimes (i count)
      (let ((offset at))
        (multiple-value-bind (type size)
            (read-varint-header (lambda () (prog1 (aref pack at) (incf at))))
          (let ((entry (make-raw-entry :offset offset :size size)))
            (cond ((= type +obj-ofs-delta+)
                   (let ((distance (read-ofs-delta-distance (lambda () (prog1 (aref pack at) (incf at))))))
                     (setf (raw-entry-type entry) :ofs-delta
                           (raw-entry-base-offset entry) (- offset distance))))
                  ((= type +obj-ref-delta+)
                   (setf (raw-entry-type entry) :ref-delta
                         (raw-entry-base-oid entry) (hex pack :start at :end (+ at 20)))
                   (incf at 20))
                  (t (setf (raw-entry-type entry) (pack-type-keyword type))))
            (multiple-value-bind (data next) (inflate-at pack at size)
              (setf (raw-entry-data entry) data
                    (raw-entry-end entry) next)
              (setq at next))
            (push entry entries)))))
    (nreverse entries)))

(defun resolve-pack-entries (entries find-by-oid)
  "ENTRIES (from `parse-pack-entries') with their objects and ids: every
delta applied to its base, a base the pack lacks taken from FIND-BY-OID
(a thin pack).  Values: the entries, and the ids of the bases that came
from outside the pack."
  (let ((by-offset (make-hash-table))
        (by-oid (make-hash-table :test #'equal))
        (outside nil))
    (dolist (entry entries) (setf (gethash (raw-entry-offset entry) by-offset) entry))
    (labels ((settle (entry object)
               (setf (raw-entry-object entry) object
                     (raw-entry-oid entry) (object-oid object)
                     (gethash (object-oid object) by-oid) entry)
               object)
             (delta-of (entry base-object)
               (settle entry (make-git-object (object-type base-object)
                                              (apply-delta (object-data base-object) (raw-entry-data entry)))))
             (resolve (entry depth final?)
               ;; the entry's object, or nil when a base of its chain is
               ;; not known yet (a ref-delta whose base comes later); with
               ;; FINAL? such a base is asked of the repository instead
               (when (> depth 1000) (git-error "A delta chain too deep."))
               (or (raw-entry-object entry)
                   (ecase (raw-entry-type entry)
                     (:ofs-delta
                      (let ((base (gethash (raw-entry-base-offset entry) by-offset)))
                        (unless base (git-error "A delta names an offset outside the pack."))
                        (let ((base-object (resolve base (1+ depth) final?)))
                          (and base-object (delta-of entry base-object)))))
                     (:ref-delta
                      (let* ((oid (raw-entry-base-oid entry))
                             (in-pack (gethash oid by-oid))
                             (base-object (cond (in-pack (resolve in-pack (1+ depth) final?))
                                                (final? (let ((found (funcall find-by-oid oid)))
                                                          (when found (pushnew oid outside :test #'string=))
                                                          found))
                                                (t nil))))
                        (cond (base-object (delta-of entry base-object))
                              (final? (git-error "A delta's base ~a is not in the pack nor here." oid))
                              (t nil))))
                     ((:commit :tree :blob :tag)
                      (settle entry (make-git-object (raw-entry-type entry) (raw-entry-data entry))))))))
      ;; passes over what is left until nothing moves; only then are
      ;; bases asked of the repository
      (let ((pending entries) (final? nil))
        (loop while pending
              do (let ((before (length pending)))
                   (setq pending (remove-if (lambda (entry) (resolve entry 0 final?)) pending))
                   (when (= (length pending) before)
                     (setq final? t)))))
      (values entries (nreverse outside)))))

(defun index-pack (pack find-by-oid)
  "The git-objects in PACK (octets), every delta resolved; FIND-BY-OID
answers an object the repository already has, for a thin pack.  Answers
the list of objects in pack order."
  (mapcar #'raw-entry-object (resolve-pack-entries (parse-pack-entries pack) find-by-oid)))

;;; Writing entries, and a pack's index

(defun write-raw-entry-header (buffer type-number size)
  "A pack entry's header: the type in three bits and the size, base 128."
  (let ((byte (logior (ash type-number 4) (ldb (byte 4 0) size)))
        (rest (ash size -4)))
    (loop
      (if (zerop rest)
          (progn (buffer-byte buffer byte) (return))
          (progn (buffer-byte buffer (logior byte #x80))
                 (setq byte (ldb (byte 7 0) rest)
                       rest (ash rest -7)))))))

(defun write-ofs-delta-distance (buffer distance)
  "DISTANCE (bytes back to the base) as git encodes it: big-endian, seven
bits a byte, every byte but the last carrying one more than its digit."
  (let ((bytes (list (ldb (byte 7 0) distance))))
    (loop (setq distance (ash distance -7))
          (when (zerop distance) (return))
          (decf distance)
          (push (logior #x80 (ldb (byte 7 0) distance)) bytes))
    (dolist (byte bytes) (buffer-byte buffer byte))))

(defun octets< (a b)
  (loop for x across a for y across b
        unless (= x y) return (< x y)
        finally (return nil)))

(defun write-pack-index-octets (entries pack-sha)
  "A version 2 pack index, as octets: ENTRIES a list of (raw-oid crc
offset), PACK-SHA the pack's trailing checksum (20 octets)."
  (let* ((sorted (sort (copy-list entries) #'octets< :key #'first))
         (count (length sorted))
         (buffer (make-buffer (+ 1032 (* count 28) 40)))
         (large nil))
    (buffer-write buffer (octets 255 (char-code #\t) (char-code #\O) (char-code #\c)))
    (buffer-write buffer (be32-octets 2))
    (let ((seen 0) (rest sorted))
      (dotimes (first 256)
        (loop while (and rest (= (aref (first (car rest)) 0) first))
              do (incf seen) (pop rest))
        (buffer-write buffer (be32-octets seen))))
    (dolist (entry sorted) (buffer-write buffer (first entry)))
    (dolist (entry sorted) (buffer-write buffer (be32-octets (second entry))))
    (dolist (entry sorted)
      (let ((offset (third entry)))
        (if (< offset #x80000000)
            (buffer-write buffer (be32-octets offset))
            (progn (buffer-write buffer (be32-octets (logior #x80000000 (length large))))
                   (setq large (append large (list offset)))))))
    (dolist (offset large)
      (buffer-write buffer (be32-octets (ash offset -32)))
      (buffer-write buffer (be32-octets (ldb (byte 32 0) offset))))
    (buffer-write buffer pack-sha)
    (buffer-write buffer (sha1-octets (buffer-octets buffer)))
    (buffer-octets buffer)))

(defun ensure-offset-tables (index)
  "INDEX with its two tables by offset filled: where each entry ends,
and which id sits at each offset.  Computed on first use."
  (unless (pack-index-ends index)
    (let* ((count (pack-index-count index))
           (offsets (make-array count))
           (by-offset (make-hash-table))
           (ends (make-hash-table))
           (size (with-open-file (in (pack-index-path index) :element-type '(unsigned-byte 8))
                   (file-length in))))
      (dotimes (i count)
        (let ((offset (index-offset-at index i)))
          (setf (aref offsets i) offset
                (gethash offset by-offset) (index-name-at index i))))
      (setq offsets (sort offsets #'<))
      (dotimes (i count)
        (setf (gethash (aref offsets i) ends)
              (if (< (1+ i) count) (aref offsets (1+ i)) (- size 20))))
      (setf (pack-index-oids-by-offset index) by-offset
            (pack-index-ends index) ends)))
  index)

(defun copy-pack-entries (index entries buffer wanted ofs-delta? write-whole)
  "Append ENTRIES, a list of (offset . oid) in offset order, of INDEX's
pack to BUFFER as they lie in the file, compressed.  WANTED maps an id
that goes into the pack to t, or to its offset in BUFFER once written.
A delta is copied as a delta when its base goes too: as an ofs-delta
when OFS-DELTA? and the base is written already, else as a ref-delta;
a delta whose base stays behind is written whole by WRITE-WHOLE, a
function of an id."
  (ensure-offset-tables index)
  (let ((ends (pack-index-ends index))
        (by-offset (pack-index-oids-by-offset index)))
    (with-open-file (in (pack-index-path index) :element-type '(unsigned-byte 8))
      (loop for (offset . oid) in entries
            do (file-position in offset)
               (multiple-value-bind (type size) (read-varint-header (lambda () (read-byte in)))
                 (let* ((delta? (or (= type +obj-ofs-delta+) (= type +obj-ref-delta+)))
                        (base-oid (cond ((= type +obj-ofs-delta+)
                                         (gethash (- offset (read-ofs-delta-distance (lambda () (read-byte in)))) by-offset))
                                        ((= type +obj-ref-delta+)
                                         (let ((raw (make-octets 20)))
                                           (read-sequence raw in)
                                           (hex raw)))
                                        (t nil))))
                   (if (and delta? (not (and base-oid (gethash base-oid wanted))))
                       (funcall write-whole oid)
                       (let* ((payload-at (file-position in))
                              (payload (make-octets (- (gethash offset ends) payload-at)))
                              (out-offset (fill-pointer buffer)))
                         (read-sequence payload in)
                         (setf (gethash oid wanted) out-offset)
                         (cond ((not delta?)
                                (write-raw-entry-header buffer type size))
                               ((and ofs-delta? (integerp (gethash base-oid wanted)))
                                (write-raw-entry-header buffer +obj-ofs-delta+ size)
                                (write-ofs-delta-distance buffer (- out-offset (gethash base-oid wanted))))
                               (t (write-raw-entry-header buffer +obj-ref-delta+ size)
                                  (buffer-write buffer (unhex base-oid))))
                         (buffer-write buffer payload)))))))))
