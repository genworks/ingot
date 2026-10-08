;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Writing a pack, and keeping one.
;;
;; A fetch answers with a pack of the objects asked for.  An object that
;; sits in one of the repository's own packs is copied from there as it
;; lies, compressed, and a delta is copied as a delta when its base goes
;; in the same pack; the rest are written whole.  So a clone of a
;; repository that was pushed gets a pack much the size of the one that
;; was pushed, deltas and all.  A mirror push sends the same.
;;
;; A push's pack is kept as it came, under objects/pack/ with a version
;; 2 index of its own (fanout, sorted ids, a CRC-32 per entry, offsets);
;; the bases a thin pack lacks are appended and the trailer redone.
;; With *store-packs?* nil every object is written loose instead, as
;; before 2026-10-08.
;;

(defparameter *store-packs?* t
  "Whether a pushed pack is kept packed, with an index of its own, rather
than exploded into loose objects.")

(defun write-entry-header (buffer type size)
  (write-raw-entry-header buffer (pack-type-number type) size))

(defun write-pack (repository oids &key (buffer (make-buffer (* 1024 1024))) ofs-delta?)
  "Append a pack of the objects OIDS (ids, all of which the repository
has) to BUFFER; answers the buffer.  Packed objects are copied as they
lie, deltas included when the base goes too (as ofs-deltas when
OFS-DELTA?, which a client asks for); loose objects are written whole."
  (buffer-write buffer (ascii "PACK"))
  (buffer-write buffer (be32-octets 2))
  (buffer-write buffer (be32-octets (length oids)))
  (let ((wanted (make-hash-table :test #'equal))
        (by-pack (make-hash-table :test #'eq))
        (loose nil))
    (dolist (oid oids) (setf (gethash oid wanted) t))
    (dolist (oid oids)
      (multiple-value-bind (index offset) (packed-location repository oid)
        (if index
            (push (cons offset oid) (gethash index by-pack))
            (push oid loose))))
    (flet ((write-whole (oid)
             (let ((object (read-object repository oid)))
               (setf (gethash oid wanted) (fill-pointer buffer))
               (write-entry-header buffer (object-type object) (object-size object))
               (buffer-write buffer (deflate (object-data object))))))
      (maphash (lambda (index entries)
                 (copy-pack-entries index (sort entries #'< :key #'car) buffer wanted ofs-delta? #'write-whole))
               by-pack)
      (dolist (oid (nreverse loose)) (write-whole oid))))
  (buffer-write buffer (sha1-octets (buffer-octets buffer)))
  buffer)

(defun write-pack-of-objects (objects &key (buffer (make-buffer (* 1024 1024))))
  "A pack of OBJECTS (git-object structures) appended to BUFFER."
  (buffer-write buffer (ascii "PACK"))
  (buffer-write buffer (be32-octets 2))
  (buffer-write buffer (be32-octets (length objects)))
  (dolist (object objects)
    (write-entry-header buffer (object-type object) (object-size object))
    (buffer-write buffer (deflate (object-data object))))
  (buffer-write buffer (sha1-octets (buffer-octets buffer)))
  buffer)

(defun store-pack! (repository pack)
  "Take PACK (octets, as a client sent it) into REPOSITORY: kept as a
pack of its own (or, with *store-packs?* nil, every object written
loose).  Answers the ids taken, in pack order."
  (if *store-packs?*
      (store-pack-packed! repository pack)
      (mapcar (lambda (object) (write-object repository (object-type object) (object-data object)))
              (index-pack pack (lambda (oid) (find-object repository oid))))))

(defun store-pack-packed! (repository pack)
  "PACK kept under objects/pack/ with its index.  A thin pack's missing
bases are appended as plain entries, the header's count and the trailer
redone; the pack is named by its trailer, as git names them."
  (let ((entries (parse-pack-entries pack)))
    (when (null entries) (return-from store-pack-packed! nil))
    (multiple-value-bind (entries outside)
        (resolve-pack-entries entries (lambda (oid) (find-object repository oid)))
      (let* ((body-end (- (length pack) 20))
             (buffer (make-buffer (+ (length pack) (* 1024 (length outside)))))
             (index-entries (mapcar (lambda (entry)
                                      (list (unhex (raw-entry-oid entry))
                                            (crc32 (suboctets pack (raw-entry-offset entry) (raw-entry-end entry)))
                                            (raw-entry-offset entry)))
                                    entries)))
        (buffer-write buffer pack :end body-end)
        (dolist (oid outside)
          (let* ((object (find-object repository oid))
                 (offset (fill-pointer buffer)))
            (write-entry-header buffer (object-type object) (object-size object))
            (buffer-write buffer (deflate (object-data object)))
            (push (list (unhex oid) (crc32 (suboctets buffer offset (fill-pointer buffer))) offset) index-entries)))
        (let ((trailer (if outside
                           (progn (put-be32 buffer 8 (+ (length entries) (length outside)))
                                  (sha1-octets (buffer-octets buffer)))
                           (suboctets pack body-end))))
          (buffer-write buffer trailer)
          (let* ((name (hex trailer))
                 (directory (pack-directory repository))
                 (pack-file (merge-pathnames (format nil "pack-~a.pack" name) directory))
                 (idx-file (merge-pathnames (format nil "pack-~a.idx" name) directory)))
            (write-file-octets pack-file (buffer-octets buffer))
            (write-file-octets idx-file (write-pack-index-octets index-entries trailer))
            (register-pack! repository idx-file pack-file)
            (mapcar #'raw-entry-oid entries)))))))
