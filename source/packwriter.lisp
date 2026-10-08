;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Writing a pack: the header, each object as a plain (undeltified)
;; entry, the SHA-1 of the whole at the end.  What a fetch answers with,
;; and what a mirror push sends.
;;

(defun write-entry-header (buffer type size)
  (let ((byte (logior (ash (pack-type-number type) 4) (ldb (byte 4 0) size)))
        (rest (ash size -4)))
    (loop
      (if (zerop rest)
          (progn (buffer-byte buffer byte) (return))
          (progn (buffer-byte buffer (logior byte #x80))
                 (setq byte (ldb (byte 7 0) rest)
                       rest (ash rest -7)))))))

(defun write-pack (repository oids &key (buffer (make-buffer (* 1024 1024))))
  "Append a pack of the objects OIDS (ids, all of which the repository
has) to BUFFER; answers the buffer.  Objects are written whole."
  (buffer-write buffer (ascii "PACK"))
  (buffer-write buffer (be32-octets 2))
  (buffer-write buffer (be32-octets (length oids)))
  (dolist (oid oids)
    (let ((object (read-object repository oid)))
      (write-entry-header buffer (object-type object) (object-size object))
      (buffer-write buffer (deflate (object-data object)))))
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
  "Take PACK (octets, as a client sent it) into REPOSITORY: every object
written loose.  Answers the ids written, in pack order."
  (let ((objects (index-pack pack (lambda (oid) (find-object repository oid)))))
    (mapcar (lambda (object)
              (write-object repository (object-type object) (object-data object)))
            objects)))
