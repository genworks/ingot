;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Loose objects: objects/ab/cdef..., a zlib stream of "type size\0"
;; and the data.
;;

(defun loose-object-path (gitdir oid)
  (merge-pathnames (format nil "objects/~a/~a" (subseq oid 0 2) (subseq oid 2)) gitdir))

(defun read-loose-object (gitdir oid)
  "The git-object OID among the loose objects of GITDIR, or nil."
  (let ((file (probe-file (loose-object-path gitdir oid))))
    (when file
      (parse-object-with-header (inflate (read-file-octets file)) oid))))

(defun loose-object-exists? (gitdir oid)
  (and (probe-file (loose-object-path gitdir oid)) t))

(defun write-loose-object (gitdir type data)
  "Write DATA as a loose object of TYPE under GITDIR; answers its id.  An
object already there is left alone (its content is its name)."
  (let* ((data (as-octets data))
         (oid (hash-object type data))
         (file (loose-object-path gitdir oid)))
    (unless (probe-file file)
      (write-file-octets file (deflate (octets-concat (object-header type (length data)) data))))
    oid))

(defun loose-oids-with-prefix (gitdir prefix)
  "The ids of the loose objects whose id begins with PREFIX (at least two
characters)."
  (let ((dir (merge-pathnames (format nil "objects/~a/" (subseq prefix 0 2)) gitdir))
        (rest (subseq prefix 2))
        (found nil))
    (when (uiop:directory-exists-p dir)
      (dolist (file (uiop:directory-files dir))
        (let ((name (file-namestring file)))
          (when (and (= (length name) 38)
                     (string= rest name :end2 (min (length rest) 38)))
            (push (concatenate 'string (subseq prefix 0 2) name) found)))))
    found))
