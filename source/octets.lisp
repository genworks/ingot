;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; Octet vectors and what is done to them everywhere else: hex, SHA-1,
;; text, files.  An OID is a 40-character lower-case hex string
;; throughout the library; the binary form appears only inside packs,
;; indexes and tree objects.
;;

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(define-condition git-error (error)
  ((text :initarg :text :reader git-error-text))
  (:report (lambda (c s) (write-string (git-error-text c) s))))

(define-condition not-found (git-error) ())

(define-condition ref-conflict (git-error) ())

(defun git-error (control &rest args)
  (error 'git-error :text (apply #'format nil control args)))

(defun not-found (control &rest args)
  (error 'not-found :text (apply #'format nil control args)))

(defun make-octets (n &optional (initial 0))
  (make-array n :element-type '(unsigned-byte 8) :initial-element initial))

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun as-octets (sequence)
  "SEQUENCE as a simple octet vector (itself when it already is one)."
  (if (typep sequence 'octets)
      sequence
      (make-array (length sequence) :element-type '(unsigned-byte 8) :initial-contents sequence)))

(defun octets-concat (&rest pieces)
  "One octet vector: PIECES (octet vectors, or nil) end to end."
  (let* ((pieces (remove nil pieces))
         (total (reduce #'+ pieces :key #'length))
         (out (make-octets total))
         (at 0))
    (dolist (piece pieces out)
      (replace out piece :start1 at)
      (incf at (length piece)))))

(defun suboctets (vector start &optional end)
  "A fresh simple octet vector of VECTOR from START to END."
  (let* ((end (or end (length vector)))
         (out (make-octets (- end start))))
    (replace out vector :start2 start :end2 end)
    out))

(defun string-to-octets (string)
  (babel:string-to-octets string :encoding :utf-8))

(defun octets-to-string (octets &key (start 0) end)
  "OCTETS as text, UTF-8; what is not UTF-8 is decoded as Latin-1 so that
nothing signals on a stranger's bytes."
  (handler-case (babel:octets-to-string octets :encoding :utf-8 :start start :end (or end (length octets)))
    (error () (babel:octets-to-string octets :encoding :latin-1 :start start :end (or end (length octets))))))

(defun ascii (string)
  "STRING, all of whose characters are below 128, as octets."
  (let ((out (make-octets (length string))))
    (dotimes (i (length string) out)
      (setf (aref out i) (char-code (char string i))))))

;;; Hex

(defparameter *hex-digits* "0123456789abcdef")

(defun hex (octets &key (start 0) end)
  "The hex string of OCTETS."
  (let* ((end (or end (length octets)))
         (out (make-string (* 2 (- end start)))))
    (loop for i from start below end
          for j from 0 by 2
          for byte = (aref octets i)
          do (setf (char out j) (char *hex-digits* (ash byte -4))
                   (char out (1+ j)) (char *hex-digits* (logand byte 15))))
    out))

(defun unhex (string)
  "The octets of the hex STRING."
  (let ((out (make-octets (floor (length string) 2))))
    (dotimes (i (length out) out)
      (setf (aref out i) (parse-integer string :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun oid? (thing)
  "True when THING is a full object id: 40 lower-case hex characters."
  (and (stringp thing) (= (length thing) 40)
       (every (lambda (c) (find c *hex-digits*)) thing)))

(defun short-oid (oid &optional (n 8))
  (subseq oid 0 (min n (length oid))))

;;; SHA-1

(defun sha1-octets (&rest pieces)
  "The 20 octets of the SHA-1 of PIECES end to end (digest.lisp)."
  (sha1-digest (if (and (= (length pieces) 1) (typep (first pieces) 'octets))
                   (first pieces)
                   (apply #'octets-concat pieces))))

(defun sha1-hex (&rest pieces)
  (hex (apply #'sha1-octets pieces)))

;;; Files

(defun read-file-octets (file)
  "The whole of FILE as octets."
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (let ((out (make-octets (file-length in))))
      (read-sequence out in)
      out)))

(defun write-file-octets (file octets &key (if-exists :supersede))
  "Write OCTETS to FILE whole: to a temporary file beside it, then a
rename, so that a reader never sees half a file."
  (ensure-directories-exist file)
  ;; The temporary file keeps the target's type: rename-file merges the
  ;; new name with the old one, so a "HEAD.tmp" renamed to "HEAD" would
  ;; become "HEAD.tmp" again.
  (let ((temporary (make-pathname :name (format nil "tmp-~a-~d-~d" (or (pathname-name file) "file")
                                                (get-universal-time) (random 1000000))
                                  :type (pathname-type file) :defaults file)))
    (with-open-file (out temporary :direction :output :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
      (write-sequence octets out))
    (when (and (eq if-exists :error) (probe-file file))
      (delete-file temporary)
      (git-error "~a exists." file))
    (rename-file-overwriting temporary file)
    file))

(defun rename-file-overwriting (from to)
  "Rename FROM to TO, replacing TO.  Clozure CL's rename-file refuses an
existing target; uiop does the dance."
  (uiop:rename-file-overwriting-target from to))

(defun file-octets-if-exists (file)
  (and (probe-file file) (read-file-octets file)))

;;; Searching

(defun position-of (byte octets &key (start 0) end)
  (position byte octets :start start :end end))

(defun octets-search (needle haystack &key (start 0))
  (search needle haystack :start2 start))

(defun octets= (a b)
  (and (= (length a) (length b)) (every #'= a b)))

;;; Big-endian integers, as packs and indexes carry them

(defun be32 (octets at)
  (logior (ash (aref octets at) 24) (ash (aref octets (+ at 1)) 16)
          (ash (aref octets (+ at 2)) 8) (aref octets (+ at 3))))

(defun be64 (octets at)
  (logior (ash (be32 octets at) 32) (be32 octets (+ at 4))))

(defun put-be32 (octets at value)
  (setf (aref octets at) (ldb (byte 8 24) value)
        (aref octets (+ at 1)) (ldb (byte 8 16) value)
        (aref octets (+ at 2)) (ldb (byte 8 8) value)
        (aref octets (+ at 3)) (ldb (byte 8 0) value))
  octets)

(defun be32-octets (value)
  (put-be32 (make-octets 4) 0 value))

;;; Time, as git writes it: seconds since the epoch and a zone

(defconstant +unix-epoch+ 2208988800)

(defun unix-time (&optional (universal (get-universal-time)))
  (- universal +unix-epoch+))

(defun universal-time (unix)
  (+ unix +unix-epoch+))

(defun zone-string (&optional (offset-minutes 0))
  "\"+0000\": OFFSET-MINUTES east of UTC as git writes a zone."
  (multiple-value-bind (hours minutes) (floor (abs offset-minutes) 60)
    (format nil "~a~2,'0d~2,'0d" (if (minusp offset-minutes) "-" "+") hours minutes)))

;;; An octet output buffer: a growing vector to write into

(defun make-buffer (&optional (size 4096))
  (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun buffer-write (buffer octets &key (start 0) end)
  (loop for i from start below (or end (length octets))
        do (vector-push-extend (aref octets i) buffer (max 1024 (length buffer))))
  buffer)

(defun buffer-byte (buffer byte)
  (vector-push-extend byte buffer (max 1024 (length buffer)))
  buffer)

(defun buffer-string (buffer string)
  (buffer-write buffer (string-to-octets string)))

(defun buffer-octets (buffer)
  "The buffer's contents as a simple octet vector."
  (suboctets buffer 0 (fill-pointer buffer)))
