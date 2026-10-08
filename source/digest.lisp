;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; SHA-1 (what names every object), SHA-256 and HMAC and PBKDF2 (what
;; the forge hashes passwords with), and CRC-32 (zip): in plain Common
;; Lisp, so that nothing here depends on a library a Lisp image may
;; lack.  Straight from the standards; fast enough for packs of some
;; tens of megabytes.
;;

(deftype u32 () '(unsigned-byte 32))

(declaim (inline u32+ rotl32))

(defun u32+ (&rest xs)
  (logand #xffffffff (apply #'+ xs)))

(defun rotl32 (x n)
  (declare (type u32 x) (type (integer 0 32) n))
  (logand #xffffffff (logior (ash x n) (ash x (- n 32)))))

(defun pad-message (octets block-bits)
  "OCTETS with the standard padding: a 1 bit, zeros, and the length in
bits as a big-endian 64-bit number, to a multiple of 64 bytes."
  (declare (ignore block-bits))
  (let* ((n (length octets))
         (bits (* 8 n))
         (total (* 64 (ceiling (+ n 9) 64)))
         (out (make-octets total)))
    (replace out octets)
    (setf (aref out n) #x80)
    (loop for i from 0 below 8
          do (setf (aref out (- total 1 i)) (ldb (byte 8 (* 8 i)) bits)))
    out))

;;; SHA-1

(defun sha1-digest (octets)
  "The 20 octets of the SHA-1 of OCTETS."
  (let ((padded (pad-message (as-octets octets) 512))
        (h0 #x67452301) (h1 #xEFCDAB89) (h2 #x98BADCFE) (h3 #x10325476) (h4 #xC3D2E1F0)
        (w (make-array 80 :element-type 'u32 :initial-element 0)))
    (declare (type u32 h0 h1 h2 h3 h4))
    (loop for block from 0 below (length padded) by 64
          do (dotimes (i 16)
               (setf (aref w i) (be32 padded (+ block (* 4 i)))))
             (loop for i from 16 below 80
                   do (setf (aref w i) (rotl32 (logxor (aref w (- i 3)) (aref w (- i 8)) (aref w (- i 14)) (aref w (- i 16))) 1)))
             (let ((a h0) (b h1) (c h2) (d h3) (e h4))
               (declare (type u32 a b c d e))
               (dotimes (i 80)
                 (multiple-value-bind (f k)
                     (cond ((< i 20) (values (logior (logand b c) (logand (logxor b #xffffffff) d)) #x5A827999))
                           ((< i 40) (values (logxor b c d) #x6ED9EBA1))
                           ((< i 60) (values (logior (logand b c) (logand b d) (logand c d)) #x8F1BBCDC))
                           (t (values (logxor b c d) #xCA62C1D6)))
                   (let ((temp (u32+ (rotl32 a 5) f e k (aref w i))))
                     (setq e d d c c (rotl32 b 30) b a a temp))))
               (setq h0 (u32+ h0 a) h1 (u32+ h1 b) h2 (u32+ h2 c) h3 (u32+ h3 d) h4 (u32+ h4 e))))
    (let ((out (make-octets 20)))
      (put-be32 out 0 h0) (put-be32 out 4 h1) (put-be32 out 8 h2) (put-be32 out 12 h3) (put-be32 out 16 h4)
      out)))

;;; SHA-256

(defparameter *sha256-k*
  (make-array 64 :element-type 'u32 :initial-contents
              '(#x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5 #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
                #xd807aa98 #x12835b01 #x243185be #x550c7dc3 #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
                #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
                #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7 #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
                #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13 #x650a7354 #x766a0abb #x81c2c92e #x92722c85
                #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3 #xd192e819 #xd6990624 #xf40e3585 #x106aa070
                #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5 #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
                #x748f82ee #x78a5636f #x84c87814 #x8cc70208 #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2)))

(declaim (inline rotr32))
(defun rotr32 (x n)
  (declare (type u32 x) (type (integer 0 32) n))
  (logand #xffffffff (logior (ash x (- n)) (ash x (- 32 n)))))

(defun sha256-digest (octets)
  "The 32 octets of the SHA-256 of OCTETS."
  (let ((padded (pad-message (as-octets octets) 512))
        (h (make-array 8 :element-type 'u32
                         :initial-contents '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
                                             #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))
        (w (make-array 64 :element-type 'u32 :initial-element 0))
        (k *sha256-k*))
    (loop for block from 0 below (length padded) by 64
          do (dotimes (i 16) (setf (aref w i) (be32 padded (+ block (* 4 i)))))
             (loop for i from 16 below 64
                   do (let* ((w15 (aref w (- i 15))) (w2 (aref w (- i 2)))
                             (s0 (logxor (rotr32 w15 7) (rotr32 w15 18) (ash w15 -3)))
                             (s1 (logxor (rotr32 w2 17) (rotr32 w2 19) (ash w2 -10))))
                        (setf (aref w i) (u32+ (aref w (- i 16)) s0 (aref w (- i 7)) s1))))
             (let ((a (aref h 0)) (b (aref h 1)) (c (aref h 2)) (d (aref h 3))
                   (e (aref h 4)) (f (aref h 5)) (g (aref h 6)) (hh (aref h 7)))
               (declare (type u32 a b c d e f g hh))
               (dotimes (i 64)
                 (let* ((s1 (logxor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
                        (ch (logxor (logand e f) (logand (logxor e #xffffffff) g)))
                        (t1 (u32+ hh s1 ch (aref k i) (aref w i)))
                        (s0 (logxor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
                        (maj (logxor (logand a b) (logand a c) (logand b c)))
                        (t2 (u32+ s0 maj)))
                   (setq hh g g f f e e (u32+ d t1) d c c b b a a (u32+ t1 t2))))
               (setf (aref h 0) (u32+ (aref h 0) a) (aref h 1) (u32+ (aref h 1) b)
                     (aref h 2) (u32+ (aref h 2) c) (aref h 3) (u32+ (aref h 3) d)
                     (aref h 4) (u32+ (aref h 4) e) (aref h 5) (u32+ (aref h 5) f)
                     (aref h 6) (u32+ (aref h 6) g) (aref h 7) (u32+ (aref h 7) hh))))
    (let ((out (make-octets 32)))
      (dotimes (i 8 out) (put-be32 out (* 4 i) (aref h i))))))

;;; HMAC and PBKDF2, over SHA-256

(defun hmac-sha256 (key message)
  (let* ((key (as-octets key))
         (key (if (> (length key) 64) (sha256-digest key) key))
         (padded (make-octets 64)))
    (replace padded key)
    (let ((ipad (make-octets 64)) (opad (make-octets 64)))
      (dotimes (i 64)
        (setf (aref ipad i) (logxor (aref padded i) #x36)
              (aref opad i) (logxor (aref padded i) #x5c)))
      (sha256-digest (octets-concat opad (sha256-digest (octets-concat ipad (as-octets message))))))))

(defun pbkdf2-sha256 (password salt iterations &optional (length 32))
  "LENGTH octets derived from PASSWORD (a string or octets) and SALT."
  (let ((password (if (stringp password) (string-to-octets password) (as-octets password)))
        (salt (as-octets salt))
        (out (make-buffer length))
        (block 1))
    (loop while (< (fill-pointer out) length)
          do (let* ((u (hmac-sha256 password (octets-concat salt (be32-octets block))))
                    (acc (copy-seq u)))
               (loop repeat (1- iterations)
                     do (setq u (hmac-sha256 password u))
                        (dotimes (i (length acc)) (setf (aref acc i) (logxor (aref acc i) (aref u i)))))
               (buffer-write out acc)
               (incf block)))
    (suboctets out 0 length)))

;;; CRC-32

(defparameter *crc32-table*
  (let ((table (make-array 256 :element-type 'u32)))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setq c (if (logbitp 0 c) (logxor #xEDB88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (octets)
  (let ((c #xffffffff) (table *crc32-table*))
    (declare (type u32 c))
    (loop for byte across octets
          do (setq c (logxor (aref table (logand (logxor c byte) #xff)) (ash c -8))))
    (logxor c #xffffffff)))
