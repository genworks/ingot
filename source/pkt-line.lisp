;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; pkt-line, the framing of the smart protocol: four hex digits of
;; length (the four included), then the bytes; "0000" is a flush,
;; "0001" a delimiter.  Readers work on an octet vector and an index;
;; writers append to a buffer.
;;

(defconstant +pkt-flush+ :flush)
(defconstant +pkt-delim+ :delim)

(defun read-pkt-line (octets at)
  "Two values: the next pkt-line in OCTETS from AT -- its payload as
octets, :flush, :delim, or nil at the end of OCTETS -- and the index
after it."
  (if (> (+ at 4) (length octets))
      (values nil (length octets))
      (let ((n (parse-integer (octets-to-string octets :start at :end (+ at 4)) :radix 16)))
        (case n
          (0 (values :flush (+ at 4)))
          (1 (values :delim (+ at 4)))
          (t (when (or (< n 4) (> (+ at n) (length octets)))
               (git-error "A pkt-line of ~d bytes runs past the request." n))
             (values (suboctets octets (+ at 4) (+ at n)) (+ at n)))))))

(defun read-pkt-lines (octets &key (start 0) end)
  "Two values: the pkt-lines in OCTETS from START up to the first flush
(each payload as octets; a :delim stays as :delim), and the index
after the flush (or the end)."
  (let ((end (or end (length octets)))
        (lines nil))
    (loop
      (when (>= start end) (return))
      (multiple-value-bind (line next) (read-pkt-line octets start)
        (setq start next)
        (cond ((null line) (return))
              ((eq line :flush) (return))
              (t (push line lines)))))
    (values (nreverse lines) start)))

(defun pkt-text (line)
  "A pkt-line's payload as a string without its trailing newline."
  (let ((text (octets-to-string line)))
    (string-right-trim '(#\Newline) text)))

(defun write-pkt-line (buffer payload)
  "Append PAYLOAD (a string or octets) to BUFFER as one pkt-line."
  (let ((octets (if (stringp payload) (string-to-octets payload) payload)))
    (buffer-write buffer (ascii (format nil "~(~4,'0x~)" (+ 4 (length octets)))))
    (buffer-write buffer octets)
    buffer))

(defun write-pkt-flush (buffer)
  (buffer-write buffer (ascii "0000")))

(defun write-pkt-delim (buffer)
  (buffer-write buffer (ascii "0001")))

(defun pkt-line (payload)
  "One pkt-line as octets."
  (buffer-octets (write-pkt-line (make-buffer 64) payload)))

(defun pkt-flush () (ascii "0000"))
(defun pkt-delim () (ascii "0001"))

;;; Side band: the pack goes out as band 1 in frames of at most 65515
;;; bytes of payload; band 2 is progress, band 3 an error.

(defparameter *side-band-frame* 65515)

(defun write-side-band (buffer band octets)
  "Append OCTETS to BUFFER as side-band frames on BAND (1, 2 or 3)."
  (loop for start from 0 below (length octets) by *side-band-frame*
        for end = (min (length octets) (+ start *side-band-frame*))
        do (buffer-write buffer (ascii (format nil "~(~4,'0x~)" (+ 5 (- end start)))))
           (buffer-byte buffer band)
           (buffer-write buffer octets :start start :end end))
  buffer)
