;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :ingot)

;;
;; zlib, in and out: chipz inflates, salza2 deflates.  Loose objects
;; and every entry of a pack are zlib streams; a pack read without an
;; index has to know where each stream ends, which `inflate-at' says.
;;

(defun inflate (octets &key (start 0) end)
  "The zlib stream in OCTETS from START, inflated."
  (chipz:decompress nil :zlib octets :input-start start :input-end (or end (length octets))))

(defun inflate-at (octets start expected-size)
  "Two values: the EXPECTED-SIZE octets of the zlib stream that begins
at START in OCTETS, and the index just past that stream."
  (let ((out (make-octets expected-size))
        (state (chipz:make-dstate :zlib)))
    (multiple-value-bind (consumed produced)
        (chipz:decompress out state octets :input-start start)
      (unless (= produced expected-size)
        (git-error "A compressed object of ~d bytes inflated to ~d." expected-size produced))
      (values out (+ start consumed)))))

(defparameter *empty-zlib-stream* (octets #x78 #x9c #x03 #x00 #x00 #x00 #x00 #x01)
  "The zlib stream of no bytes at all: salza2 answers a truncated one for
empty input, which neither zlib nor chipz accept, and a pack holding the
empty blob was unreadable (found on gendl, 2026-10-08).")

(defun deflate (octets)
  "OCTETS as a zlib stream."
  (if (zerop (length octets))
      (copy-seq *empty-zlib-stream*)
      (salza2:compress-data octets 'salza2:zlib-compressor)))

(defun gzip (octets)
  "OCTETS as a gzip file."
  (salza2:compress-data octets 'salza2:gzip-compressor))

(defun gunzip (octets)
  (chipz:decompress nil :gzip octets))
