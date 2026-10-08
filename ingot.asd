;;;; -*- coding: utf-8 -*-

;;; Written by hand: Ingot is plain Common Lisp and is built with ASDF
;;; alone, so that it loads into any Lisp without Gendl.  Add a new file
;;; here, in load order.

(asdf:defsystem #:ingot
  :description "Git in Common Lisp: objects, packs, refs, trees, commits, diffs, merges, the smart protocol's server side, archives.  No subprocess, no git binary."
  :author "Genworks International"
  :license "AGPL-3.0-or-later"
  :version "0.1.0"
  :serial t
  :depends-on (:chipz :salza2 :babel :bordeaux-threads)
  :components ((:file "source/package")
               (:file "source/octets")
               (:file "source/digest")
               (:file "source/zlib")
               (:file "source/pkt-line")
               (:file "source/objects")
               (:file "source/loose")
               (:file "source/pack")
               (:file "source/refs")
               (:file "source/repository")
               (:file "source/write")
               (:file "source/diff")
               (:file "source/merge")
               (:file "source/packwriter")
               (:file "source/protocol")
               (:file "source/archive")))
