;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

(in-package :cl-user)

;;
;; ingot: Git in Common Lisp.  The object store (loose and packed),
;; refs, trees, commits, diffs, merges, the smart protocol on the
;; server side and the pack a fetch answers with, and archives of a
;; tree.  Nothing here serves HTTP or knows a user; the forge above it
;; does.  Nothing here starts a subprocess.
;;

(defpackage :ingot
  (:use :common-lisp)
  (:export
   ;; octets
   #:octets #:make-octets #:octets-to-string #:string-to-octets #:octets-concat
   #:hex #:unhex #:sha1-hex #:sha1-octets #:oid? #:read-file-octets #:write-file-octets
   ;; zlib
   #:inflate #:inflate-at #:deflate #:gzip
   ;; pkt-line
   #:pkt-line #:pkt-flush #:pkt-delim #:read-pkt-lines #:write-pkt-line #:write-pkt-flush
   ;; objects
   #:git-object #:object-type #:object-data #:object-oid #:object-size
   #:tree-entry #:tree-entry-mode #:tree-entry-name #:tree-entry-oid #:tree-entry-kind
   #:parse-tree #:serialize-tree #:parse-commit #:serialize-commit #:parse-tag
   #:commit-tree #:commit-parents #:commit-author #:commit-committer #:commit-message
   #:commit-oid #:commit-author-name #:commit-author-email #:commit-author-time
   #:commit-committer-time #:commit-subject #:tag-object #:tag-type #:tag-name #:tag-tagger #:tag-message
   #:identity-string #:parse-identity #:make-identity #:ident #:ident-name #:ident-email #:ident-time #:ident-zone
   #:hash-object #:object-header #:make-git-object
   ;; repository
   #:repository #:open-repository #:init-repository #:repository-path #:repository-exists?
   #:find-object #:object-exists? #:read-object #:read-blob #:read-tree #:read-commit #:peel-to-commit
   #:write-object #:write-blob #:write-tree #:write-commit
   #:ref-value #:ref-value* #:refs #:branches #:tags #:head-ref #:head-oid #:default-branch #:set-default-branch!
   #:update-ref! #:delete-ref! #:resolve #:resolve-commit #:short-oid #:expand-short-oid #:valid-ref-name?
   #:*zero-oid* #:fast-forward? #:make-ref-command #:ref-command #:ref-command-name #:ref-command-old
   #:ref-command-new #:ref-command-status #:ref-command-reason
   #:commit-diff #:change #:change-status #:change-path #:change-old-oid #:change-new-oid #:change-old-mode #:change-new-mode
   #:split-lines #:tag-message #:tag-tagger #:merge-conflict #:merge-conflict-paths #:last-path-part
   #:sha1-digest #:sha256-digest #:hmac-sha256 #:pbkdf2-sha256 #:crc32 #:gunzip #:file-octets-if-exists
   #:make-buffer #:buffer-write #:buffer-octets #:buffer-byte #:as-octets #:suboctets #:ascii #:be32
   #:tree-entry-at #:tree-at-path #:walk-tree #:tree-paths #:blob-at
   #:log-commits #:merge-base #:ancestor? #:reachable-objects #:count-commits
   #:repository-lock #:with-repository-lock
   ;; writing
   #:apply-actions #:make-commit!
   ;; diff
   #:tree-diff #:diff-lines #:unified-diff #:binary-octets? #:diff-stat
   ;; merge
   #:merge-trees #:merge-text #:merge-branches! #:merge-conflict #:can-merge?
   ;; pack
   #:write-pack #:index-pack #:store-pack! #:*store-packs?*
   ;; protocol
   #:advertise-refs #:upload-pack #:receive-pack #:parse-receive-commands
   ;; archive
   #:tar-tree #:tar-gz-tree #:zip-tree #:tar-entries #:tar-append-entry #:tar-find #:untar
   ;; conditions
   #:git-error #:not-found #:ref-conflict))
