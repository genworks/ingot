;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; Ingot's checks: load into an image that has the system loaded.  A
;;; repository is made in a scratch directory and objects, refs,
;;; commits, diffs, merges, packs, archives and both sides of the
;;; protocol are worked, with no git binary anywhere.  INGOT_REPO names
;;; an existing repository to read as well.  One line per check; with
;;; INGOT_CI or ANVIL_CI set the Lisp leaves with status 0 or 1, else
;;; a failure signals at the end.
;;;

(in-package :cl-user)

(defvar *ingot-failures* nil)
(defvar *ingot-checks* 0)

(defmacro ingot-check (name &body body)
  `(let ((ok? (handler-case (progn ,@body)
                (error (condition) (format t "~&      ~a~%" condition) nil))))
     (incf *ingot-checks*)
     (format t "~&~a ~a~%" (if ok? "ok  " "FAIL") ,name)
     (unless ok? (push ,name *ingot-failures*))
     ok?))

(defun ingot-scratch-directory ()
  (merge-pathnames (format nil "ingot-test-~a/" (random 1000000000))
                   (uiop:temporary-directory)))

(let* ((root (ingot-scratch-directory))
       (repo nil) (other nil)
       (who (ingot:make-identity "Test Person" "test@example.org" :time 1700000000))
       (c1 nil) (c2 nil))
  (setq *ingot-failures* nil *ingot-checks* 0)
  (unwind-protect
       (progn
         (ingot-check "hashing: the empty blob and a known string"
           (and (string= (ingot:hash-object :blob (ingot:make-octets 0)) "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")
                (string= (ingot:hash-object :blob (ingot:string-to-octets "hello")) "b6fc4c620b67d95f953a5c1c1230aaab5db5a1b0")))
         (ingot-check "pkt-line round trip"
           (let* ((buffer (ingot::make-buffer))
                  (octets (progn (ingot:write-pkt-line buffer "hello") (ingot:write-pkt-flush buffer)
                                 (ingot:write-pkt-line buffer (ingot:string-to-octets "world"))
                                 (ingot::buffer-octets buffer))))
             (multiple-value-bind (lines at) (ingot:read-pkt-lines octets)
               (and (= (length lines) 1) (string= (ingot::pkt-text (first lines)) "hello")
                    (= at 13)
                    (string= (ingot::pkt-text (ingot::read-pkt-line octets at)) "world")))))
         (ingot-check "a bare repository is made"
           (setq repo (ingot:init-repository (merge-pathnames "one.git/" root)))
           (and (ingot:repository-exists? (merge-pathnames "one.git/" root))
                (string= (ingot:default-branch repo) "master")
                (null (ingot:head-oid repo))))
         (ingot-check "a first commit from actions"
           (setq c1 (ingot:make-commit! repo :branch "master" :author who :message "First"
                                            :actions '((:action :create :path "README.md" :content "# Hello

A line.
")
                                                       (:action :create :path "src/a.lisp" :content "(defun a () 1)
")
                                                       (:action :create :path "src/b.lisp" :content "(defun b () 2)
"))))
           (and (ingot:oid? c1)
                (string= (ingot:head-oid repo) c1)
                (= (length (ingot:tree-paths repo (ingot:commit-tree (ingot:read-commit repo c1)))) 3)
                (string= (ingot:octets-to-string (ingot:blob-at repo c1 "src/a.lisp")) "(defun a () 1)
")))
         (ingot-check "a second commit: update, delete, move"
           (setq c2 (ingot:make-commit! repo :branch "master" :author who :message "Second"
                                            :actions '((:action :update :path "README.md" :content "# Hello

A line.
Another line.
")
                                                       (:action :delete :path "src/b.lisp")
                                                       (:action :move :previous-path "src/a.lisp" :path "lib/a.lisp"))))
           (let ((paths (mapcar #'car (ingot:tree-paths repo (ingot:commit-tree (ingot:read-commit repo c2))))))
             (and (equal paths '("README.md" "lib/a.lisp"))
                  (equal (ingot:commit-parents (ingot:read-commit repo c2)) (list c1)))))
         (ingot-check "the log, newest first"
           (equal (mapcar #'ingot:commit-subject (ingot:log-commits repo (ingot:head-oid repo)))
                  '("Second" "First")))
         (ingot-check "the log of one path"
           (equal (mapcar #'ingot:commit-subject (ingot:log-commits repo (ingot:head-oid repo) :path "src/b.lisp"))
                  '("Second" "First")))
         (ingot-check "the tree diff of the second commit"
           (let ((changes (ingot::commit-diff repo c2)))
             (equal (mapcar (lambda (c) (list (ingot::change-status c) (ingot::change-path c))) changes)
                    '((:modified "README.md") (:added "lib/a.lisp") (:deleted "src/a.lisp") (:deleted "src/b.lisp")))))
         (ingot-check "a unified diff"
           (let ((text (ingot:unified-diff "a
b
c
d
e
f
g
" "a
b
c
d
X
f
g
h
" :old-name "old" :new-name "new")))
             (and (search "@@ -2,6 +2,7 @@" text) (search "-e" text) (search "+X" text) (search "+h" text))))
         (ingot-check "a short id resolves, a ref name resolves"
           (and (string= (ingot:resolve repo (subseq c2 0 7)) c2)
                (string= (ingot:resolve repo "master") c2)
                (string= (ingot:resolve repo "refs/heads/master") c2)
                (null (ingot:resolve repo "nothing-here"))))
         (ingot-check "refs: create, compare-and-swap, delete"
           (ingot:update-ref! repo "refs/heads/topic" c1 :old nil)
           (let ((conflict? (handler-case (progn (ingot:update-ref! repo "refs/heads/topic" c2 :old "0000000000000000000000000000000000000000") nil)
                              (ingot:ref-conflict () t))))
             (and conflict?
                  (string= (cdr (assoc "topic" (ingot:branches repo) :test #'string=)) c1)
                  (ingot:delete-ref! repo "refs/heads/topic")
                  (null (assoc "topic" (ingot:branches repo) :test #'string=)))))
         (ingot-check "a fast-forward merge"
           (ingot:update-ref! repo "refs/heads/ff" c1 :old nil)
           (and (eq (ingot:can-merge? repo c2 c1) :fast-forward)
                (string= (ingot:merge-branches! repo "master" "ff" :author who) c2)))
         (ingot-check "a three-way merge without conflict"
           (ingot:update-ref! repo "refs/heads/left" c2 :old nil)
           (ingot:update-ref! repo "refs/heads/right" c2 :old nil)
           (ingot:make-commit! repo :branch "left" :author who :message "Left"
                                   :actions '((:action :update :path "README.md" :content "# Hello, left

A line.
Another line.
")))
           (ingot:make-commit! repo :branch "right" :author who :message "Right"
                                   :actions '((:action :update :path "README.md" :content "# Hello

A line.
Another line.
Right at the end.
")
                                              (:action :create :path "right.txt" :content "right
")))
           (let* ((left (ingot:resolve repo "left")) (right (ingot:resolve repo "right")))
             (and (eq (ingot:can-merge? repo right left) :merge)
                  (let ((merged (ingot:merge-branches! repo "right" "left" :author who)))
                    (and (= (length (ingot:commit-parents (ingot:read-commit repo merged))) 2)
                         (string= (ingot:octets-to-string (ingot:blob-at repo merged "README.md"))
                                  "# Hello, left

A line.
Another line.
Right at the end.
")
                         (ingot:blob-at repo merged "right.txt"))))))
         (ingot-check "a three-way merge with a conflict"
           (ingot:update-ref! repo "refs/heads/c-left" c2 :old nil)
           (ingot:update-ref! repo "refs/heads/c-right" c2 :old nil)
           (ingot:make-commit! repo :branch "c-left" :author who :message "Left"
                                   :actions '((:action :update :path "README.md" :content "# Left

A line.
Another line.
")))
           (ingot:make-commit! repo :branch "c-right" :author who :message "Right"
                                   :actions '((:action :update :path "README.md" :content "# Right

A line.
Another line.
")))
           (and (eq (ingot:can-merge? repo (ingot:resolve repo "c-right") (ingot:resolve repo "c-left")) :conflict)
                (handler-case (progn (ingot:merge-branches! repo "c-right" "c-left" :author who) nil)
                  (ingot::merge-conflict (c) (equal (ingot::merge-conflict-paths c) '("README.md"))))))
         (ingot-check "a tar of the tree"
           (let ((tar (ingot::buffer-octets (ingot:tar-tree repo (ingot:commit-tree (ingot:read-commit repo c2)) :prefix "one/"))))
             (and (zerop (mod (length tar) 512))
                  (search (ingot:string-to-octets "one/lib/a.lisp") tar)
                  (search (ingot:string-to-octets "(defun a () 1)") tar))))
         (ingot-check "a zip of the tree"
           (let ((zip (ingot:zip-tree repo (ingot:commit-tree (ingot:read-commit repo c2)) :prefix "one/")))
             (and (= (aref zip 0) #x50) (= (aref zip 1) #x4b)
                  (search (ingot:string-to-octets "one/README.md") zip))))
         (ingot-check "a pack written and indexed again"
           (let* ((oids (ingot:reachable-objects repo (list c2) nil))
                  (pack (ingot::buffer-octets (ingot:write-pack repo oids)))
                  (objects (ingot:index-pack pack (lambda (oid) (declare (ignore oid)) nil))))
             (and (= (length oids) 10)  ; 2 commits, 4 trees (root, src, root', lib), 4 blobs
                  (= (length objects) (length oids))
                  (every (lambda (object oid) (string= (ingot:object-oid object) oid)) objects oids))))
         (ingot-check "a delta applies"
           ;; a hand-made delta: copy 5 bytes from offset 0 of the base, insert "XY"
           (let* ((base (ingot:string-to-octets "hello world"))
                  (delta (ingot::octets 11 7 #x90 5 2 (char-code #\X) (char-code #\Y))))
             (string= (ingot:octets-to-string (ingot::apply-delta base delta)) "helloXY")))
         (ingot-check "the empty blob survives a pack: deflate of nothing is a whole stream"
           (let* ((empty (ingot:write-blob repo (ingot:make-octets 0)))
                  (pack (ingot::buffer-octets (ingot:write-pack repo (list empty c2))))
                  (objects (ingot:index-pack pack (constantly nil))))
             (and (string= empty "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")
                  (= (length objects) 2)
                  (= 8 (length (ingot::deflate (ingot:make-octets 0))))
                  (zerop (length (ingot::inflate (ingot::deflate (ingot:make-octets 0)))))
                  (find empty objects :key #'ingot:object-oid :test #'string=))))
         (ingot-check "an ofs-delta distance round trip"
           (every (lambda (n)
                    (let ((b (ingot::make-buffer)))
                      (ingot::write-ofs-delta-distance b n)
                      (let ((at 0) (octets (ingot::buffer-octets b)))
                        (= n (ingot::read-ofs-delta-distance (lambda () (prog1 (aref octets at) (incf at))))))))
                  '(0 1 127 128 129 255 256 16383 16384 16511 16512 2097151 2097152 300000000)))
         (ingot-check "a pack index written and read again"
           (let* ((oids (ingot:reachable-objects repo (list c2) nil))
                  (entries (loop for oid in oids for i from 0
                                 collect (list (ingot:unhex oid) (* 7 i) (+ 12 (* 100 i)))))
                  (sha (ingot:make-octets 20 7))
                  (idx (merge-pathnames "written.idx" root))
                  (pack (merge-pathnames "written.pack" root)))
             (ingot:write-file-octets idx (ingot::write-pack-index-octets entries sha))
             (ingot:write-file-octets pack (ingot:make-octets 32))
             (let ((index (ingot::read-pack-index idx pack)))
               (and (= (ingot::pack-index-count index) (length oids))
                    (every (lambda (entry)
                             (let ((i (ingot::index-find index (ingot:hex (first entry)))))
                               (and i (= (ingot::index-offset-at index i) (third entry)))))
                           entries)
                    (null (ingot::index-find index (make-string 40 :initial-element #\f)))))))
         (ingot-check "receive-pack into an empty repository, then upload-pack out of it"
           (setq other (ingot:init-repository (merge-pathnames "two.git/" root)))
           (let* ((oids (ingot:reachable-objects repo (list c2) nil))
                  (pack (ingot::buffer-octets (ingot:write-pack repo oids)))
                  (request (ingot::buffer-octets
                            (let ((b (ingot::make-buffer)))
                              (ingot:write-pkt-line b (format nil "~a ~a refs/heads/master~a report-status side-band-64k~%"
                                                             ingot::*zero-oid* c2 (code-char 0)))
                              (ingot:write-pkt-flush b)
                              (ingot::buffer-write b pack)
                              b))))
             (multiple-value-bind (response commands) (ingot:receive-pack other request)
               (and (= (length commands) 1)
                    (eq (ingot::ref-command-status (first commands)) :ok)
                    (search (ingot:string-to-octets "unpack ok") response)
                    (string= (ingot:head-oid other) c2)
                    (string= (ingot:octets-to-string (ingot:blob-at other c2 "lib/a.lisp")) "(defun a () 1)
")
                    ;; now fetch it back: a clone's request
                    (let* ((advert (ingot:octets-to-string (ingot:advertise-refs other :upload-pack)))
                           (fetch (ingot::buffer-octets
                                   (let ((b (ingot::make-buffer)))
                                     (ingot:write-pkt-line b (format nil "want ~a multi_ack_detailed side-band-64k~%" c2))
                                     (ingot:write-pkt-flush b)
                                     (ingot:write-pkt-line b (format nil "done~%"))
                                     b)))
                           (answer (ingot:upload-pack other fetch)))
                      (and (search "# service=git-upload-pack" advert)
                           (search (format nil "~a HEAD" c2) advert)
                           (search "symref=HEAD:refs/heads/master" advert)
                           ;; NAK, then the pack on band 1
                           (string= (ingot::pkt-text (ingot::read-pkt-line answer 0)) "NAK")
                           (let ((received (ingot::make-buffer)) (at 8))
                             (loop
                               (multiple-value-bind (line next) (ingot::read-pkt-line answer at)
                                 (setq at next)
                                 (cond ((or (null line) (eq line :flush)) (return))
                                       ((= (aref line 0) 1) (ingot::buffer-write received line :start 1)))))
                             (= (length (ingot:index-pack (ingot::buffer-octets received) (constantly nil)))
                                (length oids)))))))))
         (ingot-check "the push was kept as a pack with its index, and reads from it"
           (let ((packs (uiop:directory-files (merge-pathnames "two.git/objects/pack/" root) "*.pack"))
                 (idxs (uiop:directory-files (merge-pathnames "two.git/objects/pack/" root) "*.idx")))
             (and (= (length packs) 1) (= (length idxs) 1)
                  (not (ingot::loose-object-exists? (ingot::repository-path other) c2))
                  (let ((object (ingot::find-packed-object other c2)))
                    (and object (eq (ingot:object-type object) :commit))))))
         (ingot-check "a push that is not a fast-forward is refused by the check"
           (let* ((request (ingot::buffer-octets
                            (let ((b (ingot::make-buffer)))
                              (ingot:write-pkt-line b (format nil "~a ~a refs/heads/master~a report-status~%" c2 c1 (code-char 0)))
                              (ingot:write-pkt-flush b)
                              b))))
             (multiple-value-bind (response commands)
                 (ingot:receive-pack other request
                                    :check (lambda (command)
                                             (unless (ingot::fast-forward? other (ingot::ref-command-old command) (ingot::ref-command-new command))
                                               "non-fast-forward")))
               (and (eq (ingot::ref-command-status (first commands)) :ng)
                    (search (ingot:string-to-octets "ng refs/heads/master non-fast-forward") response)
                    (string= (ingot:head-oid other) c2)))))
         (ingot-check "a branch deleted by push"
           (ingot:update-ref! other "refs/heads/gone" c1 :old nil)
           (let* ((request (ingot::buffer-octets
                            (let ((b (ingot::make-buffer)))
                              (ingot:write-pkt-line b (format nil "~a ~a refs/heads/gone~a report-status~%" c1 ingot::*zero-oid* (code-char 0)))
                              (ingot:write-pkt-flush b)
                              b))))
             (ingot:receive-pack other request)
             (null (assoc "gone" (ingot:branches other) :test #'string=))))
         (ingot-check "merge-text: both sides change different places"
           (multiple-value-bind (text conflict?)
               (ingot:merge-text "a
b
c
" "A
b
c
" "a
b
C
")
             (and (not conflict?) (string= text "A
b
C
"))))
         (ingot-check "a tree sorts as git sorts: a shorter name first, a subtree as name/"
           (flet ((blob (text) (ingot:write-blob repo (format nil "~a~%" text)))
                  (entry (mode name oid) (ingot::make-tree-entry :mode mode :name name :oid oid)))
             (let* ((sub (ingot:write-tree repo (list (entry "100644" "x" (blob "a/x")))))
                    (tree (ingot:write-tree repo (list (entry "100644" "a.txt" (blob "a.txt"))
                                                       (entry "40000" "a" sub)
                                                       (entry "100644" "Makefile.in" (blob "Makefile.in"))
                                                       (entry "100644" "Makefile" (blob "Makefile"))
                                                       (entry "100644" ".hidden" (blob ".hidden"))
                                                       (entry "100644" "a-b" (blob "a-b"))))))
               (and (string= sub "34c2d24ff4bd6c52f81983e24f62425c4b3e07d6")
                    (string= tree "b0cebd6f42c7f5db82eab1095677933b95711c8d")))))
         (ingot-check "SHA-1, SHA-256 and CRC-32 against known values, across the block boundaries"
           (flet ((as (n) (ingot:string-to-octets (make-string n :initial-element #\a))))
             (and (string= (ingot:sha1-hex (ingot:string-to-octets "abc")) "a9993e364706816aba3e25717850c26c9cd0d89d")
                  (string= (ingot:hex (ingot:sha256-digest (ingot:string-to-octets "abc")))
                           "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
                  (every (lambda (n sha1 sha256)
                           (and (string= (ingot:sha1-hex (as n)) sha1)
                                (string= (ingot:hex (ingot:sha256-digest (as n))) sha256)))
                         '(55 56 63 64 119 120)
                         '("c1c8bbdc22796e28c0e15163d20899b65621d65a" "c2db330f6083854c99d4b5bfb6e8f29f201be699"
                           "03f09f5b158a7a8cdad920bddc29b81c18a551f5" "0098ba824b5c16427bd7a1122a5a442a25ec644d"
                           "ee971065aaa017e0632a8ca6c77bb3bf8b1dfc56" "f34c1488385346a55709ba056ddd08280dd4c6d6")
                         '("9f4390f8d30c2dd92ec9f095b65e2b9ae9b0a925a5258e241c9f1e910f734318"
                           "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a"
                           "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34"
                           "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb"
                           "31eba51c313a5c08226adf18d4a359cfdfd8d2e816b13f4af952f7ea6584dcfb"
                           "2f3d335432c70b580af0e8e1b3674a7c020d683aa5f73aaaedfdc55af904c21c"))
                  (= (ingot:crc32 (ingot:string-to-octets "hello")) #x3610a686)
                  (zerop (ingot:crc32 (ingot:make-octets 0))))))
         (ingot-check "a signed commit reads and writes back byte for byte"
           (let* ((text (format nil "tree ~a~%parent ~a~%author A <a@x> 1700000000 +0100~%committer B <b@x> 1700000001 -0500~%gpgsig -----BEGIN PGP SIGNATURE-----~% ~% iQEzBAABCAAdFiEE~% =abcd~% -----END PGP SIGNATURE-----~%~%Signed~%~%With a body.~%"
                                (ingot:commit-tree (ingot:read-commit repo c1)) c1))
                  (octets (ingot:string-to-octets text))
                  (commit (ingot:parse-commit octets)))
             (and (equal (ingot:commit-parents commit) (list c1))
                  (string= (ingot:commit-subject commit) "Signed")
                  (string= (ingot:ident-zone (ingot:commit-committer commit)) "-0500")
                  (= 1 (length (ingot::commit-headers commit)))
                  (ingot::octets= (ingot:serialize-commit commit) octets))))
         (ingot-check "an annotated tag: written, listed, peeled, advertised peeled, found by include-tag"
           (let* ((tag (ingot::make-tag :object c2 :type :commit :name "v1" :tagger who :message (format nil "Version one~%")))
                  (oid (ingot:write-object repo :tag (ingot::serialize-tag tag))))
             (ingot:update-ref! repo "refs/tags/v1" oid :old nil)
             (let ((advert (ingot:octets-to-string (ingot:advertise-refs repo :upload-pack)))
                   (back (ingot:parse-tag (ingot:object-data (ingot:read-object repo oid)) oid)))
               (and (string= (cdr (assoc "v1" (ingot:tags repo) :test #'string=)) oid)
                    (string= (ingot:peel-to-commit repo oid) c2)
                    (string= (ingot:resolve-commit repo "v1") c2)
                    (string= (ingot:tag-name back) "v1")
                    (string= (ingot:ident-email (ingot:tag-tagger back)) "test@example.org")
                    (search (format nil "~a refs/tags/v1^{}" c2) advert)
                    (member oid (ingot::tags-pointing-into repo (list c2)) :test #'string=)))))
         (ingot-check "packed-refs: read, shadowed by a loose ref, the peeled line kept across a delete"
           (let ((gitdir (ingot::repository-path repo)))
             (ingot:write-file-octets (merge-pathnames "packed-refs" gitdir)
                                      (ingot:string-to-octets (format nil "# pack-refs with: peeled fully-peeled sorted ~%~a refs/heads/packed-only~%~a refs/heads/master~%~a refs/tags/packed-tag~%^~a~%" c1 c1 c1 c2)))
             (and (string= (ingot:ref-value* repo "refs/heads/packed-only") c1)
                  (string= (ingot:ref-value* repo "refs/heads/master") c2)
                  (progn (ingot:delete-ref! repo "refs/heads/packed-only")
                         (null (ingot:ref-value* repo "refs/heads/packed-only")))
                  (string= (ingot:ref-value* repo "refs/tags/packed-tag") c1)
                  (search "^" (ingot:octets-to-string (ingot:read-file-octets (merge-pathnames "packed-refs" gitdir))))
                  (progn (ingot:delete-ref! repo "refs/tags/packed-tag")
                         (null (ingot:ref-value* repo "refs/tags/packed-tag"))))))
         (ingot-check "ref names git would refuse"
           (and (every #'ingot:valid-ref-name? '("refs/heads/master" "refs/heads/a/b-c" "refs/tags/v1.0"))
                (notany #'ingot:valid-ref-name? '("" "refs/heads/" "refs/heads/.hidden" "refs/heads/a..b" "refs/heads/a b"
                                                 "refs/heads/x.lock" "refs/heads/a//b" "refs/heads/a~" "refs/heads/end."))))
         (ingot-check "a diff of texts without a final newline, one line each"
           (let ((text (ingot:unified-diff "one" "two")))
             (and (search "@@ -1 +1 @@" text) (search (format nil "-one~%+two~%") text))))
         (ingot-check "ancestry and a merge base"
           (and (ingot:ancestor? repo c1 c2) (not (ingot:ancestor? repo c2 c1))
                (string= (ingot:merge-base repo (ingot:resolve repo "c-left") (ingot:resolve repo "c-right")) c2)))
         (ingot-check "a thin pack: the base it lacks is appended to the stored pack"
           (let* ((base (ingot:write-blob other "hello world"))
                  (delta (ingot::octets 11 7 #x90 5 2 (char-code #\X) (char-code #\Y)))
                  (body (ingot:make-buffer 256))
                  (pack-files (lambda () (length (uiop:directory-files (merge-pathnames "two.git/objects/pack/" root) "*.pack")))))
             (ingot:buffer-write body (ingot:ascii "PACK"))
             (ingot:buffer-write body (ingot::be32-octets 2))
             (ingot:buffer-write body (ingot::be32-octets 1))
             (ingot::write-raw-entry-header body ingot::+obj-ref-delta+ (length delta))
             (ingot:buffer-write body (ingot:unhex base))
             (ingot:buffer-write body (ingot:deflate delta))
             (ingot:buffer-write body (ingot:sha1-octets (ingot:buffer-octets body)))
             (let* ((before (funcall pack-files))
                    (stored (ingot:store-pack! other (ingot:buffer-octets body)))
                    (made (ingot:hash-object :blob (ingot:string-to-octets "helloXY"))))
               (and (equal stored (list made))
                    (string= (ingot:octets-to-string (ingot:read-blob other made)) "helloXY")
                    (= (1+ before) (funcall pack-files))
                    (let ((index (ingot::packed-location other made)))
                      (and index (ingot::index-find index base) t))))))
         (ingot-check "a dotfile keeps its dot through a tar: tar-find and untar"
           (let* ((commit (ingot:make-commit! repo :branch "dots" :start-branch "master" :author who :message "Dots"
                                               :actions '((:action :create :path ".hidden" :content "h
"))))
                  (tar (ingot:buffer-octets (ingot:tar-tree repo (ingot:commit-tree (ingot:read-commit repo commit)))))
                  (dir (merge-pathnames "untarred/" root)))
             (ingot:untar tar dir)
             (and (string= (ingot:octets-to-string (ingot:tar-find tar ".hidden")) "h
")
                  (probe-file (merge-pathnames ".hidden" dir)))))
         (ingot-check "a clone and a second fetch through the client side, over an in-process transport"
           (let* ((third (ingot:init-repository (merge-pathnames "three.git/" root)))
                  (transport (lambda (service body)
                               (if body (ingot:upload-pack repo body) (ingot:advertise-refs repo service)))))
             (multiple-value-bind (set deleted received) (ingot:fetch! third transport)
               (and (plusp received) (null deleted)
                    (= (length set) (length (ingot:refs repo)))
                    (string= (ingot:head-oid third) (ingot:head-oid repo))
                    (string= (ingot:default-branch third) "master")
                    (string= (ingot:octets-to-string (ingot:blob-at third c2 "lib/a.lisp")) "(defun a () 1)
")
                    (string= (ingot:resolve-commit third "v1") c2)
                    (multiple-value-bind (set2 deleted2 received2) (ingot:fetch! third transport)
                      (and (null set2) (null deleted2) (zerop received2)))
                    (progn (ingot:delete-ref! repo "refs/heads/dots")
                           (ingot:update-ref! repo "refs/heads/ff" c1)
                           (multiple-value-bind (set3 deleted3) (ingot:fetch! third transport)
                             (and (equal deleted3 '("refs/heads/dots"))
                                  (equal set3 (list (cons "refs/heads/ff" c1))))))))))
         ;; an existing repository, when one is named
         (let ((existing (uiop:getenv "INGOT_REPO")))
           (when (and existing (plusp (length existing)))
             (ingot-check (format nil "reads ~a" existing)
               (let* ((r (ingot:open-repository existing))
                      (head (ingot:head-oid r))
                      (commit (ingot:read-commit r head)))
                 (format t "      head ~a ~s, ~d files, log ~{~a~^ ~}~%"
                         (ingot:short-oid head) (ingot:commit-subject commit)
                         (length (ingot:tree-paths r (ingot:commit-tree commit)))
                         (mapcar (lambda (c) (ingot:short-oid (ingot:commit-oid c))) (ingot:log-commits r head :limit 5)))
                 (and head (ingot:commit-tree commit)))))))
    (ignore-errors (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (format t "~&~d check~:p, ~d failure~:p.~%" *ingot-checks* (length *ingot-failures*))
  (cond ((or (uiop:getenv "INGOT_CI") (uiop:getenv "ANVIL_CI")) (uiop:quit (if *ingot-failures* 1 0)))
        (*ingot-failures* (error "ingot: ~{~a~^; ~}" (reverse *ingot-failures*)))
        (t :all-passed)))
