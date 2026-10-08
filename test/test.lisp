;; Copyright © 2026 Genworks International
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.  Distributed WITHOUT
;; ANY WARRANTY; see <https://www.gnu.org/licenses/agpl-3.0.html>.

;;;
;;; ingot's own checks.  Load this into an image that has the
;;; system loaded.  It makes a repository in a scratch directory and
;;; exercises objects, refs, commits, diffs, merges, packs and the
;;; protocol end to end, with no git binary anywhere.  With INGOT_REPO
;;; naming an existing repository (a checkout or a bare one), it also
;;; reads that one: its head, its log, its tree.  It prints a line per
;;; check; with ANVIL_CI set it leaves the Lisp with status 0 or 1,
;;; otherwise it signals at the end when anything failed.
;;;

(in-package :cl-user)

(defvar *agit-failures* nil)
(defvar *agit-checks* 0)

(defmacro agit-check (name &body body)
  `(let ((ok? (handler-case (progn ,@body)
                (error (condition) (format t "~&      ~a~%" condition) nil))))
     (incf *agit-checks*)
     (format t "~&~a ~a~%" (if ok? "ok  " "FAIL") ,name)
     (unless ok? (push ,name *agit-failures*))
     ok?))

(defun agit-scratch-directory ()
  (merge-pathnames (format nil "ingot-test-~a/" (random 1000000000))
                   (uiop:temporary-directory)))

(let* ((root (agit-scratch-directory))
       (repo nil) (other nil)
       (who (ingot:make-identity "Test Person" "test@example.org" :time 1700000000))
       (c1 nil) (c2 nil) (c3 nil))
  (setq *agit-failures* nil *agit-checks* 0)
  (unwind-protect
       (progn
         (agit-check "hashing: the empty blob and a known string"
           (and (string= (ingot:hash-object :blob (ingot:make-octets 0)) "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")
                (string= (ingot:hash-object :blob (ingot:string-to-octets "hello")) "b6fc4c620b67d95f953a5c1c1230aaab5db5a1b0")))
         (agit-check "pkt-line round trip"
           (let* ((buffer (ingot::make-buffer))
                  (octets (progn (ingot:write-pkt-line buffer "hello") (ingot:write-pkt-flush buffer)
                                 (ingot:write-pkt-line buffer (ingot:string-to-octets "world"))
                                 (ingot::buffer-octets buffer))))
             (multiple-value-bind (lines at) (ingot:read-pkt-lines octets)
               (and (= (length lines) 1) (string= (ingot::pkt-text (first lines)) "hello")
                    (= at 13)
                    (string= (ingot::pkt-text (ingot::read-pkt-line octets at)) "world")))))
         (agit-check "a bare repository is made"
           (setq repo (ingot:init-repository (merge-pathnames "one.git/" root)))
           (and (ingot:repository-exists? (merge-pathnames "one.git/" root))
                (string= (ingot:default-branch repo) "master")
                (null (ingot:head-oid repo))))
         (agit-check "a first commit from actions"
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
         (agit-check "a second commit: update, delete, move"
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
         (agit-check "the log, newest first"
           (equal (mapcar #'ingot:commit-subject (ingot:log-commits repo (ingot:head-oid repo)))
                  '("Second" "First")))
         (agit-check "the log of one path"
           (equal (mapcar #'ingot:commit-subject (ingot:log-commits repo (ingot:head-oid repo) :path "src/b.lisp"))
                  '("Second" "First")))
         (agit-check "the tree diff of the second commit"
           (let ((changes (ingot::commit-diff repo c2)))
             (equal (mapcar (lambda (c) (list (ingot::change-status c) (ingot::change-path c))) changes)
                    '((:modified "README.md") (:added "lib/a.lisp") (:deleted "src/a.lisp") (:deleted "src/b.lisp")))))
         (agit-check "a unified diff"
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
         (agit-check "a short id resolves, a ref name resolves"
           (and (string= (ingot:resolve repo (subseq c2 0 7)) c2)
                (string= (ingot:resolve repo "master") c2)
                (string= (ingot:resolve repo "refs/heads/master") c2)
                (null (ingot:resolve repo "nothing-here"))))
         (agit-check "refs: create, compare-and-swap, delete"
           (ingot:update-ref! repo "refs/heads/topic" c1 :old nil)
           (let ((conflict? (handler-case (progn (ingot:update-ref! repo "refs/heads/topic" c2 :old "0000000000000000000000000000000000000000") nil)
                              (ingot:ref-conflict () t))))
             (and conflict?
                  (string= (cdr (assoc "topic" (ingot:branches repo) :test #'string=)) c1)
                  (ingot:delete-ref! repo "refs/heads/topic")
                  (null (assoc "topic" (ingot:branches repo) :test #'string=)))))
         (agit-check "a fast-forward merge"
           (ingot:update-ref! repo "refs/heads/ff" c1 :old nil)
           (and (eq (ingot:can-merge? repo c2 c1) :fast-forward)
                (string= (ingot:merge-branches! repo "master" "ff" :author who) c2)))
         (agit-check "a three-way merge without conflict"
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
         (agit-check "a three-way merge with a conflict"
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
         (agit-check "a tar of the tree"
           (let ((tar (ingot::buffer-octets (ingot:tar-tree repo (ingot:commit-tree (ingot:read-commit repo c2)) :prefix "one/"))))
             (and (zerop (mod (length tar) 512))
                  (search (ingot:string-to-octets "one/lib/a.lisp") tar)
                  (search (ingot:string-to-octets "(defun a () 1)") tar))))
         (agit-check "a zip of the tree"
           (let ((zip (ingot:zip-tree repo (ingot:commit-tree (ingot:read-commit repo c2)) :prefix "one/")))
             (and (= (aref zip 0) #x50) (= (aref zip 1) #x4b)
                  (search (ingot:string-to-octets "one/README.md") zip))))
         (agit-check "a pack written and indexed again"
           (let* ((oids (ingot:reachable-objects repo (list c2) nil))
                  (pack (ingot::buffer-octets (ingot:write-pack repo oids)))
                  (objects (ingot:index-pack pack (lambda (oid) (declare (ignore oid)) nil))))
             (and (= (length oids) 10)  ; 2 commits, 4 trees (root, src, root', lib), 4 blobs
                  (= (length objects) (length oids))
                  (every (lambda (object oid) (string= (ingot:object-oid object) oid)) objects oids))))
         (agit-check "a delta applies"
           ;; a hand-made delta: copy 5 bytes from offset 0 of the base, insert "XY"
           (let* ((base (ingot:string-to-octets "hello world"))
                  (delta (ingot::octets 11 7 #x90 5 2 (char-code #\X) (char-code #\Y))))
             (string= (ingot:octets-to-string (ingot::apply-delta base delta)) "helloXY")))
         (agit-check "receive-pack into an empty repository, then upload-pack out of it"
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
         (agit-check "a push that is not a fast-forward is refused by the check"
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
         (agit-check "a branch deleted by push"
           (ingot:update-ref! other "refs/heads/gone" c1 :old nil)
           (let* ((request (ingot::buffer-octets
                            (let ((b (ingot::make-buffer)))
                              (ingot:write-pkt-line b (format nil "~a ~a refs/heads/gone~a report-status~%" c1 ingot::*zero-oid* (code-char 0)))
                              (ingot:write-pkt-flush b)
                              b))))
             (ingot:receive-pack other request)
             (null (assoc "gone" (ingot:branches other) :test #'string=))))
         (agit-check "merge-text: both sides change different places"
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
         ;; an existing repository, when one is named
         (let ((existing (uiop:getenv "INGOT_REPO")))
           (when (and existing (plusp (length existing)))
             (agit-check (format nil "reads ~a" existing)
               (let* ((r (ingot:open-repository existing))
                      (head (ingot:head-oid r))
                      (commit (ingot:read-commit r head)))
                 (format t "      head ~a ~s, ~d files, log ~{~a~^ ~}~%"
                         (ingot:short-oid head) (ingot:commit-subject commit)
                         (length (ingot:tree-paths r (ingot:commit-tree commit)))
                         (mapcar (lambda (c) (ingot:short-oid (ingot:commit-oid c))) (ingot:log-commits r head :limit 5)))
                 (and head (ingot:commit-tree commit)))))))
    (ignore-errors (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  (format t "~&~d check~:p, ~d failure~:p.~%" *agit-checks* (length *agit-failures*))
  (cond ((uiop:getenv "ANVIL_CI") (uiop:quit (if *agit-failures* 1 0)))
        (*agit-failures* (error "ingot: ~{~a~^; ~}" (reverse *agit-failures*)))
        (t :all-passed)))
