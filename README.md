# Ingot

Git in Common Lisp.  Ingot reads and writes Git repositories that are
byte-for-byte the ones C Git makes, with no `git` binary and no
subprocess: loose and packed objects (packfile deltas included), refs
and `packed-refs`, trees and commits and tags, history walks and merge
bases, tree and line diffs, three-way merges, the smart protocol's
server side (`upload-pack` and `receive-pack` over stateless HTTP) with
the pack a fetch answers with, the client side of a fetch over a
transport the caller lends, and archives of a tree.  It is the Git
engine of [Anvil](../), the forge, and a system of its own that loads
into any Lisp with ASDF.

Scope, on purpose: a reliable server-side engine, not the porcelain.
There is no work tree, no index, no `git status`; a commit is made from
a base tree and a list of actions.

## Loading

```lisp
(asdf:load-asd "/path/to/anvil/ingot/ingot.asd")
(ql:quickload :ingot)            ; chipz, salza2, babel, bordeaux-threads
```

## A tour

```lisp
(defparameter *repo* (ingot:open-repository "/path/to/checkout/"))   ; or a bare .git

(ingot:default-branch *repo*)                      ; "master"
(ingot:head-oid *repo*)                            ; "c68adcc2..."
(ingot:branches *repo*)                            ; (("master" . "c68a...") ...)

(let ((commit (ingot:read-commit *repo* (ingot:head-oid *repo*))))
  (list (ingot:commit-subject commit)
        (ingot:commit-author-name commit)
        (ingot:commit-parents commit)
        (length (ingot:tree-paths *repo* (ingot:commit-tree commit)))))

(ingot:octets-to-string (ingot:blob-at *repo* (ingot:head-oid *repo*) "README.md"))

(mapcar #'ingot:commit-subject (ingot:log-commits *repo* (ingot:head-oid *repo*) :limit 10 :path "src/"))

;; a commit with no work tree
(ingot:make-commit! *repo* :branch "master"
                           :author (ingot:make-identity "A Person" "person@example.org")
                           :message "Add a file"
                           :actions '((:action :create :path "notes/today.md" :content "# Today\n")))

;; diffs and merges
(ingot:commit-diff *repo* (ingot:head-oid *repo*))        ; change structures by path
(ingot:unified-diff old-text new-text)
(ingot:can-merge? *repo* source-commit target-commit)     ; :fast-forward :merge :conflict :already
(ingot:merge-branches! *repo* "topic" "master" :author who)

;; the protocol, as the HTTP layer uses it
(ingot:advertise-refs *repo* :upload-pack)                ; the info/refs answer
(ingot:upload-pack *repo* request-octets)                 ; the pack
(ingot:receive-pack *repo* request-octets :check (lambda (command) nil))   ; takes the push

;; archives
(ingot:tar-gz-tree *repo* tree-oid :prefix "project-1.0/")
(ingot:zip-tree *repo* tree-oid :prefix "project-1.0/")

;; the client side: bring a repository up to date from another server
;; (a clone, when it is empty) over a transport the caller lends -- a
;; function of a service keyword and a request body, nil for the refs
;; advertisement, answering the server's octets; Anvil lends
;; AllegroServe's client (anvil:http-git-transport)
(ingot:fetch! *repo* transport)          ; values: the refs set, the refs deleted, the objects received
```

## Git commands and their Ingot forms

Ingot is the engine, not the porcelain, but most of what one types at
`git` has a form here.  `*repo*` is an open repository; ids are
40-character strings; a commit is read with `read-commit` and asked
with the `commit-` readers.

| `git` | Ingot |
| --- | --- |
| `git init --bare notes.git` | `(init-repository "notes.git/")` |
| `git rev-parse HEAD`, `git rev-parse topic` | `(head-oid *repo*)`, `(resolve *repo* "topic")` |
| `git branch`, `git tag` | `(branches *repo*)`, `(tags *repo*)` |
| `git log -10 -- src/` | `(log-commits *repo* oid :limit 10 :path "src/")` |
| `git show <commit>` | `(read-commit *repo* oid)`, `(commit-diff *repo* oid)` |
| `git ls-tree -r <tree>` | `(tree-paths *repo* tree-oid)` |
| `git show <commit>:path/file` | `(blob-at *repo* commit-oid "path/file")` |
| `git hash-object file` | `(hash-object :blob octets)` |
| `git add` + `git commit` (no work tree here) | `(make-commit! *repo* :branch "master" :author who :message "..." :actions '((:action :create :path "f" :content "...")))` |
| `git checkout -b topic master` + a commit | `(make-commit! *repo* :branch "topic" :start-branch "master" ...)` |
| `git diff a b -- file` | `(unified-diff old-text new-text)`; whole trees: `(tree-diff *repo* old-tree new-tree)` |
| `git merge-base a b` | `(merge-base *repo* a b)` |
| `git merge topic` (on master) | `(can-merge? *repo* src dst)` then `(merge-branches! *repo* "topic" "master" :author who)` |
| `git update-ref`, `git branch -D` | `(update-ref! *repo* "refs/heads/x" oid :old old)`, `(delete-ref! *repo* "refs/heads/x")` |
| `git symbolic-ref HEAD refs/heads/main` | `(set-default-branch! *repo* "main")` |
| `git archive --format=tar.gz --prefix=p/ <tree>` | `(tar-gz-tree *repo* tree-oid :prefix "p/")`, `(zip-tree ...)` |
| `git fetch`, `git clone` | `(fetch! *repo* transport)` |
| `git push` | `write-pack` of `(reachable-objects *repo* wants haves)` behind the receive-pack commands; Anvil's mirror push is the worked example |
| serving `git-upload-pack`, `git-receive-pack` | `(advertise-refs *repo* :upload-pack)`, `(upload-pack *repo* body)`, `(receive-pack *repo* body :check ...)` |

Not here: `git status`, `git add`, the index and the work tree.  A
commit is made from a base tree and a list of actions.

Every function signals `ingot:git-error` (or its subtypes `not-found`
and `ref-conflict`) rather than returning half an answer.  Ref updates
are compare-and-swap under a per-repository lock, written through a
temporary file and a rename; a push stores its pack under the same lock
before any ref moves.

## What is not here

SHA-256 repositories (the object id is twenty bytes throughout: the
seam is a handful of constants).  Shallow clones (a client asking
`--depth` is told so by its own git).  Deltas of Ingot's own making: a
pack it writes reuses the deltas of the packs it holds and writes the
rest whole, and nothing repacks.  A pushed pack is kept as it came,
with an index of its own.  The client side of a push beyond what a
mirror needs.  Rename detection in diffs.  Submodule contents.

## Checks

`test/test.lisp` makes a repository in a scratch directory and works
every part, protocol included, with no git anywhere; `INGOT_REPO=` names
a real repository to read as well.  `test/differential.sh` is the
differential suite: it asks the host's `git` for the facts of a
repository and has Ingot, in a Lisp container, answer the same
questions, and compares.

## License

GNU Affero General Public License, version 3 or later.
