#!/bin/sh
# Ingot's differential suite: the host's git and Ingot answer the same
# questions about a repository, and the answers must agree.
#
#   ingot/test/differential.sh /path/to/repository [image]
#
# git runs here; Ingot runs in a Lisp container (genworks/gendl:devo-sbcl
# unless another image is named) that mounts the repository read-only
# and this checkout.  Exit status 0 when every answer agrees.
set -eu
repo=${1:?a repository}
image=${2:-genworks/gendl:devo-sbcl}
here=$(cd "$(dirname "$0")/../.." && pwd)
facts=$(mktemp -d)
trap 'rm -rf "$facts"' EXIT

g() { git -C "$repo" "$@"; }

# what git says
g rev-parse HEAD > "$facts/head"
g symbolic-ref --short HEAD > "$facts/branch" 2>/dev/null || echo "" > "$facts/branch"
g for-each-ref --format='%(refname) %(objectname)' > "$facts/refs"
g log --format=%H -50 > "$facts/log"
g ls-tree -r HEAD --format='%(objectmode) %(objecttype) %(objectname) %(path)' > "$facts/tree"
g rev-list --count HEAD > "$facts/count"
# twenty blobs, scattered: their ids and sizes
g ls-tree -r HEAD --format='%(objectname)' | awk 'NR % 7 == 1' | head -20 > "$facts/blobs"
while read -r oid; do g cat-file -s "$oid"; done < "$facts/blobs" > "$facts/blob-sizes"
# the files the last commit changed
g diff-tree --no-commit-id --name-status -r HEAD > "$facts/changes"
# a merge base, when there are two branches
first=$(g for-each-ref --format='%(objectname)' refs/heads | sed -n 1p)
second=$(g for-each-ref --format='%(objectname)' refs/heads | sed -n 2p)
if [ -n "${second:-}" ]; then
  g merge-base "$first" "$second" > "$facts/merge-base" || true
  printf '%s %s\n' "$first" "$second" > "$facts/merge-base-pair"
fi

echo "git's facts written; asking Ingot in $image"
docker run --rm \
  -v "$(cd "$repo" && pwd)":/repo:ro \
  -v "$here":/anvil:ro \
  -v "$facts":/facts:ro \
  "$image" \
  sh -c 'cd /opt/gendl && ./gdl/program/gdl-$(case "$0" in *ccl*) echo ccl;; *) echo sbcl;; esac) -n -b --load /anvil/ingot/test/differential.lisp' "$image"
