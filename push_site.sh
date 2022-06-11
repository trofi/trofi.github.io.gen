#!/bin/sh

set -e

commit_id=`git log -1 --format=format:"%h: %s (%ai)"`

cd _site
if [ ! -d .git ]; then
    git init
    git remote add --fetch origin git@github.com:trofi/trofi.github.io.git
    # Use pristine freshly generated _site state for new commit.
    # Don't take anything from parent except for the commit ID itself.
    git reset --soft origin/main
fi
git add .
git commit -s -m "sync trofi.github.io.git:${commit_id}" .
git push -u origin main
