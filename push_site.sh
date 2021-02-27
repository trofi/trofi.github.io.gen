#!/bin/sh

set -e

commit_id=`git log -1 --format=format:"%h: %s (%ai)"`

cd _site
if [ ! -d .git ]; then
    git init
    git remote add --fetch origin git@github.com:trofi/trofi.github.io.git
fi
git add .
git commit -s -m "sync trofi.github.io.git:${commit_id}" .
git push --force -u origin main
