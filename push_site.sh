#!/bin/sh

set -e

commit_id=`git log -1 --format=format:"%h: %s (%ai)"`

cd _site
if [ ! -d .git ]; then
    git init

    # reuse local checkout's objects if possible
    alternate_store=$(pwd)/../../trofi.github.io/.git/objects
    if [ ! -d ${alternate_store} ]; then
        echo "WARNING: '$alternate_store' checkout does not exist"
        echo "WARNING: push will always refetch initial state."
    else
        echo "$alternate_store" >> .git/objects/info/alternates
    fi

    git remote add --fetch origin git@github.com:trofi/trofi.github.io.git
    # Use pristine freshly generated _site state for new commit.
    # Don't take anything from parent except for the commit ID itself.
    git reset --soft origin/main
fi
git add .
git commit -s -m "sync trofi.github.io.git:${commit_id}" .
git push -u origin main
