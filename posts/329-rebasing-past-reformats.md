---
title: "Rebasing past reformats"
date: December 12, 2024
---

## TL;DR

Did you ever have to deal with a huge list of conflicts on rebase caused
by automatic reformatting of an upstream code base?

If you got into a similar situation you might be able to automatically
recreate your changes with `git filter-branch --tree-filter` and a
`git commit --allow-empty` trick.

## story mode

I have local fork of `staging` branch of
[`nixpkgs`](https://github.com/NixOS/nixpkgs/) `git` repository to do
various tests against experimental upstream packages (like `gcc` from
`master` branch) or experimental `nix` features (like `ca-derivations`).

I have about 350 patches in the fork. I sync this forked branch about
daily against the upstream `nixpkgs/staging`. Most of the time
`git pull --rebase` is enough and no conflicts are there. Once a month
there is one or two files to tweak. Not a big deal.

A few days ago `nixpkgs` landed a partial source code reformatting
patch as [PR#322537](https://github.com/NixOS/nixpkgs/pull/322537). It
automatically re-indents ~21000 `.nix` files in the repository with a
`nixfmt` tool. My `git pull --rebase` generated conflicts on first few
patches against my branch. I aborted it with `git rebase --abort`.

I would not be able to manually solve such a huge list of commits and I
wondered if I could somehow regenerate my patches against the indented
source.

In theory rebasing past such change should be a mechanical operation: I
have the source tree before the patch and after the patch. All I need to
do is to autoformat both `before` and `after` trees and then `diff`
them.

I managed to do it with help of `git commit --allow-empty` and
`git filter-branch --tree-filter`.

Here are my exact commands used:

### actual commands

Here is the step-by-step I did to rebase my local `staging` branch past
the source reformatting
[`667d42c00d566e091e6b9a19b365099315d0e611` commit](https://github.com/NixOS/nixpkgs/commit/667d42c00d566e091e6b9a19b365099315d0e611)
to avoid conflicts:

1. Create an empty commit (to absorb initial formatting later):

   ```
   $ git commit --allow-empty -m "EMPTY commit: will absorb relevant formatting changes"
   ```

2. Move the last empty commit in the patch queue to the beginning of
   the patch queue:

   ```
   $ git rebase -i --keep-base
   ```

   In the edit menu move the
   `"EMPTY commit: will absorb relevant formatting changes"` entry from
   last line of the list to the first line.

3. Get files in the branch affected by the formatting change:

   The formatting change is `667d42c00d566e091e6b9a19b365099315d0e611`.

   ```
   $ FORMATTED_FILES=$(git diff --name-only \
       667d42c00d566e091e6b9a19b365099315d0e611^..667d42c00d566e091e6b9a19b365099315d0e611 \
       -- $(git diff --name-only origin/staging...staging) | tr $'\n' ' ')
   ```

   This will populate `FORMATTED_FILES` shell variable with affected
   files.

4. Reformat the `$FORMATTED_FILES` files:

   ```
   $ FILTER_BRANCH_SQUELCH_WARNING=1 git filter-branch \
     --tree-filter "nixfmt $FORMATTED_FILES" -- $(git merge-base origin/staging staging)..
   ...
   Rewrite 6fc0a951e9b7a7e3f80628ca0a6c4c9f54fd2dd6 (56/327) (65 seconds passed, remaining 314 predicted)
   ...
   Rewrite c20df82da66da6521f355af508bfedc047cffa64 (326/326) (1183 seconds passed, remaining 0 predicted)
   Ref 'refs/heads/staging' was rewritten
   ```

   This command will populate our empty commit with reformatting changes
   and rebase the rest of commits against it without manual intervention.

5. Rebase past the formatting as usual:

   ```
   $ git rebase -i
   ```

   Here `git rebase -i` will tell you that the first commit became empty.
   You can either skip or commit an empty one. I skipped it with
   `git rebase --skip`.

Done!

Once I executed the above I got just one trivial conflict unrelated to
reformatting.

## parting words

`git filter-branch --tree-filter` is a great tool to mangle the
repository! But before using it make sure you back you local tree: it's
very easy to get it to "destroy" all your work (`git reflog` will still
be able to save your past commits).

It took `git filter-branch --tree-filter` about 5 minutes to rebase
`326` commits that touch ~200 files. My understanding is that most time
is spent on `nixfmt` utility itself and not on `git` operations.
`nixfmt` is not very fast: it takes about a minute to reformat the whole
of `nixpkgs` (~300MB of `.nix` files).

`nixpkgs` plans for reformat event more sources in future. I will likely
be using this tip a few more times.

Have fun!
