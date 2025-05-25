---
title: cvsps gets an update
date: January 10, 2013
---

`TL;DR`:

``` bash
cvsps --fast-export --root $CVSROOT $proj | git fast-import
```

Not so long ago [`esr`](https://en.wikipedia.org/wiki/Eric_S._Raymond) has
taken over maintainer role of wonderful `cvsps tool`.

In short `CVS` does not store change sets at all and special tools
are needed to extract that information out of `CVS` repository.
The problem with this is usually lack of direct access to repository
itself. You only have a client program to check out any random version or
query history for a given file or a set of files.
Tools like `git-cvsimport` use `cvsps` to extract change set
information, fetch all files belonging to the commit and commit it to
respective branch.
`cvsps` was a buggy and mostly abandoned project. `2.2_beta1` was
the only version that did not crash for me.
`esr` decided to fix bugs all the way down *and* get rid of
`git-cvsimport` as an intermediary (`--fast-export` option in `3.x`
series).

Alive upstream is a virtue and I've packaged one without any testing.
The `3.x` came out incompatible for `git-cvsimport`, but it's
really a feature. Just use it directly and post-process the result with
`git filter-branch` or similar.
After a [breakage report](https://bugs.gentoo.org/450424) I've decided
to use it myself on our largest and oldest `CVS` project at work.
`cvsps` hung at the very start. `valgrind` told me there was garbage
in input data and it started my real
[contribution](https://gitorious.org/cvsps/cvsps/commit/c4b06934ede0ad50b4d88c6d7cc0bf86bc9ebb39).
The hangup didn't go away and I've started digging into in-the-wire
`CVS` client format which resulted in a [real
fix](https://gitorious.org/cvsps/cvsps/commit/1baf820a9dbac2bdf5bd5536ec388af7f47a987b)
of the problem.

And now the fun part (the reason I have written the post): while fixing
above bug i've noticed that sometimes code fetching the revisions works
noticeably faster (5x speedup) depending on random factors.
I've looked at `strace -r` output and figured that first response to
`cvs co <file>` request comes back after `150-200` milliseconds.
It's a severe lag. You can't fetch faster, than 5 files per second.
Playing a bit with **Nagle's** hacks the turbo booster fix gone to [the
tree](https://gitorious.org/cvsps/cvsps/commit/5c876f67cf9e0bf544d0d0ad0b09b54decaac6d1).

After those hacks I haven't managed to kill `cvsps` on my internal
projects.
Well, let's try to test it on really large `CVS` project: `gentoo`'s
`ebuild` tree. It has `2.2GB` of history.
Some preparations:

``` bash
# in $HOME/portage/gentoo-x86.rsync
rsync -aP rsync://anonvcs.gentoo.org/vcs-public-cvsroot/gentoo-x86/ gentoo-x86/
rsync -aP rsync://anonvcs.gentoo.org/vcs-public-cvsroot/CVSROOT/ CVSROOT/
```

Let's try to import `kde-base` category (it's one of largest
categories, takes `190MBs` of history, 10% of the whole tree).

``` bash
$ git init
$ time { ../cvsps --root :local:$HOME/portage/gentoo-x86.rsync --fast-export gentoo-x86/kde-base | git fast-import; }
cvsps: branch symbol RELEASE-1_4 not translated
cvsps: multiple vendor or anonymous branches; head content may be incorrect.
git-fast-import statistics:
....
real    29m11.682s
user    4m11.970s
sys     1m9.217s
```

A bit more complex example with authentication:

``` bash
$ git init
$ CVS_RSH=ssh ../cvsps --fast-export --root :ext:slyfox@cvs.gentoo.org:/var/cvsroot gentoo-x86/dev-lang/ghc | git fast-import
```

And the scariest run. The whole-tree conversion

``` bash
$ git init
$ ../cvsps --root :local:$HOME/portage/gentoo-x86.rsync --fast-export gentoo-x86 | git fast-import
```

It takes `3.8G` of `RAM` to build in-RAM revision history. I
haven't got it finished yet, but I expect 3-4 hours of work.
Next step it to setup incremental updates and push the result out to the
public.

**UPDATE**: finished import. It took `~5 hours`, resulting repo is
`1.2GB`:

    git-fast-import statistics:
    ---------------------------------------------------------------------
    Alloc'd objects:    2655000
    Total objects:      2653581 (    148626 duplicates                  )
          blobs  :       986447 (    119173 duplicates     493906 deltas of     966966 attempts)
          trees  :      1348212 (     29453 duplicates    1192295 deltas of    1241649 attempts)
          commits:       318922 (         0 duplicates          0 deltas of          0 attempts)
          tags   :            0 (         0 duplicates          0 deltas of          0 attempts)
    Total branches:           8 (         3 loads     )
          marks:     1073741824 (   1424542 unique    )
          atoms:         174556
    Memory total:        150808 KiB
           pools:         26355 KiB
         objects:        124453 KiB
    ---------------------------------------------------------------------
    pack_report: getpagesize()            =       4096
    pack_report: core.packedGitWindowSize = 1073741824
    pack_report: core.packedGitLimit      = 8589934592
    pack_report: pack_used_ctr            =    8342911
    pack_report: pack_mmap_calls          =     354193
    pack_report: pack_open_windows        =          2 /          3
    pack_report: pack_mapped              = 1213886709 / 1890833032
    ---------------------------------------------------------------------
    real    317m53.483s
    user    19m24.108s
    sys     5m47.618s

And it `broke`. Latest commit is:

    commit e123e7caa8b45f3ce8a7b358e3137de393f2619c
    Author: agriffis <agriffis>
    Date:   Tue Feb 7 08:55:13 2006 +0000

**UPDATE 2**: more info. It turns out to be a bug in `cvs server`
itself. It leaked all the `32GB` of RAM and crashed on poor `cvsps`
leaving incomplete import.

Due to those leaks repo importing slows down a bit on every checkout
request: `cvs server` serves every request by forking, thus the more
`PTEs` have to be copied on each `fork()`. Looking at `cvs server`
now to fix the disease.
