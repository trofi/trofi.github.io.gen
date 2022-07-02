---
title: "A small update on 'make --shuffle' mode"
date: June 21, 2022
---

# Tl;DR

GNU Make \-\-shuffle mode [entered upstream git repository](https://git.savannah.gnu.org/cgit/make.git/commit/?id=621d3196fae94e9006a7e9c5ffdaf5ec209bf832) \\o/.

For testing convenience I also tarballed the development **GNU make**
snapshot from today's [git state](https://git.savannah.gnu.org/cgit/make.git/commit/?id=84ed34ba5a32dd52600c756445f3724c9e23cf95):
<https://slyfox.uni.cx/distfiles/make/make-4.3.90.20220619.tar.lz>.

Note that **\-\-shuffle** is not enabled by default. You can enable it
by any of below methods whichever matches best your environment:

- Run **make \-\-shuffle**. For casual testing.
- Export **MAKEFLAGS=\-\-shuffle** environment variable. For day-to-day
  development or distribution-wide testing. It is also a safe value
  for **GNU make** that does not yet understand **\-\-shuffle** option.
- Apply <https://slyfox.uni.cx/distfiles/make/make-4.3.90.20220619-random-by-default.patch>
  on top of snapshot tarball. Useful for environments where there is no
  easy way to pass a parameter to **make** or to set **MAKEFLAGS**
  variable.

# minor improvements

Compared to the initial patch announced as a
[proof of concept](/posts/238-new-make-shuffle-mode.html)
there is one extra change: presence of **.NOTPARALLEL:** directive
in a **Makefile** now disables shuffling in that file.

It was done to accomodate rare projects that rely on execution order
specified in **Makefile** and don't plan to make dependencies correct
in near future. The example is **netpbm**:
<https://sourceforge.net/p/netpbm/code/HEAD/tree/trunk/GNUmakefile#l110>

# failure examples

A few new bugs were found and/or fixed:

- vim: <https://github.com/vim/vim/pull/9978>
- groff: <https://savannah.gnu.org/bugs/?62084>
- gpm: <https://github.com/telmich/gpm/pull/43>
- gnu-efi: <https://sourceforge.net/p/gnu-efi/patches/84/>
- cmigemo: <https://github.com/koron/cmigemo/pull/29>
- harvid: <https://github.com/NixOS/nixpkgs/pull/178647>
- subversion: <https://issues.apache.org/jira/browse/SVN-4901>
- ocamlbuild: <https://github.com/ocaml/ocamlbuild/pull/318>
- ghc(!): <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8531>
- slang: fixed in slang-pre2.3.3-77 snapshot in 5d36d7c2410cbd640470a9c97cc544d257c64660 commit. Could not find web UI for repository.
- strace: <https://github.com/strace/strace/pull/215>
- ski: <https://github.com/trofi/ski/commit/7cc1ced067bf3822ef87c323b6110f38bc0ca1b5>
- heimdal: <https://github.com/heimdal/heimdal/pull/999>
- src-highlite: <https://savannah.gnu.org/patch/index.php?10262>
- gcc: <https://gcc.gnu.org/PR106162>

Nothing complicated. Just a few missing dependencies.

Give it a try!