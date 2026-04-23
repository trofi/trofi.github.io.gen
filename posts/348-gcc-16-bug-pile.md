---
title: "gcc-16 bugs"
date: April 23, 2026
---

Almost exactly a year passed since my previous
[pile report](/posts/332-gcc-15-bugs-pile-2.html). `gcc-16` was
[branched off](https://gcc.gnu.org/pipermail/gcc/2026-April/247980.html)
from `master` and will receive only regression fixes. `master` is called
`gcc-17` now.

Instead of posting incremental updates at arbitrary tine I'll try yearly
ones. That should cover all the bugs I saw in a release cycle in a
single post.

## Summary

During `gcc-16` cycle I found 31 bug:

- [`libstdc++/119970`](https://gcc.gnu.org/PR119970): `mingw32` `libstdc++` build failure (buffer handling)
- [`target/119929`](https://gcc.gnu.org/PR119929): `mips` build failure
- [`target/120036`](https://gcc.gnu.org/PR120036): `gcc` ICE on `highway-1.2.0` (`float16` related)
- [`c/120060`](https://gcc.gnu.org/PR120060): `gcc` ICE on `bash-5.2` (deprecated declarations)
- [`libstdc++/120147`](https://gcc.gnu.org/PR120147): `mingw32` `libstdc++` build failure (configure variable leak bug)
- [`c++/120185`](https://gcc.gnu.org/PR120185): `gcc` crash on `nss-3.11`, c++ template instantiation bugs
- [`c++/120504`](https://gcc.gnu.org/PR120504): `gcc` now fails to build some forms of template code with partially defined types
- [`target/120697`](https://gcc.gnu.org/PR120697): `ICE` in `ix86_expand_prologue`
- [`target/120830`](https://gcc.gnu.org/PR120830): `ICE` in `subreg3` pass on `highway-1.2.0` and `svt-av1`.
- [`tree-optimization/120929`](https://gcc.gnu.org/PR120929): fortify failure on `file` due to object size mis-estimation
- [`diagnostics/121260`](https://gcc.gnu.org/PR121260): `--enable-checking=release` `#ifdef`ery failures
- [`target/121572`](https://gcc.gnu.org/PR121572): `mpfr` and `python` TLS descriptor clobbers
- [`target/121635`](https://gcc.gnu.org/PR121635): `nodejs` `ICE` in TLS lifter
- [`libgcc/121718`](https://gcc.gnu.org/PR121718): unexpected requirement of `-lm` on `-Decimal128` arithmetic
- [`libgcc/122198`](https://gcc.gnu.org/PR122198): `gcc` `ICE` in `GC`
- [`fortran/122257`](https://gcc.gnu.org/PR122257): `fortran` bootstrap failure, `-Werror=`
- [`tree-optimization/122301`](https://gcc.gnu.org/PR122301): `dav1d-1.5.1` `ICE` in `gcc` vectorizer.
- [`tree-optimization/122376`](https://gcc.gnu.org/PR122376): `graphviz` `ICE` on vectorizer
- [`tree-optimization/no-bug`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=1ceda79ca5fe1a1a296624a98de8fd04958fbe55): ICE fix in `mesa` (fixed before the report), vectorizer
- [`tree-optimization/122406`](https://gcc.gnu.org/PR122406): `ICE` on `numpy`, vectorizer
- [`tree-optimization/122502`](https://gcc.gnu.org/PR122502): `ICE` on `systemd-258.1` and `ffmpeg-8.0`, `sccp` after `clz` addition
- [`libstdc++/122903`](https://gcc.gnu.org/PR122903): `gjs` build failure, ended up being `gjs` bug
- [`target/123032`](https://gcc.gnu.org/PR123032): `libsodium` wrong code on vectorizer
- [`tree-optimization/123043`](https://gcc.gnu.org/PR123043): `zlib` wrong code and `ICE` in vectorizer
- [`rtl-optimization/123267`](https://gcc.gnu.org/PR123267): `gcc` own `ICE` (`tzdb`)
- [`ipa/123543`](https://gcc.gnu.org/PR123543): `ipa` bug causes wrong code on `-O3` for `python`, `libgit2`, `graphite2`, etc.
- [`target/123725`](https://gcc.gnu.org/PR123725): `highway-1.3.0` `-O3` build failure on `aarch64-linux`.
- [`tree-optimization/123803`](https://gcc.gnu.org/PR123803): `gcc-6.3.0` ICE with `-fno-strict-overflow` in `slsr`
- [`c/123882`](https://gcc.gnu.org/PR123882): `wlroots` build failure around `restrict` keyword
- [`fortran/no-bug`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=d4893480373deea3e9da267f85d764469d65b497): `openblas` ICE in `gfortran`
- [`tree-optimization/124875`](https://gcc.gnu.org/PR124875): `llvm-22.1.2` wrong code on test suite

## Fun Bugs

All wrong code seen at runtime of compiled programs were fun to extract.
Minimized examples are not very pretty to look sat. I'll just leave the
links here:

- [`target/123032`](https://gcc.gnu.org/PR123032): `libsodium` wrong code on vectorizer
- [`tree-optimization/123043`](https://gcc.gnu.org/PR123043): `zlib` wrong code and `ICE` in vectorizer
- [`ipa/123543`](https://gcc.gnu.org/PR123543): `ipa` bug causes wrong code on `-O3` for `python`, `libgit2`, `graphite2`, etc.
- [`tree-optimization/124875`](https://gcc.gnu.org/PR124875): `llvm-22.1.2` wrong code on test suite

## Histogram

This time I got most bugs from `tree-optimization` subsystem:

- `tree-optimization`: 9
- `target`: 8
- `libstdc++`: 2
- `c`: 2
- `c++`: 2
- `libgcc`: 2
- `fortran`: 2
- `diagnostics`: 1
- `ipa`: 1
- `rtl-optimization`: 1

Almost as many bugs came up in `target`-specific areas.

## Parting Words

`gcc-master` is still able to produce new interesting bugs.

31 bug per year is slightly more than one bug per week. It feels like a
lot given how little time I spend to extract the samples.

Of `31` bugs I saw I reported `22`. The rest were caught (and sometimes
already fixed!) by others by the time I got to them.

Have fun!
