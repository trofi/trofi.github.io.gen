---
title: "profiling binutils linkers in nixpkgs"
date: October 11, 2025
---

## background

I've been using `binutils-2.45` against local `nixpkgs` checkout for a
while to weed out minor problems in other packages. So far I encountered
my old friend
[`guile` over-stripping issue](https://github.com/NixOS/nixpkgs/pull/438714).

[`GNU gold` linker](https://en.wikipedia.org/wiki/Gold_(linker)) was
[deprecated](https://lists.gnu.org/archive/html/info-gnu/2025-02/msg00001.html)
in `binutils` upstream as it does not have developer
power behind it. While `bfd` linker (the default) gets maintenance
attention. `binutils-2.45` intentionally does not have `gold` source
distributed with it to nudge users off `gold`.

To fix rare `nixpkgs` package build failures that rely on `ld.gold` I
trivially replaced all the links to `ld.bfd` locally and built my system.
No major problems found.

## an `ld.gold` removal obstacle

In a [recent discussion thread](https://discourse.nixos.org/t/removing-gold-from-nixpkgs/70496/8)
the question was raised if/how `nixpkgs` could switch to `gold`-less
`binutils`. One of the interesting points of the thread is that `ld.bfd`
is occasionally ~`3x` times [slower than `ld.gold`](https://github.com/NixOS/nixpkgs/pull/418735#issuecomment-2993624063)
on files that already take multiple seconds to link with `ld.gold`.
For me it was quite a surprise as `ld.bfd` does
[get speedup improvements](https://www.youtube.com/watch?v=h5pXt_YCwkU)
time to time. Thus, I suspected it should be some kind of a serious bug
on `ld.bfd` side.
I wondered if I could reproduce such a big performance
drop and if I could find a low hanging fix. Or at least report a bug to
`binutils` upstream.

I used the same `pandoc` `nixpkgs` package to do the linker testing. It's
a nice example as it builds only 4 small `haskell` source files and
links in a huge amount of static `haskell` libraries. A perfect linker
load test. Preparing the baseline:

```
# pulling in already built package into cache
$ nix build --no-link -f. pandoc
# pulling in all the build dependencies into cache
$ nix build --no-link -f. pandoc --rebuild

# timing the build:
$ time nix build --no-link -f. pandoc --rebuild
error: derivation '/nix/store/az6dbzm341jc7n4sw7w0ifspxgsm4093-pandoc-cli-3.7.0.2.drv' may not be deterministic: output '/nix/store/znmj21k8nrqc3hcax6yfy446g8bgk7z3-pandoc-cli-3.7.0.2' differs

real    0m12,850s
user    0m0,719s
sys     0m0,105s
```

Do not mind that determinism error. I got about `13 seconds` of the
package build time. Seems to match the original timing. Then I tried
`ld.bfd` by passing extra `--ghc-option=-optl-fuse-ld=bfd` option to
`./Setup configure`:

```
$ time nix build --impure --expr 'with import <nixpkgs> {};
    pandoc.overrideAttrs (oa: {
        configureFlags = oa.configureFlags ++ ["--ghc-option=-optl-fuse-ld=bfd"];})'
...
real    0m37,391s
user    0m0,691s
sys     0m0,120s
```

37 seconds! At least `3x` slowdown. I was glad to see such a simple
reproducer.

## linker performance profiles

I dropped into a development shell to explore individual `ld` commands:

```
$ nix develop --impure --expr 'with import <nixpkgs> {};
    pandoc.overrideAttrs (oa: {
        configureFlags = oa.configureFlags ++ ["--ghc-option=-optl-fuse-ld=bfd" ];})'
$$ genericBuild
...
[4 of 4] Linking dist/build/pandoc/pandoc
^C

$$ # ready for interactive exploration
```

I ran the `./Setup build -v` to extract exact `ghc --make ...` invocation:

```
$$ ./Setup build -v
...
Linking...
Running: <<NIX>>-ghc-9.10.3/bin/ghc --make -fbuilding-cabal-package -O -split-sections -static -outputdir dist/build/pandoc/pandoc-tmp -odir dist/build/pandoc/pandoc-tmp ...
```

And ran `ld.bfd` under `perf`:

```
$ perf record -g <<NIX>>-ghc-9.10.3/bin/ghc --make ... '-optl-fuse-ld=bfd' -fforce-recomp
```

Then I built the [`flamegraph`](https://github.com/brendangregg/FlameGraph) picture:

```
$ perf script > out.perf
$ perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded
$ perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > bfd.svg
```

[![`bfd.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/bfd.svg "ld.bfd profile on pandoc")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/bfd.svg)

You can click on the pictures and explore it interactively.
Does it look fine to you? Anything odd?
We already see a tiny hint: `_bfd_elf_gc_mark()` takes suspiciously large
amount of space on the picture.
Let's build the same picture for `gold` using `-optl-fuse-ld=gold` option:

```
$ perf record -g <<NIX>>-ghc-9.10.3/bin/ghc --make ... '-optl-fuse-ld=gold' -fforce-recomp
$ perf script > out.perf
$ perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded
$ perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > gold.svg
```

[![`gold.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/gold.svg "ld.gold profile on pandoc")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/gold.svg)

The profile looks more balanced. The staircase around sorting looks
peculiar. Most of the time is spent
on section sorting in `gold::Output_section::sort_attached_input_sections`.
It's also a great hint: **how many sections should there be so that sorting
alone would take `25%` of the link time**?

## section count

Where do these numerous sections come from?
`nixpkgs` enables [`-split-sections`](https://github.com/NixOS/nixpkgs/blob/54a538734a5e77bca43dcbe4ad20357b5eb1cffd/pkgs/development/haskell-modules/generic-builder.nix#L349C20-L349C45)
by default on `linux` which uses `ghc` [`-fsplit-sections`](https://downloads.haskell.org/ghc/9.12.2/docs/users_guide/phases.html#ghc-flag-fsplit-sections).
Those are very close in spirit to `gcc` [`-ffunction-sections`](https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html#index-ffunction-sections)
feature. Both place each function in a separate `ELF` section assuming
that the user will use `-Wl,--gc-sections` linker option to
garbage-collect unreferenced sections in the final executable to make
binaries smaller.

So how many sections do you normally expect per object file?

In `C` (with `-ffunction-sections`) I would expect section count to
be close to function count in the source
file. Some optimization passes duplicate (clone) or inline functions,
but the expansion should not be too large (famous last words). Hopefully
not `100x`, but closer to `1.5x` maybe? `C++` might be trickier to
reason about.

In `haskell` it's a lot more complicated: lazy evaluation model creates
numerous smaller functions out of one source function, cross-module
aggressive inlining brings in many expressions.
Here is an example of a one liner compilation and it's section count:

```haskell
-- # cat Main.hs
main = print "hello"
```

**Quiz question: how many sections do you expect to see for this source
file after `ghc -c Main.hs`?**

Building and checking unsplit form first:

```
$ ghc -c Main.hs -fforce-recomp

$ size Main.o
   text    data     bss     dec     hex filename
    378     304       0     682     2aa Main.o

$ readelf -SW Main.o
There are 13 section headers, starting at offset 0xb50:

Section Headers:
  [Nr] Name              Type            Address          Off    Size   ES Flg Lk Inf Al
  [ 0]                   NULL            0000000000000000 000000 000000 00      0   0  0
  [ 1] .text             PROGBITS        0000000000000000 000040 00013a 00  AX  0   0  8
  [ 2] .rela.text        RELA            0000000000000000 000708 0001c8 18   I 10   1  8
  [ 3] .data             PROGBITS        0000000000000000 000180 000130 00  WA  0   0  8
  [ 4] .rela.data        RELA            0000000000000000 0008d0 000210 18   I 10   3  8
  [ 5] .bss              NOBITS          0000000000000000 0002b0 000000 00  WA  0   0  1
  [ 6] .rodata.str       PROGBITS        0000000000000000 0002b0 000010 01 AMS  0   0  1
  [ 7] .note.GNU-stack   PROGBITS        0000000000000000 0002c0 000000 00      0   0  1
  [ 8] .comment          PROGBITS        0000000000000000 0002c0 00000c 01  MS  0   0  1
  [ 9] .note.gnu.property NOTE            0000000000000000 0002d0 000030 00   A  0   0  8
  [10] .symtab           SYMTAB          0000000000000000 000300 000228 18     11   5  8
  [11] .strtab           STRTAB          0000000000000000 000528 0001dd 00      0   0  1
  [12] .shstrtab         STRTAB          0000000000000000 000ae0 00006e 00      0   0  1
Key to Flags:
  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),
  L (link order), O (extra OS processing required), G (group), T (TLS),
  C (compressed), x (unknown), o (OS specific), E (exclude),
  D (mbind), l (large), p (processor specific)
```

378 bytes of code, 12 sections. 6 of them are relevant: `.text`, `.rela.text`,
`.data`, `.rela.data`, `.rodata.str`. All very close to a typical `C` program.

Now let's throw in `-fsplit-sections`.

**Quiz question: guess how many more sections there will be? 0? 1? 10? 100? 1000?**

```
$ ghc -c Main.hs -fforce-recomp -fsplit-sections

$ size Main.o
   text    data     bss     dec     hex filename
    365     304       0     669     29d Main.o

$ readelf -SW Main.o
There are 39 section headers, starting at offset 0xd90:

Section Headers:
  [Nr] Name              Type            Address          Off    Size   ES Flg Lk Inf Al
  [ 0]                   NULL            0000000000000000 000000 000000 00      0   0  0
  [ 1] .text             PROGBITS        0000000000000000 000040 000000 00  AX  0   0  1
  [ 2] .data             PROGBITS        0000000000000000 000040 000000 00  WA  0   0  1
  [ 3] .bss              NOBITS          0000000000000000 000040 000000 00  WA  0   0  1
  [ 4] .rodata.str..LrKs_bytes PROGBITS        0000000000000000 000040 000005 01 AMS  0   0  1
  [ 5] .rodata.str..LrKq_bytes PROGBITS        0000000000000000 000045 000005 01 AMS  0   0  1
  [ 6] .rodata.str.cKA_str PROGBITS        0000000000000000 00004a 000006 01 AMS  0   0  1
  [ 7] .data..LsKw_closure PROGBITS        0000000000000000 000050 000028 00  WA  0   0  8
  [ 8] .rela.data..LsKw_closure RELA            0000000000000000 0007c8 000030 18   I 36   7  8
  [ 9] .data..LuKL_srt   PROGBITS        0000000000000000 000078 000020 00  WA  0   0  8
  [10] .rela.data..LuKL_srt RELA            0000000000000000 0007f8 000048 18   I 36   9  8
  [11] .text..LsKu_info  PROGBITS        0000000000000000 000098 000062 00  AX  0   0  8
  [12] .rela.text..LsKu_info RELA            0000000000000000 000840 000090 18   I 36  11  8
  [13] .data..LsKu_closure PROGBITS        0000000000000000 000100 000020 00  WA  0   0  8
  [14] .rela.data..LsKu_closure RELA            0000000000000000 0008d0 000018 18   I 36  13  8
  [15] .data..LuL2_srt   PROGBITS        0000000000000000 000120 000028 00  WA  0   0  8
  [16] .rela.data..LuL2_srt RELA            0000000000000000 0008e8 000060 18   I 36  15  8
  [17] .text.Main_main_info PROGBITS        0000000000000000 000148 000069 00  AX  0   0  8
  [18] .rela.text.Main_main_info RELA            0000000000000000 000948 0000a8 18   I 36  17  8
  [19] .data.Main_main_closure PROGBITS        0000000000000000 0001b8 000020 00  WA  0   0  8
  [20] .rela.data.Main_main_closure RELA            0000000000000000 0009f0 000018 18   I 36  19  8
  [21] .data..LuLj_srt   PROGBITS        0000000000000000 0001d8 000020 00  WA  0   0  8
  [22] .rela.data..LuLj_srt RELA            0000000000000000 000a08 000048 18   I 36  21  8
  [23] .text.ZCMain_main_info PROGBITS        0000000000000000 0001f8 000062 00  AX  0   0  8
  [24] .rela.text.ZCMain_main_info RELA            0000000000000000 000a50 000090 18   I 36  23  8
  [25] .data.ZCMain_main_closure PROGBITS        0000000000000000 000260 000020 00  WA  0   0  8
  [26] .rela.data.ZCMain_main_closure RELA            0000000000000000 000ae0 000018 18   I 36  25  8
  [27] .data..LrKr_closure PROGBITS        0000000000000000 000280 000010 00  WA  0   0  8
  [28] .rela.data..LrKr_closure RELA            0000000000000000 000af8 000030 18   I 36  27  8
  [29] .data..LrKt_closure PROGBITS        0000000000000000 000290 000010 00  WA  0   0  8
  [30] .rela.data..LrKt_closure RELA            0000000000000000 000b28 000030 18   I 36  29  8
  [31] .data.Main_zdtrModule_closure PROGBITS        0000000000000000 0002a0 000020 00  WA  0   0  8
  [32] .rela.data.Main_zdtrModule_closure RELA            0000000000000000 000b58 000048 18   I 36  31  8
  [33] .note.GNU-stack   PROGBITS        0000000000000000 0002c0 000000 00      0   0  1
  [34] .comment          PROGBITS        0000000000000000 0002c0 00000c 01  MS  0   0  1
  [35] .note.gnu.property NOTE            0000000000000000 0002d0 000030 00   A  0   0  8
  [36] .symtab           SYMTAB          0000000000000000 000300 0002e8 18     37  13  8
  [37] .strtab           STRTAB          0000000000000000 0005e8 0001dd 00      0   0  1
  [38] .shstrtab         STRTAB          0000000000000000 000ba0 0001ea 00      0   0  1
Key to Flags:
  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),
  L (link order), O (extra OS processing required), G (group), T (TLS),
  C (compressed), x (unknown), o (OS specific), E (exclude),
  D (mbind), l (large), p (processor specific)
```

38 sections! If we ignore 6 irrelevant sections it's 32 relevant sections
compared to 6 relevant sections before.
Our actual `main` top-level function itself is hiding in `.*Main_main.*`
sections. You will notices a lot of them. And on top of that whatever `ghc`
managed to "float-out" outside the function.

This is unoptimized code. If we throw in `-O2` we will get this:

```
$ ghc -c Main.hs -fforce-recomp -fsplit-sections -O2

$ size Main.o
   text    data     bss     dec     hex filename
    306     304       0     610     262 Main.o

$ readelf -SW Main.o
There are 45 section headers, starting at offset 0x10d0:

Section Headers:
  [Nr] Name              Type            Address          Off    Size   ES Flg Lk Inf Al
  [ 0]                   NULL            0000000000000000 000000 000000 00      0   0  0
  [ 1] .text             PROGBITS        0000000000000000 000040 000000 00  AX  0   0  1
  [ 2] .data             PROGBITS        0000000000000000 000040 000000 00  WA  0   0  1
  [ 3] .bss              NOBITS          0000000000000000 000040 000000 00  WA  0   0  1
  [ 4] .rodata.str.Main_zdtrModule2_bytes PROGBITS        0000000000000000 000040 000005 01 AMS  0   0  1
  [ 5] .rodata.str.Main_zdtrModule4_bytes PROGBITS        0000000000000000 000045 000005 01 AMS  0   0  1
  [ 6] .rodata.str.Main_main5_bytes PROGBITS        0000000000000000 00004a 000006 01 AMS  0   0  1
  [ 7] .data.Main_main4_closure PROGBITS        0000000000000000 000050 000028 00  WA  0   0  8
  [ 8] .rela.data.Main_main4_closure RELA            0000000000000000 000a60 000030 18   I 42   7  8
  [ 9] .data..Lu1AB_srt  PROGBITS        0000000000000000 000078 000020 00  WA  0   0  8
  [10] .rela.data..Lu1AB_srt RELA            0000000000000000 000a90 000048 18   I 42   9  8
  [11] .text.Main_main3_info PROGBITS        0000000000000000 000098 000062 00  AX  0   0  8
  [12] .rela.text.Main_main3_info RELA            0000000000000000 000ad8 000090 18   I 42  11  8
  [13] .data.Main_main3_closure PROGBITS        0000000000000000 000100 000020 00  WA  0   0  8
  [14] .rela.data.Main_main3_closure RELA            0000000000000000 000b68 000018 18   I 42  13  8
  [15] .data.Main_main2_closure PROGBITS        0000000000000000 000120 000020 00  WA  0   0  8
  [16] .rela.data.Main_main2_closure RELA            0000000000000000 000b80 000048 18   I 42  15  8
  [17] .text.Main_main1_info PROGBITS        0000000000000000 000140 000032 00  AX  0   0  8
  [18] .rela.text.Main_main1_info RELA            0000000000000000 000bc8 000060 18   I 42  17  8
  [19] .data.Main_main1_closure PROGBITS        0000000000000000 000178 000028 00  WA  0   0  8
  [20] .rela.data.Main_main1_closure RELA            0000000000000000 000c28 000060 18   I 42  19  8
  [21] .text.Main_main_info PROGBITS        0000000000000000 0001a0 00001d 00  AX  0   0  8
  [22] .rela.text.Main_main_info RELA            0000000000000000 000c88 000030 18   I 42  21  8
  [23] .data.Main_main_closure PROGBITS        0000000000000000 0001c0 000010 00  WA  0   0  8
  [24] .rela.data.Main_main_closure RELA            0000000000000000 000cb8 000018 18   I 42  23  8
  [25] .text.Main_main6_info PROGBITS        0000000000000000 0001d0 000024 00  AX  0   0  8
  [26] .rela.text.Main_main6_info RELA            0000000000000000 000cd0 000030 18   I 42  25  8
  [27] .data.Main_main6_closure PROGBITS        0000000000000000 0001f8 000020 00  WA  0   0  8
  [28] .rela.data.Main_main6_closure RELA            0000000000000000 000d00 000048 18   I 42  27  8
  [29] .text.ZCMain_main_info PROGBITS        0000000000000000 000218 00001d 00  AX  0   0  8
  [30] .rela.text.ZCMain_main_info RELA            0000000000000000 000d48 000030 18   I 42  29  8
  [31] .data.ZCMain_main_closure PROGBITS        0000000000000000 000238 000010 00  WA  0   0  8
  [32] .rela.data.ZCMain_main_closure RELA            0000000000000000 000d78 000018 18   I 42  31  8
  [33] .data.Main_zdtrModule3_closure PROGBITS        0000000000000000 000248 000010 00  WA  0   0  8
  [34] .rela.data.Main_zdtrModule3_closure RELA            0000000000000000 000d90 000030 18   I 42  33  8
  [35] .data.Main_zdtrModule1_closure PROGBITS        0000000000000000 000258 000010 00  WA  0   0  8
  [36] .rela.data.Main_zdtrModule1_closure RELA            0000000000000000 000dc0 000030 18   I 42  35  8
  [37] .data.Main_zdtrModule_closure PROGBITS        0000000000000000 000268 000020 00  WA  0   0  8
  [38] .rela.data.Main_zdtrModule_closure RELA            0000000000000000 000df0 000048 18   I 42  37  8
  [39] .note.GNU-stack   PROGBITS        0000000000000000 000288 000000 00      0   0  1
  [40] .comment          PROGBITS        0000000000000000 000288 00000c 01  MS  0   0  1
  [41] .note.gnu.property NOTE            0000000000000000 000298 000030 00   A  0   0  8
  [42] .symtab           SYMTAB          0000000000000000 0002c8 000378 18     43   2  8
  [43] .strtab           STRTAB          0000000000000000 000640 00041a 00      0   0  1
  [44] .shstrtab         STRTAB          0000000000000000 000e38 000295 00      0   0  1
Key to Flags:
  W (write), A (alloc), X (execute), M (merge), S (strings), I (info),
  L (link order), O (extra OS processing required), G (group), T (TLS),
  C (compressed), x (unknown), o (OS specific), E (exclude),
  D (mbind), l (large), p (processor specific)
```

44 sections! A Lot.

Back to our `pandoc` binary. It just calls into `libHSpandoc.a` library
(`nixpkgs` uses static linking for `haskell` bits today). Linker would
have to wade through all it's sections to pick only used things.

**Quiz question: guess how many sections does `pandoc` library have?
100? 1000? 10000? A million? What is your guess?**

Let's count! On my system `pandoc` library happens to hide at
`<<NIX>>-pandoc-3.7.0.2/lib/ghc-9.10.3/lib/x86_64-linux-ghc-9.10.3-2870/pandoc-3.7.0.2-Af80LA3Iq30D5LRTMZUszs/libHSpandoc-3.7.0.2-Af80LA3Iq30D5LRTMZUszs.a`.
I'll just use that ugly path.

```
# dumping section count per individual object file in the archive:
$ readelf -h <<NIX>>-pandoc-3.7.0.2/lib/ghc-9.10.3/lib/x86_64-linux-ghc-9.10.3-2870/pandoc-3.7.0.2-Af80LA3Iq30D5LRTMZUszs/libHSpandoc-3.7.0.2-Af80LA3Iq30D5LRTMZUszs.a |
    grep 'Number of section'
  Number of section headers:         18
  ...
  Number of section headers:         26207
  Number of section headers:         16755
  ...
  Number of section headers:         8898
  ..
  Number of section headers:         1047
  Number of section headers:         1324
  ...
  Number of section headers:         687
  Number of section headers:         228

# summing up all section counts:
$ readelf -h <<NIX>>-pandoc-3.7.0.2/lib/ghc-9.10.3/lib/x86_64-linux-ghc-9.10.3-2870/pandoc-3.7.0.2-Af80LA3Iq30D5LRTMZUszs/libHSpandoc-3.7.0.2-Af80LA3Iq30D5LRTMZUszs.a |
    grep 'Number of section' | awk '{ size += $5 } END { print size }'
494450
```

`494` `thousands` sections! Almost half a million sections. And it's just
one (largest) of many `pandoc` dependencies. That's why `ld.gold` takes
a considerable amount of time to just sort through all these sections.

## performance hog clues

`ld.bfd` has even harder time getting through such a big list of sections.
But why exactly? The names
of `_bfd_elf_gc_mark / _bfd_elf_gc_mark_reloc` functions in the profiles
hint that they track unreferenced sections.

To trigger section garbage collection
`ghc` [uses `-Wl,--gc-sections`](https://github.com/ghc/ghc/blob/f9790ca81deb8b14ff2eabf701aecbcfd6501963/compiler/GHC/Linker/Static.hs#L241)
linker option.
Note that `ghc` only enables garbage collection for `GNU ld` (I think
both `ld.gold` and `ld.bfd` count as GNU). `lld` and `mold`
both the option as well.

If we look at the implementation
of `_bfd_elf_gc_mark` we see a suspicious
[list traversal](https://sourceware.org/git/?p=binutils-gdb.git;a=blob;f=bfd/elflink.c;h=91c77c211ef065a77883004eb696adacd92a00be;hb=815d9a14cbbb3b81843f7566222c87fb22e7255d#l14063)
and some recursive descend:

```c
bool
_bfd_elf_gc_mark_reloc (struct bfd_link_info *info,
                        asection *sec,
                        elf_gc_mark_hook_fn gc_mark_hook,
                        struct elf_reloc_cookie *cookie)
{
  asection *rsec;
  bool start_stop = false;

  rsec = _bfd_elf_gc_mark_rsec (info, sec, gc_mark_hook, cookie, &start_stop);
  while (rsec != NULL)
    {
      if (!rsec->gc_mark)
        {
          if (bfd_get_flavour (rsec->owner) != bfd_target_elf_flavour
              || (rsec->owner->flags & DYNAMIC) != 0)
            rsec->gc_mark = 1;
          else if (!_bfd_elf_gc_mark (info, rsec, gc_mark_hook))
            return false;
        }
      if (!start_stop)
        break;
      rsec = bfd_get_next_section_by_name (rsec->owner, rsec);
    }
  return true;
}
```

If we do such marking for each section it probably has quadratic complexity.
But maybe not. To get something simpler to explore I tried to craft a
trivial example of `1 million` sections:

```
$ for (( i=0; i<1000000; i++ )); do printf "int var_$i __attribute__ ((section (\".data.$i\"))) = { $i };\n"; done > main.c; printf "int main() {}" >> main.c; gcc -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd; echo "gold:"; time gcc main.o -o main -fuse-ld=gold
bfd:

real    0m6,123s
user    0m5,384s
sys     0m0,701s
gold:

real    0m1,107s
user    0m0,844s
sys     0m0,242s
```

This test generates the following boilerplate code:

```c
// $ head -n 5 main.c
int var_0 __attribute__ ((section (".data.0"))) = { 0 };
int var_1 __attribute__ ((section (".data.1"))) = { 1 };
int var_2 __attribute__ ((section (".data.2"))) = { 2 };
int var_3 __attribute__ ((section (".data.3"))) = { 3 };
int var_4 __attribute__ ((section (".data.4"))) = { 4 };
// ...
// $ tail -n 5 main.c
int var_999996 __attribute__ ((section (".data.999996"))) = { 999996 };
int var_999997 __attribute__ ((section (".data.999997"))) = { 999997 };
int var_999998 __attribute__ ((section (".data.999998"))) = { 999998 };
int var_999999 __attribute__ ((section (".data.999999"))) = { 999999 };
int main() {}
```

We are seeing `6x` time difference between linkers. Could it be our case?
Let's check with `perf` if we hit the same hot paths as in `pandoc` case:

```
$ perf record -g gcc main.o -o main -fuse-ld=bfd
$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > try1.svg
```

[![`try1.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/try1.svg "ld.bfd profile on synthetic 1M independent data sections")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/try1.svg)

The profile looks not too bad: no large `.*_gc_.*` bits seen anywhere.
Not exactly our problem then. Let's try to add more references across
sections to see if we start seeing the symbol traversals:

```
$ printf "int var_0 __attribute__ ((section (\".data.0\"))) = { $i };\n" > main.c; for (( i=1; i<1000000; i++ )); do printf "void * var_$i __attribute__ ((section (\".data.$i\"))) = { &var_$((i-1)) };\n"; done >> main.c; printf "int main() { return (long)var_99999; }" >> main.c; gcc -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections -Wl,--no-as-needed; echo "gold:"; time gcc main.o -o main -fuse-ld=gold -Wl,--gc-sections -Wl,--no-as-needed
gcc: internal compiler error: Segmentation fault signal terminated program cc1
Please submit a full bug report, with preprocessed source (by using -freport-bug).
See <https://gcc.gnu.org/bugs/> for instructions.
```

Whoops, crashed `gcc`. Filed [`PR122198`](https://gcc.gnu.org/PR122198).
It's a stack overflow. Throwing more stack at the problem with `ulimit -s unlimited`:

```
$ printf "int var_0 __attribute__ ((section (\".data.0\"))) = { $i };\n" > main.c; for (( i=1; i<1000000; i++ )); do printf "void * var_$i __attribute__ ((section (\".data.$i\"))) = { &var_$((i-1)) };\n"; done >> main.c; printf "int main() { return (long)var_99999; }" >> main.c; gcc -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections -Wl,--no-as-needed; echo "gold:"; time gcc main.o -o main -fuse-ld=gold -Wl,--gc-sections -Wl,--no-as-needed
bfd:

real    0m5.296s
user    0m4.572s
sys     0m0.714s
gold:

real    0m1.172s
user    0m0.874s
sys     0m0.295s
```

This test generates slightly different form:

```c
// $ head -n 5 main.c
int var_0 __attribute__ ((section (".data.0"))) = { 1000000 };
void * var_1 __attribute__ ((section (".data.1"))) = { &var_0 };
void * var_2 __attribute__ ((section (".data.2"))) = { &var_1 };
void * var_3 __attribute__ ((section (".data.3"))) = { &var_2 };
void * var_4 __attribute__ ((section (".data.4"))) = { &var_3 };
// ...
// $ tail -n 5 main.c
void * var_999996 __attribute__ ((section (".data.999996"))) = { &var_999995 };
void * var_999997 __attribute__ ((section (".data.999997"))) = { &var_999996 };
void * var_999998 __attribute__ ((section (".data.999998"))) = { &var_999997 };
void * var_999999 __attribute__ ((section (".data.999999"))) = { &var_999998 };
int main() { return (long)var_99999; }
```

The main difference compared to the previous attempt is that sections are used
and can't be removed by section garbage collector. Is this profile closer to
`pandoc` case? Looking at the trace:

```
$ perf record -g gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections -Wl,--no-as-needed
$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > try2.svg
```

[![`try2.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/try2.svg "ld.bfd profile on 1M interdependent data sections")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/try2.svg)

It's not too close to `pandoc` case, but the huge vertical thing on the
left is a good sign that we at least start hitting the `gc` traversal
code. Now we need to increase its presence somehow.
Perhaps it's more symbols per section? I tried to simulate code
references instead of data references and see what happens:

```
$ printf "int f_0() __attribute__ ((section (\".text.0\"))); int f_0() { return 0; };\n" > main.c; for (( i=1; i<20000; i++ )); do printf "int f_$i() __attribute__ ((section (\".text.$i\"))); int f_$i() { return f_$((i-1))(); };\n"; done >> main.c; printf "int main() { return f_19999(); }" >> main.c; gcc -O0 -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections -Wl,--no-as-needed; echo "gold:"; time gcc main.o -o main -fuse-ld=gold -Wl,--gc-sections -Wl,--no-as-needed
bfd:

real    0m5,627s
user    0m2,567s
sys     0m3,047s
gold:

real    0m0,053s
user    0m0,041s
sys     0m0,012s
```

How the test generates this file:

```c
// $ head -n 5 main.c
int f_0() __attribute__ ((section (".text.0"))); int f_0() { return 0; };
int f_1() __attribute__ ((section (".text.1"))); int f_1() { return f_0(); };
int f_2() __attribute__ ((section (".text.2"))); int f_2() { return f_1(); };
int f_3() __attribute__ ((section (".text.3"))); int f_3() { return f_2(); };
int f_4() __attribute__ ((section (".text.4"))); int f_4() { return f_3(); };
// ..
// $ tail -n 5 main.c
int f_19996() __attribute__ ((section (".text.19996"))); int f_19996() { return f_19995(); };
int f_19997() __attribute__ ((section (".text.19997"))); int f_19997() { return f_19996(); };
int f_19998() __attribute__ ((section (".text.19998"))); int f_19998() { return f_19997(); };
int f_19999() __attribute__ ((section (".text.19999"))); int f_19999() { return f_19998(); };
int main() { return f_19999(); }
```

That time difference is more interesting! Note that `ld.gold` spends just
`50ms` on the input while `ld.bfd` does something for `5 seconds`!
Getting the profile:

```
$ perf record -g gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections -Wl,--no-as-needed
$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > try3.svg
```

[![`try3.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/try3.svg "ld.bfd trace on 20K dependent text sections")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/try3.svg)

At last! We managed to hit exactly the `_bfd_elf_gc_mark ()`.
The final input file object file is not too large:

```
$ ls -lh main.o
-rw-r--r-- 1 slyfox users 5.7M Oct  8 20:58 main.o
```

I filed [`PR33530`](https://sourceware.org/PR33530) upstream bug report
hoping that fix will not be too complicated and started writing this
blog post.

## testing the patch

My plan was to figure out more details about `binutils` `gc` in this post
and try to fix it. Alas even before I got to it H.J. already prepared
[the fix](https://sourceware.org/PR33530#c1)!

Synthetic test shown great results:

```
$ printf "int f_0() __attribute__ ((section (\".text.0\"))); int f_0() { return 0; };\n" > main.c; for (( i=1; i<20000; i++ )); do printf "int f_$i() __attribute__ ((section (\".text.$i\"))); int f_$i() { return f_$((i-1))(); };\n"; done >> main.c; printf "int main() { return f_19999(); }" >> main.c; gcc -O0 -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections
bfd:

real    0m0,119s
user    0m0,080s
sys     0m0,038s
```

`120ms` compared to the previous `5s` is a `40x` speedup. It's still
twice as slow as `50ms` for `ld.gold`, but the absolute time is way
harder to notice. I tried to find a new degradation point by adding more
functions:

```
# 100K sections:
$ printf "int f_0() __attribute__ ((section (\".text.0\"))); int f_0() { return 0; };\n" > main.c; for (( i=1; i<100000; i++ )); do printf "int f_$i() __attribute__ ((section (\".text.$i\"))); int f_$i() { return f_$((i-1))(); };\n"; done >> main.c; printf "int main() { return f_99999(); }" >> main.c; gcc -O0 -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections
bfd:

real    0m0.628s
user    0m0.472s
sys     0m0.154s

# 1M sections:
$ printf "int f_0() __attribute__ ((section (\".text.0\"))); int f_0() { return 0; };\n" > main.c; for (( i=1; i<1000000; i++ )); do printf "int f_$i() __attribute__ ((section (\".text.$i\"))); int f_$i() { return f_$((i-1))(); };\n"; done >> main.c; printf "int main() { return f_999999(); }" >> main.c; gcc -O0 -c main.c -o main.o; echo "bfd:"; time gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections
bfd:

real    0m8.697s
user    0m6.956s
sys     0m1.726s

$ size main.o
   text    data     bss     dec     hex filename
43000115              0       0 43000115        2902133 main.o
```

`8s` on a file with `1M` sections sounds quite fast! Let's see where
`ld.bfd` spends its time now:

```
$ perf record -g gcc main.o -o main -fuse-ld=bfd -Wl,--gc-sections
$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > fixed.svg
```

[![`fixed.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/fixed.svg "fixed ld.bfd profile on 1M dependent text sections")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/fixed.svg)

Once again the profile looks more balanced now. Yay! How about real
`pandoc`?

```
$ time nix build --no-link -f. pandoc --rebuild

real    0m17,013s
user    0m0,672s
sys     0m0,123s
```

`17s` is a bit slower than `13s` of `ld.gold`, but not as bad as it used
to be. And it's profile:

```
$ perf record -g <<NIX>>-ghc-9.10.3/bin/ghc --make ... '-optl-fuse-ld=bfd' -fforce-recomp

$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > fixed-pandoc.svg
```

[![`fixed-pandoc.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/fixed-pandoc.svg "fixed ld.bfd profile on 1M dependent text sections")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/fixed-pandoc.svg)

## bonus: other linkers

Both `ld.gold` and `ld.lld` are quite fast at handling synthetic tests now,
But both still spend a few seconds on `pandoc`. How about other linkers?
Let's add `lld` and `mold` to the mix. I'll measure
`ghc --make '-optl-fuse-ld=$linker' -fforce-recomp` execution time:

```
# without the fix
$ time <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=bfd -fforce-recomp
real    0m30.589s user    0m24.576s sys     0m6.447s

# with the fix
$ time <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=bfd -fforce-recomp
real    0m8.676s user    0m6.484s sys     0m2.584s

$ time <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=gold -fforce-recomp
real    0m5.929s user    0m5.543s sys     0m0.829s

$ time <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=lld -fforce-recomp
real    0m1.413s user    0m1.754s sys     0m1.509s

$ time <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=mold -fforce-recomp
real    0m1.209s user    0m0.424s sys     0m0.215s
```

Note: we do measure not just liker time, but also `ghc` code generation
time. Actual link time does not take as much. Same values in tables:

| linker           | `real` (sec) | `user` (sec) | `sys` (sec) |
| --               | --   | --   | --  |
| `ld.bfd` (orig)  | 30.6 | 24.6 | 6.4 |
| `ld.bdf` (fixed) | 8.7  | 6.5  | 2.6 |
| `ld.gold`        | 5.9  | 5.5  | 0.8 |
| `lld`            | 1.4  | 1.8  | 1.5 |
| `mold`           | 1.2  | 0.4  | 0.2 |

`lld` and `mold` link times are impressive! They are `6x-7x` times faster
than fixed version of `ld.bfd`. Let's look at their profiles to see what
they spend their time on. `lld` goes first:

```
$ perf record -g <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=lld -fforce-recomp

$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > lld-pandoc.svg
```

[![`lld-pandoc.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/lld-pandoc.svg "lld profile on pandoc")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/lld-pandoc.svg)

The only things that stick out in this profile are `malloc()` (about `20%`
of the time?) and `memmove()` calls (about `6%` of the time). Otherwise,
we see about equal time spent on reading (`linkerDriver` part on the right),
relocation processing (`RelocationScanner` in the middle) and writing
(`HashTableSection::writeTo` slightly to the left).

I wonder if memory management were to be optimized for `lld`, would it be
as fast as `mold` on this input? And now `mold` profile:

```
$ perf record -g <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=mold -fforce-recomp

$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > mold-pandoc.svg
```

[![`mold-pandoc.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/mold-pandoc.svg "mold profile on pandoc")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/mold-pandoc.svg)

The profile is not very readable as we mainly see the `tbb` threading
dispatch of chunks of work to do. If I scroll around I see
things like `scan_abs_relocations`, `apply_reloc_alloc`, `resolve_symbols`,
`split_contents`, `scan_relocations`. It looks like half the time
is spent on the management of parallelism. Playing a bit with the
`mold` parameters I noticed that `-Wl,--threads=4` is the maximum thread
count where I get any speed improvements for parallelism. Anything above
will clutter CPU usage profile with `sched_yield` "busy" wait threads.

To get the idea where actual CPU is spent it might be more interesting to
look at single-threaded profile using `-optl-Wl,--no-threads`:

```
$ perf record -g <<NIX>>-ghc-9.10.3/bin/ghc --make ... pandoc ... -optl-fuse-ld=mold -optl-Wl,--no-threads -fforce-recomp

$ perf script > out.perf && perl ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded && perl ~/dev/git/FlameGraph/flamegraph.pl out.folded > mold-no-threads.svg
```

[![`mold-pandoc-no-threads.svg`](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/mold-pandoc-no-threads.svg "mold profile on pandoc single thread")](/posts.data/340-profiling-binutils-linkers-in-nixpkgs/mold-pandoc-no-threads.svg)

I'll leave the interpretation of the picture to the reader.

## parting words

`ld.gold` is about to be removed from `binutils`.

In `-fsplit-sections` mode `ghc` code generator produces huge amount of
sections per `ELF` file which manages to strain both `ld.bfd` and
`ld.gold` linkers. `ghc` should probably be fixed to produce one section
per strongly connected component of functions that refer one another.
`libHSpandoc` consists of almost half a million `ELF` sections. Average
section size for which is about 250 bytes.

`ld.bfd` while being slower than `ld.gold` still has simple performance
bugs in obscure scenarios. Just like today's
[`PR33530`](https://sourceware.org/PR33530) example.

`binutils` upstream was very fast to come up with a possible fix to test.

Even fixed `ld.bfd` is still quite a bit slower on synthetic test with
huge sections compared to `ld.gold`. But at least it's not exponentially
worse.

`lld` and `mold` are still way faster than either `ld.bfd` or `ld.gold`.
About 6-7 times on `pandoc` test. `ld.bfd` still has a lot of room for
improvement :)

Have fun!
