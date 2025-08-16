---
title: "Zero Hydra Failures towards 23.05 NixOS release"
date: May 10, 2023
---

## `ZHF`

By the end of May `NixOS-23.05` will be released. Current development
phase is called Zero Hydra Failures: at this time the main focus is to
fix as many build failures in `nixpkgs/master` repository as possible.

[`Issue#230712`](https://github.com/NixOS/nixpkgs/issues/230712) tracks
the effort. It has hints on how to locate all known build failures.
So far there is a few thousands build failures in `nixpkgs/master`.

`ZHF` is a great time to contribute to `nixpkgs`! Let's pick a failed
package and try to fix it.

## `libfsm` example

[`trunk jobset`](https://hydra.nixos.org/jobset/nixpkgs/trunk) shows us
about ~2500 build failures. I'll pick an obscure `libfsm`
[failure](https://hydra.nixos.org/log/1d8dcs7b47ibrn183yn0k7sj8ghiwich-libfsm-0.1pre2442_9c5095f7.drv)
and will try to fix it. Full build log is reasonably short:

```
unpacking sources
unpacking source archive /nix/store/bjdkwx4rpbqb2ny9wx7qs935b4nmslhv-source
source root is source
patching sources
configuring
no configure script, doing nothing
building
build flags: -j2 SHELL=/nix/store/ywi6kzrk88zl22jvazdnlfaf9rqrj2aq-bash-5.2-p15/bin/bash -r PREFIX=\$\(out\)
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 93: Inconsistent operator for all
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 93: Inconsistent operator for clean
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/init.mk" line 90: Inconsistent operator for all
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 93: Inconsistent operator for all
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 93: Inconsistent operator for clean
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/init.mk" line 90: Inconsistent operator for all
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 102: Inconsistent operator for all
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/prog.mk" line 140: Inconsistent operator for all
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 102: Inconsistent operator for all
bmake[1]: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/final.mk" line 18: Inconsistent operator for install
bmake[1]: Fatal errors encountered -- cannot continue
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/srctop.mk" line 60: warning: "cd /build/source && MAKESYSPATH=/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk bmake -V .OBJDIR" returned non-zero status
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 102: Inconsistent operator for all
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/obj.mk" line 46: Malformed conditional (${MK_AUTO_OBJ} == "yes")
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/dpadd.mk" line 330: Malformed conditional (${.MAKE.MODE:Mmeta*} != "" && exists(${.MAKE.DEPENDFILE}))
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/prog.mk" line 140: Inconsistent operator for all
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/subdir.mk" line 102: Inconsistent operator for all
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/final.mk" line 11: Malformed conditional (${MK_STAGING} == "yes")
bmake: "/nix/store/f488ivld88vqd3avw1p8x9lm027h60lr-bmake-20230126/share/mk/final.mk" line 18: Inconsistent operator for install
bmake: Fatal errors encountered -- cannot continue
bmake: stopped in /build/source
```

It's not a big build log. And yet I'm completely clueless what goes
wrong. [Build tab](https://hydra.nixos.org/build/219125675) tells us
that last successful build of `libfsm` was around `2023-01-14 21:00:15`
on `3a29a0b2aa4aad61d8a80969cc8c386ad548c44c` `nixpkgs` input.

It's enough to bisect it down to a suspicious commit. Maybe it will
provide us more clues. Bisecting `nixpkgs` using `$ nix build -f. libfsm`
as a success criteria gives us this suspect:

```
commit f799d93ac179f8dd7b06d867d129fc6e7498c4fd
Date:   Sat Jan 28 23:02:19 2023 -0300

    bmake: 20220928 -> 20230126

 pkgs/development/tools/build-managers/bmake/default.nix | 8 ++++----
 1 file changed, 4 insertions(+), 4 deletions(-)
```

Tracing [`commit f799d93`](https://github.com/NixOS/nixpkgs/commit/f799d93ac179f8dd7b06d867d129fc6e7498c4fd)
back to a `PR` shows us [`PR#213276`](https://github.com/NixOS/nixpkgs/pull/213276) in
the web UI. There are no comments about `libfsm` breakage.

Looks like `bmake` changed `Makefile` rule handling and broke something.

[`bmake` home page](https://www.crufty.net/help/sjg/bmake.html) has no
changelog and suggests using version control system directly to look for
changes. Let's run `git shortlog`:

```
$ cd ~/dev/git/NetBSD/src
$ git shortlog 0f816dc9b17b3e254d50e477c4594fc61580e889..f6f18f84e68de4c08aedab76898ad11129ff4cdf -- usr.bin/make | cat
christos (1):
      Accept whitespace between command specifiers @+- like gmake does. New binutils does this.

rillig (28):
      make: merge duplicate code
      tests/make: make comment in test for .for easier to understand
      make: use consistent variable names in parser
      make: constify, fix comment indentation
      make.1: clean up style, typography, accuracy
      make: document the guard for directories in unlink_file
      make: change return type of unlink_file back to int
      make.1: make horizontal and vertical spacing more consistent
      tests/make: document that the ':tl' modifier does not split words
      make.1: clarify in which case an expression may omit braces
      make: inline local macro in GNodeFlags_ToString
      make: inline LazyBuf_AddBytesBetween
      make: clean up comments
      make: clean up comments
      make: don't assemble identifiers from smaller tokens
      make.1: reduce indentation of the long list of variable names
      make.1: move description of .MAKE.MODE below the .MAKE.META block
      make.1: use consistent markup for boolean flags
      make.1: sort list of built-in variables
      make.1: sync list of built-in variables with reality
      make.1: fix markup
      make.1: bump date
      make: add more details to warning 'Extra targets ignored'
      tests/make: test backslash-newline after macro expansion in command
      tests/make: add tests for whitespace in leading '@+-'
      make: inline macro for variable name
      tests/make: remove dependency on expr(1) from a test
      tests/make: rename files that are not test cases

sjg (5):
      make: .[NO]READONLY for control of read-only variables
      make: log adjustments of var.readOnly
      Add the unit tests
      make: .SYSPATH: to add dirs to sysIncPath
      make: some variables should be read-only
```

It looks like the only change that could affect `bmake` behavior is
something around read-only variables. Before investigating more let's
try to update `bmake` first:

```diff
--- a/pkgs/development/tools/build-managers/bmake/default.nix
+++ b/pkgs/development/tools/build-managers/bmake/default.nix
@@ -10,11 +10,11 @@

 stdenv.mkDerivation (finalAttrs: {
   pname = "bmake";
-  version = "20230126";
+  version = "20230414";

   src = fetchurl {
     url = "http://www.crufty.net/ftp/pub/sjg/${finalAttrs.pname}-${finalAttrs.version}.tar.gz";
-    hash = "sha256-hk9yGFgs95Dsc7ILcQVCXLn/ozUiJUF3LwMTMGtqC8Q=";
+    hash = "sha256-KcsdJqrn3p3vkr2us6rUUg6JlRzpey518LibrhuVOZ8=";
   };

   # Make tests work with musl
```

I proposed `bmake` update as [`PR#231027`](https://github.com/NixOS/nixpkgs/pull/231027).
Is it enough to fix `libfsm`? Trying to build:

```
$ nix build -f. libfsm -L
...
libfsm> bmake: "/nix/store/gbbmrdcz1z7808gds3ddyf2ywi67113g-bmake-20230414/share/mk/final.mk" line 11: Malformed conditional (${MK_STAGING} == "yes")
libfsm> bmake: "/nix/store/gbbmrdcz1z7808gds3ddyf2ywi67113g-bmake-20230414/share/mk/final.mk" line 18: Inconsistent operator for install
libfsm> bmake: Fatal errors encountered -- cannot continue
libfsm> bmake: stopped in /build/source
```

No difference. Maybe `libfsm` already fixed it upstream?

Looking at `Makefile` [project history](https://github.com/katef/libfsm/commits/main/Makefile)
there were a few fixes recently that fix compatibility with newer `bmake`.

Let's update `libfsm` then!

```diff
--- a/pkgs/development/libraries/libfsm/default.nix
+++ b/pkgs/development/libraries/libfsm/default.nix
@@ -4,13 +4,13 @@

 stdenv.mkDerivation rec {
   pname = "libfsm";
-  version = "0.1pre2442_${builtins.substring 0 8 src.rev}";
+  version = "0.1pre2987_${builtins.substring 0 8 src.rev}";

   src = fetchFromGitHub {
     owner  = "katef";
     repo   = pname;
-    rev    = "9c5095f7364fa464efff6c81fad9b60b19dfcc99";
-    sha256 = "1bs51agvrrwqid0slq2svj2yj7kkjdsnv3xsrk8zmf1jbgza6jrm";
+    rev    = "087e3389ad2cd5e5c40caeb40387e632567d7258";
+    hash   = "sha256-XWrZxnRbMB609l+sYFf8VsXy3NxqBsBPUrHgKLIyu/I=";
     fetchSubmodules = true;
   };
```

Trying to use:

```
$ nix build --no-link -L -f. libfsm
...
libfsm> install -m 644 build/pc/libre.pc /nix/store/v33dns98gw8mw0sqvjjp1gvjcaq0p2n2-libfsm-0.1pre2987_087e3389/share/pkgconfig/libre.pc
libfsm> install: cannot stat 'build/pc/libre.pc': No such file or directory
libfsm> *** [install] Error code 1
```

Still fails. But at least the failure looks different enough from initial
problem. Phis looks more like a missing expected file. It's a parallel
install failure. Disabling that I got a working package with
[`PR#231029`](https://github.com/NixOS/nixpkgs/pull/231029).

During the review (by running `nix-review pr 231029`) I found a similar
failure in `kgt`. It is known upstream as
[`Issue#62`](https://github.com/katef/kgt/issues/62). Fixing it is left
as an exercise for the reader.

## Parting words

We squashed at least one `ZHF` problem and as a bonus updated one related
package. Many build fixes are trivial. If you are wondering if you
should try or not give it a go!

Some rare packages are broken beyond repair and are not worth keeping
around. You can mark those as `broken = true;` and those will be cleaned
up after a while if nobody steps up to fix them.

Have fun!
