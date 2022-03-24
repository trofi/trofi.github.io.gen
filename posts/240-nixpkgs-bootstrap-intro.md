---
title: "nixpkgs bootstrap intro"
date: March 24, 2022
---

This post is an informal walk through bootstrap setup of **nixpkgs**.
It's quite long and has many facets we look at here. If you don't
understand some bits of it don't worry: it's both a complicated topic
and I did a bad job at explaining it.

Initially I tried to cram everything into a single post and got
largest post ever. Once I realized it became unreadable I moved out
details on **glibc** into a separate [hacking-on-glibc post](/posts/239-hacking-on-glibc.html).
Now this post is "only" on par with other larges posts /o\\.

Good luck :)

# Intro

**nixpkgs** is a package repository for **NixOS** linux distribution.
**nixpkgs** can also be used outside **NixOS** on other **linux** (and
non-**linux**!) distributions.

Each **nixpkgs** package is built in a container environment where
build process sees only explicitly specified dependencies (via mount
namespace). That way we get more deterministic (and ideally fully
reproducible) build environment and result when ran on another machine.

Such a setup makes it trivial to notice missing required dependencies.
I frequently write a **nixpkgs** recipe for a package before trying
to package it on another distributions :)

Precise dependencies sound great in theory, but how does such a system
deal with circular dependencies and bootstrap dependencies? For example
**gcc** depends on some C compiler presence. How do they get satisfied?

# Bootstrap binaries

**nixpkgs** solves it by providing a set of pre-built seed binaries
called **bootstrap-files** (or **bootstrapTools**). These binaries were
at some previous point built on an already working **nixpkgs** system.
One can also build them manually.

Seed binaries don't change until someone decides to refresh them.
Specifically they don't get rebuil on each **nixpkgs** commit. In theory
**bootstrap-files** could be left untouched forever at least for existing
target systems. In practice bugs do occasinally happen in **bootstrap-files**
and we need to fix them. It's also useful to have some up-to-date
baseline when building other fresh packages: building **gcc-11**
with **gcc-8** (seed binary) is a lot simpler than building **gcc-11**
with **gcc-3**.

To get the idea what it takes to bet a set of bootstrap files let's build
fresh set of them ourselves. We'll use default definition in
[make-bootstrap-tools.nix](https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/linux/make-bootstrap-tools.nix):

```shell
$ nix build -f ./pkgs/stdenv/linux/make-bootstrap-tools.nix bootstrapFiles
```

We can even cross-compile bootstrap files for a differen (potentially not yet
supported) target:

```shell
$ nix build -f ./pkgs/stdenv/linux/make-bootstrap-tools.nix bootstrapFiles --argstr crossSystem powerpc64le-linux
```

Is it big? Inspecting the final result:

```shell
$ readlink result
/nix/store/3xq6in2gn3z3cvjjf51fyn53bg3k4nh6-bootstrap-tools.tar.xz

$ LANG=C ls -lh /nix/store/3xq6in2gn3z3cvjjf51fyn53bg3k4nh6-bootstrap-tools.tar.xz
-r--r--r-- 2 root root 21M Jan  1  1970 /nix/store/3xq6in2gn3z3cvjjf51fyn53bg3k4nh6-bootstrap-tools.tar.xz
```

Archive size is 21MB. This size is smaller than compressed **nixpkgs**
tree (~26MB today). Let's peek at things that hide inside:

```shell
$ tar --list -f /nix/store/3xq6in2gn3z3cvjjf51fyn53bg3k4nh6-bootstrap-tools.tar.xz
./bin/
./bin/[
./bin/ar
...
./bin/yes
./include/
./include/c++/
./include/c++/10.3.0/
./include/c++/10.3.0/algorithm
...
./include-glibc/
./include-glibc/a.out.h
./include-glibc/aio.h
...
./include-glibc/xen/privcmd.h
./lib/
./lib/crt1.o
./lib/crti.o
./lib/crtn.o
./lib/gcc/
./lib/gcc/x86_64-unknown-linux-gnu/
./lib/gcc/x86_64-unknown-linux-gnu/10.3.0/
...
./lib/gcc/x86_64-unknown-linux-gnu/10.3.0/libgcov.a
./lib/ld-2.33.so
./lib/ld-linux-x86-64.so.2
./lib/libbfd-2.35.2.so
./lib/libbfd.la
...
./lib/libz.so.1.2.11
./libexec/
./libexec/gcc/
./libexec/gcc/x86_64-unknown-linux-gnu/
./libexec/gcc/x86_64-unknown-linux-gnu/10.3.0/
./libexec/gcc/x86_64-unknown-linux-gnu/10.3.0/cc1
...
```

The contents (once again) are defined by
<https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/linux/make-bootstrap-tools.nix>.
It tells ut that the following packages are present in the final tarball:

- **busybox** (statically linked against **musl**)
- **glibc**
- **gcc** (this package and below are dynamically linked against **glibc**)
- **binutils**
- **coreutils**
- **tar**
- **bash**
- **findutils**
- **diffutils**
- **sed**
- **grep**
- **awk**
- **gzip**
- **bzip**
- **patch**
- **patchelf**
- **gmp**
- **mpfr**
- **mpc**
- **zlib**
- **isl**
- **libelf**

Just 21 package! Most are very cross-compiler friendly. Some of packages
have reduced functionality not needed for simplest build requirements:

```
  coreutilsMinimal = coreutils.override (args: { aclSupport = false; attrSupport = false; /*...*/ })
  tarMinimal = gnutar.override { acl = null; };
  busyboxMinimal = busybox.override { useMusl = true; enableStatic = true; /*...*/ }
  bootGCC = gcc.cc.override { enableLTO = false; };
  bootBinutils = binutils.bintools.override { withAllTargets = false; gold = false; enableShared = false; /*...*/ }
```

The tarball generaion process is literally copying build files to make
self-contained archive:

```
  build = stdenv.mkDerivation {
    name = "stdenv-bootstrap-tools";

    buildCommand = ''
        set -x
        mkdir -p $out/bin $out/lib $out/libexec

        # Copy what we need of Glibc.
        cp -d ${libc.out}/lib/ld*.so* $out/lib
        cp -d ${libc.out}/lib/libc*.so* $out/lib
        # ...
        cp -d ${coreutilsMinimal.out}/bin/* $out/bin
        (cd $out/bin && rm vdir dir sha*sum pinky factor pathchk runcon shuf who whoami shred users)
        cp ${bash.out}/bin/bash $out/bin
        cp ${findutils.out}/bin/find $out/bin
        cp ${findutils.out}/bin/xargs $out/bin
        cp -d ${diffutils.out}/bin/* $out/bin
        # ...
        nuke-refs $out/bin/*
        nuke-refs $out/lib/*
        # ...
    '';
    //...
```

Once these bootstrap binaries are built they are referred explicitly
as a **fetchurl{}** "source" tarball input:
<https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/linux/bootstrap-files/i686.nix>

There is a caveat: we can't run these binaries as is if we just unpack the tarball.
I'll try anyway:

```shell
$ mkdir /tmp/b
$ cd /tmp/b
$ tar xf /nix/store/3xq6in2gn3z3cvjjf51fyn53bg3k4nh6-bootstrap-tools.tar.xz
$ ls bin/bash
bin/bash
$ unshare --user --map-root-user chroot . /bin/bash
chroot: failed to run command ‘/bin/bash’: No such file or directory
```

It happens because binaries intentionally hardcode invalid absolute paths to dynamic linker:

```shell
$ lddtree bin/bash
bash => bin/bash (interpreter => /nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.33-108/lib/ld-linux-x86-64.so.2)
    libdl.so.2 => not found
    libc.so.6 => not found
```

These invalid paths are meant to be relocated at install time: **patchelf**
binary patching tool is used for that in
<https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/linux/bootstrap-tools/scripts/unpack-bootstrap-tools.sh>

Note that **patchelf** is also a dynamically linked binary. How can we run it against
broken interpreter? You might have already noticed it in **unpack-bootstrap-tools.sh** above:
dynamic loader is called by absolute path and it's search paths are overridden
by **LD_LIBRARY_PATH** variable.

Trying running **bash** using the same trick:

```shell
$ PS1='foo> ' unshare --user --map-root-user chroot . /lib/ld-linux-x86-64.so.2 --library-path /lib /bin/bash

foo> echo /*
/bin /include /include-glibc /lib /libexec

foo> ls /
bash: ls: command not found
foo> LD_LIBRARY_PATH=/lib /lib/ld-linux-x86-64.so.2 /bin/ls
bin  include  include-glibc  lib  libexec
```

Seems to work :)

I used **\-\-library-path /lib** to step aside the complications of mixing
**LD_LIBRARY_PATH** value for host's **chroot** command. But once in a chroot
**LD_LIBRARY_PATH=/lib** does the trick as well.

Another way to make **chroot** to Just Work without **LD_LIBRARY_PATH=**
is to fake relocation with this funny symlink:

```shell
$ ln -s ../../../lib nix/store/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-glibc-2.33-108/lib
$ unshare --user --map-root-user chroot . /bin/bash
$ export PATH=/bin PS1='minimal> '
minimal> ls /
bin  include  include-glibc  lib  libexec  nix
minimal>  gcc --version
gcc (GCC) 10.3.0

minimal> echo 'int main(){}' > a.c
minimal> gcc a.c -o a
minimal> ./a
bash: ./a: No such file or directory
```

Now we are able to run **gcc** (and also **bash**).

The environment is able to compile simple executables. Why do compiled binaries
still fail to run? That is is an exercise for the reader :)
Hint: it can be hacked with yet another symlink. Try to find which one.
[hacking-on-glibc post](/posts/239-hacking-on-glibc.html) also provides another way of solving it.

# stdenv

So how do we bootstrap fresh **nixpkgs** out of outdated **gcc** and
**glibc** from **bootstrap-tools**?

To provide minimal build environment for most packages **nixpkgs** has a
special package: **stdenv**. Let's inspect it's contents:

```
$ nix-shell --pure -p stdenv

[nix-shell]$ echo $PATH | tr ':' $'\n' | ~/bin/unnix
/<<NIX>>/bash-interactive-5.1-p12/bin
/<<NIX>>/patchelf-0.14.3/bin
/<<NIX>>/gcc-wrapper-10.3.0/bin
/<<NIX>>/gcc-10.3.0/bin
/<<NIX>>/glibc-2.33-108-bin/bin
/<<NIX>>/coreutils-9.0/bin
/<<NIX>>/binutils-wrapper-2.35.2/bin
/<<NIX>>/binutils-2.35.2/bin
/<<NIX>>/coreutils-9.0/bin
/<<NIX>>/findutils-4.8.0/bin
/<<NIX>>/diffutils-3.8/bin
/<<NIX>>/gnused-4.8/bin
/<<NIX>>/gnugrep-3.7/bin
/<<NIX>>/gawk-5.1.1/bin
/<<NIX>>/gnutar-1.34/bin
/<<NIX>>/gzip-1.11/bin
/<<NIX>>/bzip2-1.0.6.0.2-bin/bin
/<<NIX>>/gnumake-4.3/bin
/<<NIX>>/bash-5.1-p12/bin
/<<NIX>>/patch-2.7.6/bin
/<<NIX>>/xz-5.2.5-bin/bin
```

The list of binaries is suspiciously close to our **bootstrap-tools**
tarball. That is not a coincidence: if it's good enough for most packages
it should be enough for **gcc**.

In theory we could just use **bootstrap-tools** to define **stdenv**
and use it to build things like **xorg**. However such a scheme would
complicate updating **gcc** (and all other **stdenv** packages):
(once again) bootstrap archive does not get updated frequently.
We would need to refresh bootstrap tarballs routinely just to get
a **gcc** update.

To solve an update problem **nixpkgs** uses a level of indirection:
first **nixpkgs** defines **bootstrap-stdenv** as **bootstrap-tools**
and then builds **stdenv** out of **bootstrap-stdenv**. The rest of
**nixpkgs** uses only **stdenv** and avoids **bootstrap-stdenv**.

Thus our example **xorg** chain of build-time dependencies is:
**bootstrap-tools** -\> **bootstrap-stdenv** -\> **stdenv** -\> **xorg**.

Simple, eh?

Let's look at a **stdenv.mkDerivation** function normally used to define
packages. We define a one-liner **foo** package that prints contents
of it's build environment at build time:

```shell
$ nix build --impure --expr 'with import <nixpkgs> {}; stdenv.mkDerivation { name = "foo"; unpackPhase = "echo $CC; $CC -v; exit 1"; }' -L

foo> unpacking sources
foo> gcc
foo> Using built-in specs.
foo> COLLECT_GCC=/nix/store/2dv93bbc06c7zg866qid73j3r36zz3jx-gcc-10.3.0/bin/gcc
foo> COLLECT_LTO_WRAPPER=/nix/store/2dv93bbc06c7zg866qid73j3r36zz3jx-gcc-10.3.0/libexec/gcc/x86_64-unknown-linux-gnu/10.3.0/lto-wrapper
foo> Target: x86_64-unknown-linux-gnu
foo> Configured with:
foo> Thread model: posix
foo> Supported LTO compression algorithms: zlib
foo> gcc version 10.3.0 (GCC)
...
```

As a small detour there are also other popular **stdenv**s, like an
**LLVM**-based one:

```shell
$ nix build --impure --expr 'with import <nixpkgs> {}; pkgsLLVM.stdenv.mkDerivation { name = "foo"; unpackPhase = "echo $CC; $CC -v; exit 1"; }' -L

foo-x86_64-unknown-linux-gnu> unpacking sources
foo-x86_64-unknown-linux-gnu> x86_64-unknown-linux-gnu-clang
foo-x86_64-unknown-linux-gnu> clang version 11.1.0
foo-x86_64-unknown-linux-gnu> Target: x86_64-unknown-linux-gnu
foo-x86_64-unknown-linux-gnu> Thread model: posix
foo-x86_64-unknown-linux-gnu> InstalledDir: /nix/store/y61l0kbqfchdk39i319ycrfblc6zz3s8-clang-11.1.0/bin
...
```

**nixpkgs** provides many toolchains via various
**stdenv**s. To name a few (assuming **x86_64-linux** system):

- **stdenv**: **gcc** and **glibc**
- **pkgsLLVM.stdenv**: **clang** and **glibc**
- **pkgsMusl.stdenv**: **gcc** and **musl**
- **pkgsi686Linux.stdenv**: **gcc** and **glibc** for 32-bit ABI on x86_64 (**CFLAGS=-m32**)
- **pkgsCross.ppc64.stdenv**: **gcc** and **glibc** cross-compiler to **powerpc64-unknown-linux-gnu** target
- ... and many many more

Finding out how those interact to one another (which **stdenv** is
defined in terms of which) is an exercise for the reader :)

# stdenv tower

So how exactly do we ascend from not-quite-working **bootstrapTools** to
**stdenv**? What is hiding behind the arrow in
"**bootstrap-stdenv** -\> **stdenv**" part above?

The precise answer is hidden in
[stdenv.nix](https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/linux/default.nix).

The main take away from there is that there are multiple stages of
**stdenv**:

**bootstrap-stdenv** -\> **?** -\> **??** -\> **???** -\> **...** -\> **stdenv**.

Instead of trying to figure out what each stage does exactly out of definition
above let's debug it and see what we can find out.

Let's inject a **prev** attribute to each intermediate instance of
**stdenv** and walk along that chain. Here is the full local patch:

```diff
--- a/pkgs/stdenv/generic/default.nix
+++ b/pkgs/stdenv/generic/default.nix
@@ -54,0 +55,3 @@ argsStdenv@{ name ? "stdenv", preHook ? "", initialPath
+
+, # debug reference to previous stdenv
+  prev ? {}
@@ -145,0 +149,2 @@ let
+      inherit prev;
+
--- a/pkgs/stdenv/linux/default.nix
+++ b/pkgs/stdenv/linux/default.nix
@@ -106,0 +107,2 @@ let
+        prev = prevStage.stdenv;
+
@@ -437,0 +440,2 @@ in
+
+      prev = prevStage.stdenv;
```

I left out almost all of patch context on purpose. It's not very
readable as a diff. Now we can access all **stdenv** iterations via
**stdenv.prev**.

First, let's find out how many hops are there from bootstrap to final **stdenv**:

```shell
nixpkgs $ nix repl .
nix-repl> stdenv
«derivation /nix/store/s6l15yfxq567as8wdw7cfvy6c3p9wscw-stdenv-linux.drv»
nix-repl> stdenv.prev
«derivation /nix/store/q3dsgi310i9r5b1pgdym2fnvlxbkxls2-bootstrap-stage4-stdenv-linux.drv»
nix-repl> stdenv.prev.prev
«derivation /nix/store/b5hwd6s4b2xq2l6nidhklypn69vhlfap-bootstrap-stage3-stdenv-linux.drv»
nix-repl> stdenv.prev.prev.prev
«derivation /nix/store/bkbn1y2593jqclaq2kjcx5iambylsafq-bootstrap-stage2-stdenv-linux.drv»
nix-repl> stdenv.prev.prev.prev.prev
«derivation /nix/store/1wklspdwi7c03jrqjbh2f6ymmpqshcif-bootstrap-stage1-stdenv-linux.drv»
nix-repl> stdenv.prev.prev.prev.prev.prev
«derivation /nix/store/bvkp4zp1kidp9wfk2f46yyy73y3n38mx-bootstrap-stage0-stdenv-linux.drv»
nix-repl> stdenv.prev.prev.prev.prev.prev.prev
error: attribute 'stdenv' missing
```

5(!) intermediate steps! Let's check out
compiler version of the intial one and the one right after:

```shell
$ nix build --impure --expr 'with import ./. {}; stdenv.prev.prev.prev.prev.prev.mkDerivation { name = "foo"; unpackPhase = "$CC --version; exit 1"; }' -L
foo> unpacking sources
foo> /nix/store/hbppa2cjx9929jrv796fpni2m06j3fzw-bootstrap-stage0-stdenv-linux/setup: line 1358: --version: command not found
```

The very first (or zeroth) bootstrap stdenv does not even provide basic
"$CC" variable. It's not really a usable stdenv just yet. We can also guess
it from it's empty definition:

```
  ({}: {
    __raw = true;

    gcc-unwrapped = null;
    binutils = null;
    coreutils = null;
    gnugrep = null;
  })
```

It's only a default case that will gradually grow a piece on each step.
Let's look at the next one:

```
$ nix build --impure --expr 'with import ./. {}; stdenv.prev.prev.prev.prev.mkDerivation { name = "foo"; unpackPhase = "$CC --version; exit 1"; }' -L

foo> unpacking sources
foo> gcc (GCC) 8.3.0
...
```

The next **stdenv** provides build environment based on **gcc-8.3.0**
(which is way older than default **nixpkgs** **gcc-10.3.0** version).

Now we have a tool to check what is the actual difference between all
these **stdenv** iterations! I usually use **NIX_DEBUG=1** variable to
look at what **nixpkgs** injects in the search paths and default
options of **gcc**:

```shell
nix build --impure --expr 'with import ./. {}; stdenv.prev.prev.prev.prev.mkDerivation { name = "foo"; unpackPhase = "NIX_DEBUG=1 $CC --version; exit 1"; }' -L

foo> unpacking sources
foo> HARDENING: disabled flags: pie
foo> HARDENING: Is active (not completely disabled with "all" flag)
foo> HARDENING: enabling fortify
foo> HARDENING: enabling stackprotector
foo> HARDENING: enabling strictoverflow
foo> HARDENING: enabling format
foo> HARDENING: enabling pic
foo> extra flags before to /nix/store/i3ibpx67yncp4w4mpkf5pwvjjsd0aqln-bootstrap-tools/bin/gcc:
foo>   -O2
foo>   -D_FORTIFY_SOURCE=2
foo>   -fstack-protector-strong
foo>   --param
foo>   ssp-buffer-size=4
foo>   -fno-strict-overflow
foo>   -Wformat
foo>   -Wformat-security
foo>   -Werror=format-security
foo>   -fPIC
foo> original flags to /nix/store/i3ibpx67yncp4w4mpkf5pwvjjsd0aqln-bootstrap-tools/bin/gcc:
foo>   --version
foo> extra flags after to /nix/store/i3ibpx67yncp4w4mpkf5pwvjjsd0aqln-bootstrap-tools/bin/gcc:
foo>   -B/nix/store/39k40hf9z4wr5wac5xbnznza1ym2f8kz-bootstrap-stage0-glibc-bootstrap/lib/
foo>   -idirafter
foo>   /nix/store/39k40hf9z4wr5wac5xbnznza1ym2f8kz-bootstrap-stage0-glibc-bootstrap/include
foo>   -idirafter
foo>   /nix/store/i3ibpx67yncp4w4mpkf5pwvjjsd0aqln-bootstrap-tools/lib/gcc/x86_64-unknown-linux-gnu/8.3.0/include-fixed
foo>   -B/nix/store/i3ibpx67yncp4w4mpkf5pwvjjsd0aqln-bootstrap-tools/lib
foo>   -B/nix/store/8wmhf2pbmx0vbs60yk6x9w5lm0zrqjlz-bootstrap-stage1-gcc-wrapper-/bin/
foo>   -frandom-seed=dhfkc7mzra
foo> gcc (GCC) 8.3.0
foo> Copyright (C) 2018 Free Software Foundation, Inc.
foo> This is free software; see the source for copying conditions.  There is NO
foo> warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

Here the important parts are the paths specified with **-L** flag (library lookup
paths) and by **-B** flag (objects and tools for compiler itself). Both should be
familiar by now from "hello world" dissection section at [hacking-on-glibc post](/posts/239-hacking-on-glibc.html).

Also note that **nixpkgs** builds files with default **-O2** optimization level
until specified otherwise (also note a few warning options on top enabled by default).

Let's check out our final **stdenv**:

```shell
$ nix build --impure --expr 'with import ./. {}; stdenv.mkDerivation { name = "foo"; unpackPhase = "NIX_DEBUG=1 $CC --version; exit 1"; }' -L

foo> unpacking sources
foo> HARDENING: disabled flags: pie
foo> HARDENING: Is active (not completely disabled with "all" flag)
foo> HARDENING: enabling pic
foo> HARDENING: enabling format
foo> HARDENING: enabling stackprotector
foo> HARDENING: enabling fortify
foo> HARDENING: enabling strictoverflow
foo> extra flags before to /nix/store/j5pl47x8yplyfpbbgvcrzjwbm08n9rvi-gcc-12.0.0/bin/gcc:
foo>   -fPIC
foo>   -Wformat
foo>   -Wformat-security
foo>   -Werror=format-security
foo>   -fstack-protector-strong
foo>   --param
foo>   ssp-buffer-size=4
foo>   -O2
foo>   -D_FORTIFY_SOURCE=2
foo>   -fno-strict-overflow
foo> original flags to /nix/store/j5pl47x8yplyfpbbgvcrzjwbm08n9rvi-gcc-12.0.0/bin/gcc:
foo>   --version
foo> extra flags after to /nix/store/j5pl47x8yplyfpbbgvcrzjwbm08n9rvi-gcc-12.0.0/bin/gcc:
foo>   -B/nix/store/km6a4zxn29liy6l2xq441p2yap1ka1j4-glibc-2.35/lib/
foo>   -idirafter
foo>   /nix/store/6xn0firi6hlz6x161drdj0p0jzcrrfla-glibc-2.35-dev/include
foo>   -idirafter
foo>   /nix/store/j5pl47x8yplyfpbbgvcrzjwbm08n9rvi-gcc-12.0.0/lib/gcc/x86_64-unknown-linux-gnu/12.0.1/include-fixed
foo>   -B/nix/store/fi0acb9a2fscg7afnwjmglj55rqwj8kj-gcc-12.0.0-lib/lib
foo>   -B/nix/store/v9s3sv5c4rr9r067qw66c1iq5i0ffsvc-gcc-wrapper-12.0.0/bin/
foo>   -frandom-seed=rw1fgisg1r
foo> gcc (GCC) 12.0.1 20220213 (experimental)
foo> Copyright (C) 2022 Free Software Foundation, Inc.
foo> This is free software; see the source for copying conditions.  There is NO
foo> warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
```

Note how all the **-B** and **-L** paths flipped from bootstrap variants
of the package (like **-B/nix/store/39k40hf9z4wr5wac5xbnznza1ym2f8kz-bootstrap-stage0-glibc-bootstrap/lib/**)
to nice final package names (like **-B/nix/store/km6a4zxn29liy6l2xq441p2yap1ka1j4-glibc-2.35/lib/**).

Such a long **stdenv** tower reaches it's main goal: **bootstrap-tools**
must not be used in final **stdenv** directly or as runtime depends.
**stdenv** must only be defined by source packages from **nixpkgs**.

Ideally changing **bootstrap-tools** archive should not affect contents of
final **stdenv** at all. In practice it happens only when **content-addressed**
mode is enabled: <https://nixos.wiki/wiki/Ca-derivations>.

To achieve independence from bootstrap binaries **nixpkgs** gradually
substitutes parts of **bootstrap-stdenv** from **bootstrap-tools** to
packages defined by **nixpkgs** expressions:

- **dummy**: provide dummy empty base case. Nothing works here
- **stage0**: provides runnable prebuilt **bootstrap-tools** in **PATH** as **gcc**, **binutils**, **coreutils**, **grep**. From now on we can compile simple tools.
- **stage1**: build **binutils** and **perl** (using **stage0**'s **gcc**, **glibc**, **coreutils**)
- **stage2**: build **glibc** (and it's library depends) (using **stage1**'s **binutils** and **stage0**'s **gcc**, **coreutils**), relink **binutils** against new **glibc**.
- **stage3**: build **gmp**/**mpfr** to prepare **gcc** rebuild (using **stage2**'s **glibc**, **stage1**'s **binutils**, **stage0**'s **gcc**, **coreutils**)
- **stage4**: build **gcc** itself (using **stage3**'s **gmp**/**mpfr**, **stage2**'s **glibc**, **stage1**'s **binutils**, **stage0**'s **gcc**, **coreutils**)
- **final**: build **binutils**, **coreutils** and everything else (using **stage3**'s **gcc**, **stage2**'s glibc, **stage1**'s binutils, **stage0**'s **coreutils**)

Sounds simple? Heh, not really. I probably got a few details above wrong.
I was still not sure what are the exact dependencies that are being rebuilt.

Let's first look at the **final** **stdenv**'s references after it's fully built:

```shell
$ nix-store --query --graph $(nix-build -A stdenv) | dot -Tsvg > stdenv-final-runtime.svg
```

Rendered svg: [stdenv-final graph](/posts.data/240-nixpkgs-bootstrap/stdenv-final-runtime.svg)

Note that there are no (runtime) references to **bootstrap-tools** packages.

Let's look at **stage4** for comparison:

```shell
$ nix-store --query --graph $(nix-build -A stdenv.prev) | dot -Tsvg > stdenv-stage4-runtime.svg
```

Rendered svg: [stdenv-stage4 graph](/posts.data/240-nixpkgs-bootstrap/stdenv-stage4-runtime.svg)

Note how **patchelf** and **binutils** still use **glibc** which was built using **bootstrap-tools**
compiler.

The above graphs don't show build-time dependencies. Ideally **stdenv-final** should not directly
depend on anything related to **boostrap-tools**. We can get the graph by looking at the derivation
instead of final store path:

```shell
$ nix-store --query --graph $(nix-instantiate -A stdenv) | dot -Tsvg > stdenv-drv-final-runtime.svg
```

Rendered svg: [stdenv-drv graph](/posts.data/240-nixpkgs-bootstrap/stdenv-drv-final-runtime.svg)

It's not really readable: there are too many mostly irrelevant minor details like patches and source
tarballs. Can we rearrange nodes explicitly as they are pulled in into each stage?

The simplest (but somewhat incomplete) seems to be the use of **nix-diff** derivation differ:

```
$ nix store diff-closures $(nix-instantiate -A stdenv.prev) $(nix-instantiate -A stdenv)

acl: ∅ → 2.3.1, +109.0 KiB
attr: ∅ → 2.5.1, +78.9 KiB
bash: ∅ → 5.1-p16, +1519.2 KiB
binutils: +6390.2 KiB
binutils-wrapper: +9.9 KiB
bootstrap: ε → ∅, -129542.0 KiB
bootstrap-stage0-glibc: ε → ∅
bootstrap-stage4-gcc-wrapper: 12.0.0 → ∅, -47.5 KiB
bootstrap-stage4-stdenv: ε → ∅, -41.7 KiB
busybox: ε → ∅, -117.2 KiB
bzip2: ∅ → 1.0.6.0.2, +147.6 KiB
coreutils: ∅ → 9.0, +1769.5 KiB
diffutils: ∅ → 3.8, +1450.5 KiB
ed: ∅ → 1.18, +134.7 KiB
expand-response: -17.0 KiB
findutils: ∅ → 4.9.0, +1472.3 KiB
gawk: ∅ → 5.1.1, +2374.2 KiB
gcc-wrapper: ∅ → 12.0.0, +47.5 KiB
gnugrep: ∅ → 3.7, +773.2 KiB
gnumake: ∅ → 4.3, +1391.1 KiB
gnused: ∅ → 4.8, +725.0 KiB
gnutar: ∅ → 1.34, +2834.7 KiB
gzip: ∅ → 1.11, +152.0 KiB
patch: ∅ → 2.7.6, +222.3 KiB
pcre: ∅ → 8.45, +514.5 KiB
stdenv: ∅ → ε, +42.1 KiB
zlib: -121.4 KiB
```

Here we see exact list of packages that differ in the whole tree between **stdenv**
and it's immediate predecessor.

Or we can look at just compiler wrapper difference of **stdenv** (let's try **nix-diff**
for a change):

```
$ nix-diff $(nix-instantiate -A stdenv.prev.cc) $(nix-instantiate -A stdenv.cc) --line-oriented | cat

- /nix/store/d0ivnqxcmjdg9ihdl4ww9a0c79pyl0nd-bootstrap-stage4-gcc-wrapper-10.3.0.drv:{out}
+ /nix/store/45fs8hhm6afg0m0p2d635zhjh608bqsj-gcc-wrapper-10.3.0.drv:{out}
• The set of input derivation names do not match:
    - bootstrap-stage0-stdenv-linux
    + bash-5.1-p12
    + bootstrap-stage4-stdenv-linux
    + coreutils-9.0
    + gnugrep-3.7
• The input derivation named `binutils-wrapper-2.35.2` differs
  - /nix/store/gcyaki78ksxg9s211y4zr4ppnrq1jwlm-binutils-wrapper-2.35.2.drv:{out}
  + /nix/store/9g4r35k465xg4p02c5krbxqnwvihpmbx-binutils-wrapper-2.35.2.drv:{out}
  • The set of outputs do not match:
      + {info}
      + {man}
  • The set of input source names do not match:
      + gnu-binutils-strip-wrapper.sh
  • The set of input derivation names do not match:
      - bootstrap-stage1-stdenv-linux
      + bash-5.1-p12
      + bootstrap-stage4-stdenv-linux
      + coreutils-9.0
      + gnugrep-3.7
  • The set of input derivations named `binutils-2.35.2` do not match
      - /nix/store/l343vxcc5ik63ccrggff3js03y1l9154-binutils-2.35.2.drv:{out}
      + /nix/store/vfzsbipvxkbj9m4yh5is0shfxn3p7b2m-binutils-2.35.2.drv:{info,man,out}
  • The input derivation named `expand-response-params` differs
    - /nix/store/5y71wc7khvy8m2qh6vvrvwx31dhk68r7-expand-response-params.drv:{out}
    + /nix/store/w07x2k4wi8xvmh2nyxvx50nw5pxaga6y-expand-response-params.drv:{out}
    • The set of input derivation names do not match:
        - bootstrap-stage1-stdenv-linux
        + bootstrap-stage3-stdenv-linux
    • The environments do not match:
        stdenv=''
        - /nix/store/7wafj75gbf8kr9i3isdajc5vlm0r8jjp-bootstrap-stage1-stdenv-linux
        + /nix/store/n89rpfsfs317j2qbm57905qzzq0amyhy-bootstrap-stage3-stdenv-linux
    ''
  • Skipping environment comparison
• Skipping environment comparison
```

The above helps getting some intuition on what packages change from one **stdenv**
to another.

This still does not show crucial details of where do those **-B** / **-L** options come
from into the **gcc-wrapper**. And why they matter at all.

# option stacking

In contrast to **FHS** distributions **nix** explicitly allows and encourages previous
versions of software to co-exist with newer ones.

In our case of **stdenv-stage2** just rebuilds **glibc**. On **FHS** system
we would update **glibc** inplace and would rely on it's backwards compatibility to
avoid system breakage right after such an update. It is practical for simle use cases but
sometimes this causes complications. For example it's hard to downgrade **glibc**
once you have rebuilt a few dependencies (say, **gcc**) against a newer version.
And inplace glibc update can cause issues with already running executables that lazily load
**nss** resolver libraries.

Simplistically **nixpkgs** sidesteps the problem by effectively building multiple separate
worlds against different libcs (libc usually come with **stdenv** update or by using
non-default stdenv, like **pkgsMusl.stdenv**).

For our bootstrap case we somehow need to transition:

- from: **gcc** (provided by **bootstrap-tools**) linked against **glibc** (provided by **bootstrap-tools**)
- to: **gcc** (provided by **nixpkgs**) linked against **glibc** (provided by **nixpkgs**)

One of the ways to do it is:

- build **glibc**
- redirect **gcc** (from **bootstrap-tools**) to built **glibc**
- build new **gcc**
- [optional] build **glibc** and **gcc** again (to disentangle from
  **bootstrap-tools**'s **gcc** code generator)

The "redirect **gcc**" part is a tricky but too much: all it needs is the
override of default **-B** / **-L** / **-Wl,-dynamic-linker,** set of flags
mentioned in [hacking-on-glibc post](/posts/239-hacking-on-glibc.html).

We need to watch for option order if we already specify our toolchain
explicitly. Let's look at the following example artificial:

```
# prepare library copies to see the effect:

$ mkdir -p a b
$ cp /<<NIX>>/glibc-2.33-108/lib/crt1.o a/
$ cp /<<NIX>>/glibc-2.33-108/lib/crt1.o b/
$ cp /<<NIX>>/glibc-2.33-108/lib/libc.so a/
$ cp /<<NIX>>/glibc-2.33-108/lib/libc.so.6 a/
$ cp /<<NIX>>/glibc-2.33-108/lib/libc.so b/
$ cp /<<NIX>>/glibc-2.33-108/lib/libc.so.6 b/
$ cp /glibc-2.33-108/lib/ld-linux-x86-64.so.2 a/
$ cp /glibc-2.33-108/lib/ld-linux-x86-64.so.2 b/

$ tree
.
├── a
│   ├── crt1.o
│   ├── ld-linux-x86-64.so.2
│   ├── libc.so
│   └── libc.so.6
├── a.c
├── b
│   ├── crt1.o
│   ├── ld-linux-x86-64.so.2
│   ├── libc.so
│   └── libc.so.6
```

Above I placed idential copies of **glibc** into a new directory (suppose
we built slightly newer version of **glibc**) and then pointed **gcc** there.

Quiz time: try to take some time and guess what the following command would print:

```
$ LANG=C gcc hello.c -o c -Wl,--verbose \
    -La -Lb \
    -Bb -Ba \
    -Wl,--dynamic-linker=$PWD/a/ld-linux-x86-64.so.2 -Wl,--dynamic-linker=$PWD/b/ld-linux-x86-64.so.2 \
    |& fgrep succeeded | unnix | uniq
```


Note that **-L** options go in a-then-b order, **-B** options go in b-then-a
order and dynamic-linker again goes in a-then-b.

Guess which files get picked from which directory.

Here is the result:

```
$ gcc a.c -o c -Wl,--verbose -La -Lb -Bb -Ba -Wl,--dynamic-linker=$PWD/a/ld-linux-x86-64.so.2 -Wl,--dynamic-linker=$PWD/b/ld-linux-x86-64.so.2 |& fgrep succeeded | unnix | uniq
attempt to open b/crt1.o succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/crti.o succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/crtbegin.o succeeded
attempt to open /tmp/cctmOQBK.o succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open a/libc.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libc.so.6 succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libc_nonshared.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/ld-linux-x86-64.so.2 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/crtend.o succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/crtn.o succeeded
```

Surprisingly (or not so surprisingly) **crt1.o** came from **-Bb**
(first option in the list), **libc.so** came from **-La** (also first
option in the list). But dynamic linker was ... ignored?

Yeah. Note that **gcc** is already a thick wrapper in **nixpkgs**.
Let's expand it with **NIX_DEBUG=1**:

```
$ NIX_DEBUG=1 gcc a.c -o c -La -Lb -Bb -Ba -Wl,--dynamic-linker=$PWD/a/ld-linux-x86-64.so.2 -Wl,--dynamic-linker=$PWD/b/ld-linux-x86-64.so.2 |& unnix

HARDENING: disabled flags: pie pic format stackprotector fortify strictoverflow
extra flags before to /<<NIX>>/gcc-11.2.0/bin/gcc:
  -Wl\,-dynamic-linker=/<<NIX>>/glibc-2.33-108/lib/ld-linux-x86-64.so.2
original flags to /<<NIX>>/gcc-11.2.0/bin/gcc:
  a.c
  -o
  c
  -La
  -Lb
  -Bb
  -Ba
  -Wl\,--dynamic-linker=/tmp/zz/a/ld-linux-x86-64.so.2
  -Wl\,--dynamic-linker=/tmp/zz/b/ld-linux-x86-64.so.2
extra flags after to /<<NIX>>/gcc-11.2.0/bin/gcc:
  -B/<<NIX>>/gcc-11.2.0-lib/lib
  -B/<<NIX>>/glibc-2.33-108/lib/
  -idirafter
  /<<NIX>>/glibc-2.33-108-dev/include
  -idirafter
  /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/include-fixed
  -B/<<NIX>>/gcc-wrapper-11.2.0/bin/
  -L/<<NIX>>/glibc-2.33-108/lib
  -L/<<NIX>>/gcc-11.2.0-lib/lib
HARDENING: disabled flags: relro pie bindnow
extra flags before to /<<NIX>>/binutils-2.35.2/bin/ld:
  ''
...
```

Note that **gcc** already injects **-Wl,-dynamic-linker=/<<NIX>>/glibc-2.33-108/lib/ld-linux-x86-64.so.2**
as the very first parameter (even before our options). As a result it gets
picked first. Inability to override the dynamic linker looks like minor a bug
of **nixpkgs** wrapper. I think wrapper should consistently treat all
libc overrides. It's unsafe to mix different parts of **glibc**
(we already saw **SIGSEGV** on **hello.c** above).

If you guessed this output right you already know a lot more than me
on this topic :)

Given that option order matters a lot **nixpkgs** needs to make sure
that overrides work as expected at least most of the time:
if we override **-L** option for **glibc**, then **-B** option override
should be present and should follow order specified by **-L** (and not
the other way around).

I found out about these details only because **nixpkgs** was actually
getting the option order wrong until
<https://github.com/NixOS/nixpkgs/pull/158047/commits/649ebfbed65189d7d62e4f2fe0e491552308a6f1>
was applied.

For quite a while **nixpkgs** was using **crt1.o** from wrong **glibc**
which made **stdenv** slightly contaminated by **bootstrap-tools**.
It used to work because **crt1.o** contents did not change for many
**glibc** releases. Until **glibc-2.34**. Then we started getting all
sorts of linkage failures at bootstrap:

```
    expand-response-params> ld: /nix/store/p4s4jf7aq6v6z9iazll1aiqwb34aqxq9-bootstrap-tools/lib/crt1.o: in function `_start':
    expand-response-params> /build/glibc-2.27/csu/../sysdeps/x86_64/start.S:101: undefined reference to `__libc_csu_fini'
    expand-response-params> ld: /build/glibc-2.27/csu/../sysdeps/x86_64/start.S:102: undefined reference to `__libc_csu_init'
    expand-response-params> collect2: error: ld returned 1 exit status
```

Once understood the fix (or workaround) was trivial.

There are many ways to avoid the mix-up in future. The simplest would be
to never pass more than one **glibc** via **-B** / **-L** and always
disable defaul search paths. But that's for another time :)

Have fun!
