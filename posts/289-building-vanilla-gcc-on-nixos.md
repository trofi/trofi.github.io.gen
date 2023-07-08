---
title: "Building vanilla gcc on NixOS"
date: July 8, 2023
---

This post's main goal is to build unpatched vanilla `gcc` itself (and to
run it's test suite). The focus is `gcc` development and not `gcc` usage
in `NixOS`.

## Why vanilla?

You might have already noticed that `nixpkgs` patches `gcc` build quite
heavily:
<https://github.com/NixOS/nixpkgs/tree/master/pkgs/development/compilers/gcc>

For example as of today [builder.sh](https://github.com/NixOS/nixpkgs/blob/84822c43fcf6787f3680868d6f63e80b69244fbe/pkgs/development/compilers/gcc/builder.sh)
takes 11KB on disk. It might not be a lot, but some changes there are
very invasive. Like interpreter injection:

```bash
    # ...
        declare -a extraLDFlags=()
        if [[ -e "${!curBintools}/nix-support/orig-libc" ]]; then
            # Figure out what extra flags when linking to pass to the gcc
            # compilers being generated to make sure that they use our libc.
            extraLDFlags=($(< "${!curBintools}/nix-support/libc-ldflags") $(< "${!curBintools}/nix-support/libc-ldflags-before" || true))
            if [ -e ${!curBintools}/nix-support/ld-set-dynamic-linker ]; then
                extraLDFlags=-dynamic-linker=$(< ${!curBintools}/nix-support/dynamic-linker)
            fi
    # ...
```

On top of that `nixpkgs` packages rely on [cc-wrapper](https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/cc-wrapper/default.nix)
features added on top. `cc-wrapper` implements features like option
passing via `NIX_CFLAGS_COMPILE`, automatic injection of non-default
paths to `glibc` headers and libraries, hardening features defaults,
slight binary renames, options mangling and many other small things.

Fun fact: `cc-wrapper` is 25K of extra shell code.

All the above makes `gcc` test suite incompatible with `nixpkgs`
patches.

It's not always easy to tell if the bug is introduced by an upstream
logic or by `nixpkgs` changes.

It's hard to upstream found `gcc` bugs or to work on `gcc` fixes
without the fear of being affected by downstream changes.

And once you have prepared an upstreamable `gcc` patch it would be nice
to run `gcc` test suite to see if the local change introduced any
regressions.

Wouldn't it be nice to just build vanilla `gcc` every now and then to
see how unmodified `gcc` behaves?

## Th perfect build

In an ideal world a program should be compilable by running
`./configure && make && make install` (or an equivalent for other build
systems).

In practice software frequently makes assumptions about default paths
that don't match file system layout imposed by `nix`. For example `gcc`
assumes that system headers should be present in `/usr/include` and
`ELF` interpreter on `x86_64` should be at `/lib64/ld-linux-x86-64.so.2`.

"Naturally" `NixOS` provides none of these paths and stores everything
under `/nix/store/<hash>-<package>-<version>` to allow multiple versions
of any package to co-exist without conflicts (be it a compiler, kernel,
libc, `bash` or `firefox`).

Specifically there is no default `/usr/include` on `NixOS`:

```
$ ls /usr/include
ls: cannot access '/usr/include': No such file or directory
```

## FHS wrapper

Normally the `nixpkgs` packaging solution is to explicitly pass
non-default include (and library, `PATH` paths) to the `./configure`.
Or implicitly via `NIX_CFLAGS_COMPILE`.

But sometimes it does not work as smoothly. `gcc` is a perfect example
of a special package here. Another example is a binary-only package not
distributed in a source form (say, a game).

To reconcile that mismatch `nixpkgs` provides a few helpers to build a
[FHS](https://en.wikipedia.org/wiki/Filesystem_Hierarchy_Standard)
"chroot" out of `/nix/store/...` paths. Helper mounts `/nix/store` paths
into locations expected by `FHS`.

One of such helpers is [buildFHSEnv](https://nixos.org/manual/nixpkgs/stable/#sec-fhs-environments).
I'll use it as an example.

To get started let's create `shell.nix` file in the current directory
and run `nix-shell` there:

```nix
{ pkgs ? import <nixpkgs> {} }:

let e =
  pkgs.buildFHSEnv {
    name = "gcc-git-build-env";
    targetPkgs = ps: with ps; [
      # library depends
      gmp gmp.dev
      isl
      libffi libffi.dev
      libmpc
      libxcrypt
      mpfr mpfr.dev
      xz xz.dev
      zlib zlib.dev

      # git checkout need flex as they are not complete release tarballs
      m4
      bison
      flex
      texinfo

      # test harness
      dejagnu
      autogen

      # valgrind annotations
      valgrind valgrind.dev

      # toolchain itself
      gcc
      stdenv.cc
      stdenv.cc.libc stdenv.cc.libc_dev
    ];
  };
in e.env
```

Running:

```
$ nix-shell
$$ ls /usr/include/
aio.h        endian.h           glob.h          memory.h    nss.h           signal.h       uchar.h
aliases.h    envz.h             gmp.h           misc        obstack.h       sound          ucontext.h
...
```

Ready! Now in that shell (and only that shell) we have `/usr/include`
(and other standard paths) populated. The implementation uses unshared
linux mount user namespaces with a few bind mounts.

Let's do a full `gcc` build in that environment using naive
`./configure` flags:

```
$$ git clone --depth 1 https://gcc.gnu.org/git/gcc.git
$$ mkdir gcc-build
$$ cd gcc-build
$$ ../gcc/configure --disable-multilib --prefix=$PWD/../gcc-installed
$$ make -j $(nproc)
$$ make install
```

Build and install are done!

I had to use `--disable-multilib` as not all 32-bit libraries are
present by default. Getting multilib to work is an exercise for the
reader :)

Now let's use our installed compiler as is:

```
$$ printf '#include <iostream>\nint main() { std::cout << "Hello!" << std::endl ; }' > a.cc
$$ ../gcc-installed/bin/g++ a.cc -o a
$$ ./a
./a: /usr/lib/libstdc++.so.6: version `GLIBCXX_3.4.32' not found (required by ./a)
```

Almost worked. `gcc` itself did start, but produced binaries did not
embed default `RPATH` to `gcc`'s own `libstdc++` and use the (outdated)
system `libstdc++`. There are a few ways to work it around. The simplest
one is to use `static-libstdc++`:

```
$$ ../gcc-installed/bin/g++ a.cc -o a -static-libstdc++
$$ ./a
Hello!
```

As a bonus we can also run unmodified `gcc` test suite works as well:

```
$$ make check
make[1]: Entering directory '/tmp/gcc/gcc-build'
make[2]: Entering directory '/tmp/gcc/gcc-build/fixincludes'
autogen -T ../../gcc/fixincludes/check.tpl ../../gcc/fixincludes/inclhack.def
...
Using /tmp/gcc/gcc/gcc/testsuite/lib/gcc.exp as tool init file.
Test run by slyfox on Fri Jul  7 08:34:18 2023
Native configuration is x86_64-pc-linux-gnu

                === gcc tests ===

Schedule of variations:
    unix

Running target unix
Using /usr/share/dejagnu/baseboards/unix.exp as board description file for target.
Using /usr/share/dejagnu/config/unix.exp as generic interface file for target.
Using /tmp/gcc/gcc/gcc/testsuite/config/default.exp as tool-and-target-specific interface file.
Running /tmp/gcc/gcc/gcc/testsuite/gcc.c-torture/compile/compile.exp ...
...
                === gcc Summary ===

# of expected passes            191743
# of unexpected failures        107
# of unexpected successes       19
# of expected failures          1502
# of unsupported tests          2593
...
```

`107` unexpected failures of `191743` performed. Not too bad!

## Parting words

`buildFHSEnv` is a great workaround when one is in need of an `FHS`
layout. It helps picky packages including `gcc` itself.

While `NixOS` has an unusual directory structure it is flexible enough to
be able to simulate traditional layouts like FHS with a small
`buildFHSEnv` "chroot" builder. It's useful for both development
environments and for running external binaries built against FHS linux
systems.

Happy hacking and have fun!
