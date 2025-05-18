---
title: ghc and static linkage
date: July 16, 2011
---

`TL;DR` variant:

``` bash
# force ld to inject binary-local search path
$ cabal configure --ghc-option=-optl-Wl,-rpath=\'$ORIGIN\'
$ cabal build
$ cp /usr/lib/libgmp.so.$YOURONE dist/build/.../
```

Some thoughts:

If you ever tried to distribute binaries built with `ghc`, you should
know what I'm talking about. `ghc` is a huge compiler with huge
runtime, so there is some things to note.

Let's explore minimal binary a bit before linking more advanced
project.

``` haskell
-- minimal.hs
main = print 1
```

```
$ ghc --make minimal.hs
    [1 of 1] Compiling Main             ( minimal.hs, minimal.o )
    Linking minimal ...
$ ./minimal
    1
$ ldd minimal
    linux-vdso.so.1 =>  (0x00007fffc0dff000)
    libgmp.so.10 => /usr/lib64/libgmp.so.10 (0x00007f1dd6233000)
    libm.so.6 => /lib64/libm.so.6 (0x00007f1dd5fb1000)
    librt.so.1 => /lib64/librt.so.1 (0x00007f1dd5da8000)
    libdl.so.2 => /lib64/libdl.so.2 (0x00007f1dd5ba4000)
    libc.so.6 => /lib64/libc.so.6 (0x00007f1dd581d000)
    libpthread.so.0 => /lib64/libpthread.so.0 (0x00007f1dd5600000)
    /lib64/ld-linux-x86-64.so.2 (0x00007f1dd64a2000)
```

Looks good. All the libraries are `GNU libc`'s libraries except for
`libgmp.so.10` one. `GMP` changed it's `ABI` recently and it's
reflected it in its `SONAME`:

- `gmp-4` and less provides `libgmp.so.3`
- `gmp-5` provides `libgmp.so.10`

As `gmp` is an `LGPL` library, so we would like to ship the lib
separately. Sometimes user would like to use host's lib instead.

```
$ ldd /usr/lib/libgmp.so.10
    linux-vdso.so.1 =>  (0x00007fff2dbff000)
    libc.so.6 => /lib64/libc.so.6 (0x00007f1383d44000)
    /lib64/ld-linux-x86-64.so.2 (0x00007f1384367000)
```

As we see it is feasible as `GMP` does not use weird things. We see only
`libc` in dependencies as well.

When some binary is loaded by the kernel, kernel either maps the binary
and handovers control to its entry point (simple `a.out` case), or
passes control to the interpreter. For most of dynamically linked
programs the interpreter is stored in `.interp` section (`INTERP`
program header):

```
$ readelf -l /bin/ls
  INTERP         0x0000000000000270 0x0000000000400270 0x0000000000400270
  [Requesting program interpreter: /lib64/ld-linux-x86-64.so.2]
  ....
```

So, my interpreter is `libc`'s `/lib64/ld-linux-x86-64.so.2` loader.
It's called indirectly when binary id ran as

    $ /bin/ls

and directly when I run it as:

    $ /lib64/ld-linux-x86-64.so.2 /bin/ls

Interpreter can take different command line arguments (like
`--library-path` / `--library-rpath`), It can also
understand environment variables to adjust its behavior, like
(`LD_LIBRARY_PATH`, `LD_DEBUG`, `LD_PRELOAD`).

Those allow us override library search path defined in
`/etc/ld.so.conf`, force-inject other libraries to hook some `libc`
function (see tiny `tsocks` project as an example).

But we can also inject search paths to an `ELF` file. It's an:

- `-rpath=<path>` `ld` option
- or `-Wl,-rpath=<path>` `gcc` option
- or `-optl-Wl,-rpath=<path>` `ghc`'s option
- or `--ghc-option=-optl-Wl,-rpath=<path>` `cabal configure`
  option

You can either pass absolute path (or relative to current working
directory) or a special magic value describing the directory where our
ran binary lies: the `$ORIGIN` value.

This technique is used commonly by relocatable software with shared
libraries, like `wine`:

``` bash
$ readelf -a `which wine` | grep PATH
    0x0000000f (RPATH)      Library rpath: [$ORIGIN/../lib32]
    0x0000001d (RUNPATH)    Library runpath: [$ORIGIN/../lib32]
```

It allows `bin/wine` to load its libraries from sibling directory:
`bin/../lib`.

So, the `cabal configure --ghc-option=-optl-Wl,-rpath=\'$ORIGIN\'`
trick allows us distribute all needed shared libraries with built
binary.

Another approach would be to attempt to link everything statically, but
`glibc` uses `dlopen()` for encoding conversion (the `iconv()`
call). And `ghc` uses `iconv` heavily when performs I/O on `String` types.

```
$ ghc --make -fforce-recomp -optl-static -optl-pthread  minimal.hs
    [1 of 1] Compiling Main             ( minimal.hs, minimal.o )
    Linking minimal ...
    /usr/lib64/ghc-7.0.4/libHSrts.a(Linker.o): In function `internal_dlopen':
    Linker.c:(.text+0x11f4): warning: Using 'dlopen' in statically linked applications requires at runtime the shared libraries from the glibc version used for linking
```

```
$ ./minimal
    minimal: mkTextEncoding: invalid argument (Invalid argument)
```

The error comes from
`ghc-7.0.4/libraries/base/GHC/IO/Encoding/Iconv.hs: newIConv`:
it calls `iconv_open()`.

As we see static linking does not even work. It seems to be a long
hanging bug in `glibc` though, as its manual explicitly allows
static linking, but requires target system to have charset conversion
shared libraries installed.

`Tl;DR`: just use `-rpath` thing and/or distribute your stuff in source
form via `hackage` if possible.
