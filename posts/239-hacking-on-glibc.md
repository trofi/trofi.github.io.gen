---
title: "Hacking on glibc"
date: March 13, 2022
---

# Intro

Sometimes it's useful to check fresh `glibc` out and test a fix
against it. Or add a few `printf()` statements into relevant modules.
Fun past cases of the above are
[here](/posts/189-glibc-on-ia64-or-how-relocations-bootstrap.html),
[here](/posts/205-stack-protection-on-mips64.html) and
[here](/posts/209-tracking-down-mysterious-memory-corruption.html).
We'll look in more detail how to build and use local `glibc` for debugging
purposes. I'll focus on redirecting the toolchain (and not on the ways to
break `glibc` with rare `CFLAGS` or configure `--options`). That would
be a separate big and entertaining topic :)
It also might be useful for folks who deal with systems that involve
multiple `glibc` versions present in the system simultaneously
(for bootstrap, embedded development or testing purposes).

## Hello World anatomy

Suppose you have just built a fresh experimental `glibc` version.
How do you build and run a simple "hello world" program against it
without modifying already installed system `glibc` and `gcc`?
Let's dissect a simple "hello world" in detail and try to extract
a few assumptions that `gcc` and `binutils` already embed.
Here is our specimen:

```c
// hello.c:
#include <stdio.h>
int main(void) { puts ("hello!"); }
```

The example compiles and runs just fine:

```
$ gcc -c hello.c -o hello.o
$ gcc hello.o -o hello
$ ./hello
hello!
```

If you are somewhat familiar with the `c` toolchain you might already know
that there are a few stages involved:

- `compilation`: `c`-source pre-processing (include substitution) and
  translation of pre-processed C-source file into an `.o` file
- `linkage`: `.o` files and their dependencies are joined together into
  final executable file

Let's look at the pre-processing stage. Where do pre-processed headers
come from? We can have a peek at it by looking at pre-processed output
using `-E` flag:

```
$ gcc -E hello.c -o - | head -n 10 | unnix

# 0 "hello.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/<<NIX>>/glibc-2.33-108-dev/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "hello.c"

# 1 "/<<NIX>>/glibc-2.33-108-dev/include/stdio.h" 1 3 4
# 27 "/<<NIX>>/glibc-2.33-108-dev/include/stdio.h" 3 4
# 1 "/<<NIX>>/glibc-2.33-108-dev/include/bits/libc-header-start.h" 1 3 4
...
```

As expected `<stdio.h>` comes from `glibc`. On `FHS` systems
that is usually `/usr/include`.
There also is another mysterious header: `stdc-predef.h` (also a `glibc`
one). It gets added even into empty files:

```
$ echo | gcc -E - -o - | head -n 10 | unnix

# 0 "<stdin>"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/<<NIX>>/glibc-2.33-108-dev/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "<stdin>"
```

`stdc-predef.h` is a header `gcc` knows to import if it targets `glibc`
(but not other `libcs`): <https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/config/glibc-c.cc>.

Pre-processing was straightforward. Ideally translation should not
depend on `glibc` specifics. In practice it might but we'll ignore it here.
Let's now look at linking phase.
We'll use `-Wl,-t` flag (thanks MaskRay!) to get details of what linker actually
pulls in:

```
$ LANG=C gcc hello.o -o hello -Wl,-t |& unnix

/<<NIX>>/glibc-2.33-108/lib/crt1.o
/<<NIX>>/glibc-2.33-108/lib/crti.o
/<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.1.0/crtbegin.o
hello.o
/<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.1.0/libgcc.a
/<<NIX>>/glibc-2.33-108/lib/libgcc_s.so
/<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1
/<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.1.0/libgcc.a
/<<NIX>>/glibc-2.33-108/lib/libc.so
/<<NIX>>/glibc-2.33-108/lib/libc.so.6
/<<NIX>>/glibc-2.33-108/lib/libc_nonshared.a
/<<NIX>>/glibc-2.33-108/lib/ld-linux-x86-64.so.2
/<<NIX>>/glibc-2.33-108/lib/libc_nonshared.a
/<<NIX>>/glibc-2.33-108/lib/ld-linux-x86-64.so.2
/<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.1.0/libgcc.a
/<<NIX>>/glibc-2.33-108/lib/libgcc_s.so
/<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1
/<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.1.0/libgcc.a
/<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/12.1.0/crtend.o
/<<NIX>>/glibc-2.33-108/lib/crtn.o
```

That is a lot of output! But be not afraid. Some things repeat here 4
times and inflate the output a bit.
All these files above are used in linking process:

- `crt{1,i,n}.o`: `glibc` runtime startup and shutdown support files (`__attribute__((contructor))` support)
- `crt{begin,end}`: `gcc` runtime startup and shutdown support files (`c++` global constructor and destructor support)
- `hello.o`: our own file!
- `libgcc.a`, `libgcc_s.so`: `gcc` runtime support for primitives that compiler
  needs when CPU does not always provide as instructions (128-bit integer multiplication, stack unwinders,
  atomic primitives on unusual type size and similar).
- `libc.so`, `libc_nonshared.a`: actual `c` library that implements `puts()` (`printf()`) and friends.
- `ld-linux-x86-64.so.2`: `glibc` dynamic loader.

## Custom `glibc`

We found out that `glibc` provides us at least:

- include headers
- `crt{1,i,n}.o` object files
- `libc.so` shared object files
- `libc_nonshared.a` static library files
- `ld-linux-x86-64.so.2`: dynamic loader

Now we should be able to redirect all these pieces. Let' build our custom
`glibc` first:

```
$ git clone https://sourceware.org/git/glibc.git /tmp/custom-glibc-src
$ mkdir -p /tmp/custom-glibc-build /tmp/custom-glibc-install

$ cd /tmp/custom-glibc-build
$ /tmp/custom-glibc-src/configure --prefix=/tmp/custom-glibc-install
$ make && make install
```

We can already use resulting `glibc` to run other programs:

```
$ /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2 --library-path /tmp/custom-glibc-install/lib $(which id)

uid=1000(slyfox) gid=100(users) groups=100(users),1(wheel),26(video)

$ LD_DEBUG=all /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2 --library-path /tmp/custom-glibc-install/lib $(which id) |& fgrep relocation | unnix
   2844932:     relocation processing: /tmp/custom-glibc-install/lib/libc.so.6 (lazy)
   2844932:     relocation processing: /tmp/custom-glibc-install/lib/libdl.so.2 (lazy)
   2844932:     relocation processing: /tmp/custom-glibc-install/lib/libpthread.so.0 (lazy)
   2844932:     relocation processing: /<<NIX>>/attr-2.5.1/lib/libattr.so.1
   2844932:     relocation processing: /<<NIX>>/acl-2.3.1/lib/libacl.so.1
   2844932:     relocation processing: /<<NIX>>/openssl-1.1.1m/lib/libcrypto.so.1.1
   2844932:     relocation processing: /tmp/custom-glibc-install/lib/librt.so.1 (lazy)
   2844932:     relocation processing: /run/current-system/sw/bin/id
   2844932:     relocation processing: /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2
```

I'm searching for "relocation" here as a hack to see all loaded files in memory.
From the above we see that `libc.so`, `libdl.so` and
`libpthread.so` are used from our custom `glibc`. But `libattr.so`
(`attr` package), `libacl.so` (`acl` package), `libcrypto.so`
(`openssl` package) are used from their current locations. It is safe
to load such libraries only if they were built against same or older
`glibc` versions. All thanks to `glibc` being backwards compatible.
If we would, say, use use `glibc-2.8` as a custom version things would
probably fail to load as those libraries depend on fresh symbols:

```
$ LD_DEBUG=all /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2 --library-path /tmp/custom-glibc-install/lib $(which id) |& fgrep libcrypto | fgrep GLIBC_2.17 | unnix
    532869:     checking for version `GLIBC_2.17' in file /tmp/custom-glibc-install/lib/libc.so.6 [0] required by file /<<NIX>>/openssl-1.1.1m/lib/libcrypto.so.1.1 [0]
```

In this case `id` binary requires at least `glibc-2.17` (via `libcrypto.so` dependency).
Ok, so running against modified `glibc` is straightforward. It would be
useful to rebuild `id` (and `openssl`) against older `glibc`.
Let's now try building our toy example against modified `glibc`.
To deal with pre-processor we can use `-I` option:

```
$ gcc -E hello.c -o - -I/tmp/custom-glibc-install/include | head -n 10 | unnix
# 0 "hello.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/tmp/custom-glibc-install/include/stdc-predef.h" 1
# 0 "<command-line>" 2
# 1 "hello.c"

# 1 "/tmp/custom-glibc-install/include/stdio.h" 1
# 27 "/tmp/custom-glibc-install/include/stdio.h"
# 1 "/tmp/custom-glibc-install/include/bits/libc-header-start.h" 1
...
```

For complex cases `gcc` supports many other flavours of handling include
paths: `-isystem`, `-idirafter`, `-isysroot`, `-iquote` and
many more. We will use simplest `-I`.
Now let's deal with the `libc.so` location. Normally `-L` option would
be enough to specify library lookup path:

```
{ LANG=C gcc hello.o -o hello -Wl,--verbose -L/tmp/custom-glibc-install/lib | fgrep succeeded; } |& unnix
/<<NIX>>/binutils-2.35.2/bin/ld: /<<NIX>>/glibc-2.33-108/lib/crt1.o: in function `_start':
/build/glibc-2.33/csu/../sysdeps/x86_64/start.S:101: undefined reference to `__libc_csu_fini'
/<<NIX>>/binutils-2.35.2/bin/ld: /build/glibc-2.33/csu/../sysdeps/x86_64/start.S:102: undefined reference to `__libc_csu_init'
/<<NIX>>/binutils-2.35.2/bin/ld: link errors found, deleting executable `hello'
collect2: error: ld returned 1 exit status
attempt to open /<<NIX>>/glibc-2.33-108/lib/crt1.o succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/crti.o succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/crtbegin.o succeeded
attempt to open hello.o succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /tmp/custom-glibc-install/lib/libc.so succeeded
attempt to open /tmp/custom-glibc-install/lib/libc.so.6 succeeded
attempt to open /tmp/custom-glibc-install/lib/libc_nonshared.a succeeded
attempt to open /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/crtend.o succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/crtn.o succeeded
```

Almost worked!

- good: we successfully redirected `libc.so.6` and `libc_nonshared.a`
  (as expected); and even `ld-linux-x86-64.so.2` was redirected!
- bad: `crt{1,i,n}.o` set of files were not redirected; they are not
  exactly libraries, thus it's expected.

`crt{1,i,n}.o` are still pulled in from system `glibc`. Such a mix of
parts from different `glibc` versions causes linkage failure:
`undefined reference to '__libc_csu_fini'`.
Object files can be redirected with `-B` (or can be redirected with
`-nostartfiles /path/to/crt{1,i,n}.o`):

```
$ LANG=C gcc hello.o -o hello -Wl,--verbose -L/tmp/custom-glibc-install/lib -B/tmp/custom-glibc-install/lib | fgrep succeeded |& unnix
attempt to open /tmp/custom-glibc-install/lib/crt1.o succeeded
attempt to open /tmp/custom-glibc-install/lib/crti.o succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/crtbegin.o succeeded
attempt to open hello.o succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /tmp/custom-glibc-install/lib/libc.so succeeded
attempt to open /tmp/custom-glibc-install/lib/libc.so.6 succeeded
attempt to open /tmp/custom-glibc-install/lib/libc_nonshared.a succeeded
attempt to open /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so succeeded
attempt to open /<<NIX>>/glibc-2.33-108/lib/libgcc_s.so.1 succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/libgcc.a succeeded
attempt to open /<<NIX>>/gcc-11.2.0/lib/gcc/x86_64-unknown-linux-gnu/11.2.0/crtend.o succeeded
attempt to open /tmp/custom-glibc-install/lib/crtn.o succeeded
```

Let's ignore `libgcc_s.so` for now. It's not really a part of `glibc` but
a `nixpkgs` idiosyncrasy. The rest of `glibc` files is successfully redirected!
Does final result look good now? Let's try!

```
$ LANG=C ./hello
Segmentation fault (core dumped)
```

Our program can't even run. Why is that?

```
$ LD_DEBUG=all ./hello |& fgrep reloc | unnix
   1359934:     relocation processing: /tmp/custom-glibc-install/lib/libc.so.6 (lazy)
   1359934:     relocation processing: ./hello (lazy)
   1359934:     relocation processing: /<<NIX>>/glibc-2.33-108/lib/ld-linux-x86-64.so.2
```

`LD_DEBUG=all` hints at dynamic loader from our system `glibc`
and not from custom `glibc`. That path is embedded into `gcc` itself:

```
gcc -dumpspecs |& fgrep ld-linux | unnix
...
    -dynamic-linker %{muclibc:/lib/ld64-uClibc.so.0;:
                    %{mbionic:/system/bin/linker64;:
                    %{mmusl:/<<NIX>>/glibc-2.33-108/lib/ld-musl-x86_64.so.1;:
                    /<<NIX>>/glibc-2.33-108/lib64/ld-linux-x86-64.so.2}}}
...
```

Here linker spec always passes `-dynamic-linker /<<NIX>>/glibc-2.33-108/lib64/ld-linux-x86-64.so.2`
path to the linker until it's explicitly overridden. Let's override it
explicitly.
Here is our final combined result of overriding headers, libraries,
object files and dynamic linker:


```
$ gcc hello.c -o hello \
    -I/tmp/custom-glibc-install/include \
    \
    -L/tmp/custom-glibc-install/lib \
    -B/tmp/custom-glibc-install/lib \
    -Wl,-dynamic-linker,/tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2

$ ./hello
hello!

$ LD_DEBUG=all ./hello |& fgrep reloc | unnix
   1398510:     relocation processing: /tmp/custom-glibc-install/lib/libc.so.6 (lazy)
   1398510:     relocation processing: ./hello (lazy)
   1398510:     relocation processing: /tmp/custom-glibc-install/lib/ld-linux-x86-64.so.2
```

Success! We completely untangled from host's `glibc`.
Sometimes (usually in bare-metal space) it's easier to drop defaults
entirely and specify all the dependencies as explicit arguments.
A few related options you might want to explore in `gcc` are:

- `-nostdinc`
- `-nostdlib`
- `-nolibc`
- `-nodefaultlibs`
- `-nostartfiles`
- `-ffreestanding`

They explicitly disable search paths or object files inclusion for:
include paths, start up files or standard and runtime support libraries.
Their interaction is subtle. I won't get into detail here either.

## Parting words

It is straightforward to experiment with new `glibc` without damaging
your main install on most Linux distributions. Just make sure you set the
environment up correctly and override all the bits.

`glibc` has many moving parts to watch for when you replace parts of
it: headers (`-I`), object files (`-B`), shared libraries (`-L`),
static libraries (also `-L`) and dynamic linker (`ld-linux-x86-64.so.2`).

`LD_DEBUG=` is extremely useful for dynamic loader debugging.

Mixing parts from different `glibc` versions in a single binary is a
sure way to get build failure and even runtime crashes.

Have fun!
