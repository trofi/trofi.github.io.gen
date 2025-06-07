---
title: "Fixing HPPA boot"
date: July 06, 2020
---

It feels like I did not have much chance to fix anything complicated on
`hppa` machines (aka `PA-RISC`). But recently ...

On `#gentoo-hppa` IRC channel Jeroen Roovers reported a problem: after a
switch to `gcc-10` and rebuilding/reinstalling `sys-boot/palo`
package (`PA-RISC` boot loader) the machine was not able to boot
anymore.
Reverting to `palo` built with `gcc-9` fixed the boot. Is it a
`gcc` bug?

## Clues

New toolchain versions tend to break boot loaders all the time. There are
so many ways to do it! Where do we start? My standard guesses are:

- code size increase and overflow (boot loaders are often constrained in
  size).
- new unhandled relocation types from fresh `binutils`
- new forms of instructions not supported by early environment (for
  example SIMD CPU extension might need reconfiguration)
- `-fPIE` `gentoo` default
- `-fstack-protector` `gentoo` default

`gcc-10` is
[known](/posts/213-gcc-10-in-gentoo.html) to have
quite a few non-trivial code generation changes like enabling
`-fno-common` defaults and maybe something else.

## First try

Jeroen tested `palo` with `gcc-10 -fcommon`, but it did not fix
machine boot. Not so simple.
I had no idea on how `hppa` boots. `palo` itself has a great doc
explaining how it should work:
<https://git.kernel.org/pub/scm/linux/kernel/git/deller/palo.git/tree/README.html>

`Tl;DR` of the boot process is:

1.  machine starts it's own firmware
2.  firmware enumerates disks and reads first 512 bytes block of the
    boot disk
3.  firmware expects first block in the following format (`IPL` stands
    for Initial Program Loader):

    ``` 
    0x80 0x00: some signature
    ... 
    IPL_ADDR:  4 bytes, offset of IPL on disk, must be 2K multiple
    IPL_SIZE:  4 bytes, size of IPL program, must be 2K multiple
    IPL_ENTRY: 4 bytes, entry point offset within IPL, 4-byte aligned
    ...
    ```
4.  firmware reads `IPL` into RAM and transfers control. This is where
    our code starts running.

Simple! Almost like `x86` `BIOS` `MBR` style boot (with one extra
indirection).
Simplicity also means that recovering system with broken `IPL` written
on disk is tedious: you need to boot from good media (other disk, `CD`,
netboot) and rewrite it on disk.
Firmware also logs the boot process and complains when something went
wrong ([boot log example](https://bugs.gentoo.org/724264)).
Our `IPL` program implementation is `iplboot` file from `palo`
package. It's full size is about 50K, which is a manageable size to get
through disassembly manually if nothing else works.

## `gcc` hint

Jeroen also noticed suspicious `palo` build warning:

``` 
bootloader.h:71:6: warning: conflicting types for built-in function ‘bzero’;
    expected ‘void(void *, unsigned int)’ [-Wbuiltin-declaration-mismatch]
```

`gcc` says that `bzero()` prototype in `palo` code is different
from the standard prototype. Who knows what that means for `gcc`?
Could it stop applying `bzero()`-related optimizations like inlining
on small fixed-sized buffers?
I attempted to check the difference in the generated code.
To speed things up a bit I moved to `x86_64` machine and continued
debugging there. Cross-compiling `palo` on `gentoo` is simple:

``` 
### build cross-compilers:
# crossdev hppa2.0-unknown-linux-gnu

### build palo
$ LANG=C PORTAGE_CONFIGROOT=/usr/hppa2.0-unknown-linux-gnu ebuild palo-2.12-r1.ebuild clean compile
...
    In file included from ../lib/gzip.c:25:
./bootloader.h:71:6: warning: conflicting types for built-in function 'bzero';
    expected 'void(void *, unsigned int)' [-Wbuiltin-declaration-mismatch]
   71 | void bzero(char *p, size_t len);
      |      ^~~~~
```

Let's look at how exactly `iplboot` is built:

``` 
make -j8 AR=hppa2.0-unknown-linux-gnu-ar CC=hppa2.0-unknown-linux-gnu-gcc \
    LD=hppa2.0-unknown-linux-gnu-ld -C ipl

make: Entering directory '/tmp/portage/sys-boot/palo-2.12-r1/work/palo-2.12/ipl'

hppa2.0-unknown-linux-gnu-gcc -D__ASSEMBLY__ -I../lib -traditional -c -o crt0.o crt0.S

hppa2.0-unknown-linux-gnu-gcc -DIPL_LOADER -I. -I../lib -I../include \
    -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks   -c -o byteio.o byteio.c
...
hppa2.0-unknown-linux-gnu-ar rv ipl.a byteio.o elf64.o ipl.o offset.o \
    gzip.o diskpart.o ext2.o lib.o pdc_bootio.o vsprintf.o elf32.o \
    fileio.o load.o pdc_cons.o pdc_misc.o
...
echo "const char bld_info[] = \"http://www.parisc-linux.org - Sat Jul 4 11:44:42 BST 2020\";" > build.c

hppa2.0-unknown-linux-gnu-gcc -DIPL_LOADER -I. -I../lib -I../include \
    -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks -c -o build.o build.c

hppa2.0-unknown-linux-gnu-ld -N --section-start .init=0x60000 -e '$START$' \
    -o iplelf crt0.o ipl.a build.o `hppa2.0-unknown-linux-gnu-gcc -print-libgcc-file-name`
...
./palo/mkbootable ipl/iplelf iplboot
```

Here we see a few frequently used tricks for boot programs:

- the boot loader is first linked into a static `ELF` file with `ld`
  command
- `ld` arguments carefully avoid linkage against `-lc` (`libc`)
- `ld` arguments pull in `libgcc.a` (via
  `-print-libgcc-file-name`)
- entry point is set to `$START$` label
- `.init` section is pinned to static `0x60000` address

To explore the difference I patched `palo` `bzero()` prototype to
match the standard `libc` definition:

``` diff
--- a/palo-2.12/ipl/bootloader.h2019-09-05 22:25:39.000000000 +0100
+++ b/palo-2.12/ipl/bootloader.h2020-07-04 12:53:53.706511217 +0100
@@ -68,7 +68,7 @@

 int streq(const char *a, const char *b);
 char *strcpy(char *dest, const char *src);
 char *strcat(char *dest, const char *src);
-void bzero(char *p, size_t len);
+void bzero(void *p, size_t len);
 void *memcpy(void *d, const void *s, size_t len);
 size_t strlen(const char *s);
 size_t strnlen(const char *s, size_t count);
--- palo-2.12-r1_orig/work/palo-2.12/ipl/lib.c2019-09-05 22:25:39.000000000 +0100
+++ palo-2.12-r1/work/palo-2.12/ipl/lib.c2020-07-04 12:55:18.861477880 +0100
@@ -202,8 +202,9 @@
 return NULL;
 }
-void bzero(char *p, size_t len)
+void bzero(void *_p, size_t len)
 {
+char *p = _p;
 /* slow but safe */
 while (len--)
     *p++ = 0;
```

And diffed disassembly produced by `hppa2.0-unknown-linux-gnu-objdump
-d $file.o`:

``` 
$ diff -u \
    <(hppa2.0-unknown-linux-gnu-objdump -r -d ./palo-2.12-r1_orig/work/palo-2.12/ipl/ipl.o) \
    <(hppa2.0-unknown-linux-gnu-objdump -r -d ./palo-2.12-r1/work/palo-2.12/ipl/ipl.o)
       ...
       b,l 71c <iplmain+0x80>,rp
-          R_PARISC_PCREL17F bzero
+          R_PARISC_PCREL17F memset
       ...
```

Here we see that `bzero()` call was changed to `memset()` call.
Normally this transformation is not harmful. But `memset()` call was
generated out of nowhere and `gcc` just assumes the function
definition exists somewhere. Luckily `palo` defines `memset()` as
well and the transformation is not problematic.

## Trying the hypothesis

Jeroen tried a similar `bzero()` patch on `hppa` machine and
confirmed this change does not fix the boot problem.
Looking at the diff suggested that `gcc` does high-level
transformations related to `builtin` functions. As boot loaders usually
don't require high performance we can disable all `builtins` with
`-fno-builtin` and get more predictable code generation.

Jeroen added `-fno-builtin` compiler option to `palo` and the system
booted again! [Upstream
patch](https://git.kernel.org/pub/scm/linux/kernel/git/deller/palo.git/commit/?id=a9d9c86da54064023633f4b71d57e68ec426d11d):

``` diff
--- a/ipl/Makefile
+++ b/ipl/Makefile
@@ -39,7 +39,7 @@ endif
 VPATH=../lib:.

 AFLAGS= -I../lib
-CFLAGS= -DIPL_LOADER -I. -I../lib -I../include -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks
+CFLAGS= -DIPL_LOADER -I. -I../lib -I../include -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks -fno-builtin
 LDFLAGS  = -N --section-start .init=0x60000 -e '$$START$$'

 all:iplelf
```

## Breakage mechanics

I wanted to understand the exact failure mode a bit better. Is it a
proper fix or a workaround for some underlying problem? To get some
details I compared disassembly files of default vs `-fno-builtin`
`palo` builds.
Most interesting code generation change happened in
[`ipl/lib.c`](https://git.kernel.org/pub/scm/linux/kernel/git/deller/palo.git/tree/ipl/lib.c?id=a9d9c86da54064023633f4b71d57e68ec426d11d).

I'll spare you from the assembly listings and show intermediate
representation instead. Let's check what optimizer does with
`ipl/lib.c` using `-fopt-info` option.

Default build:

``` 
$ hppa2.0-unknown-linux-gnu-gcc -DIPL_LOADER -I. -I../lib -I../include -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks  -c -o lib.o lib.c -fopt-info
In file included from lib.c:8:
bootloader.h:71:6: warning: conflicting types for built-in function «bzero»; expected «void(void *, unsigned int)» [-Wbuiltin-declaration-mismatch]
   71 | void bzero(char *p, size_t len);
      |      ^~~~~
lib.c:42:12: optimized:  Inlining malloc_aligned/36 into malloc/37.
lib.c:113:9: optimized:  Inlining strpbrk/42 into strtok/44.
...
lib.c:58:6: optimized: Semantic equality hit:release/40->malloc_init/41
lib.c:58:6: optimized: Assembler symbol names:release/40->malloc_init/41
optimized:  Inlined release/74 into malloc_init/41 which now has time 3.000000 and size 4, net change of -1.
lib.c:108:12: optimized:  Inlined strspn/75 into strtok/44 which now has time 590.273748 and size 44, net change of +9.
lib.c:137:8: optimized: Loop 1 distributed: split to 0 loops and 1 library calls.
lib.c:137:8: optimized: Loop 1 distributed: split to 0 loops and 1 library calls.
lib.c:208:8: optimized: Loop 1 distributed: split to 0 loops and 1 library calls.
```

`-fno-builtin` build:

``` 
$ hppa2.0-unknown-linux-gnu-gcc -DIPL_LOADER -I. -I../lib -I../include -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks -fno-builtin   -c -o lib.o lib.c -fopt-info
lib.c:42:12: optimized:  Inlining malloc_aligned/36 into malloc/37.
lib.c:113:9: optimized:  Inlining strpbrk/42 into strtok/44.
...
lib.c:58:6: optimized: Semantic equality hit:release/40->malloc_init/41
lib.c:58:6: optimized: Assembler symbol names:release/40->malloc_init/41
optimized:  Inlined release/73 into malloc_init/41 which now has time 3.000000 and size 4, net change of -1.
lib.c:108:12: optimized:  Inlined strspn/74 into strtok/44 which now has time 544.436932 and size 44, net change of +9.
```

Function inlining is expected and probably not very interesting. Let's
check out other two optimizations:

``` c
// Semantic equality hit:release/40->malloc_init/41

void release (void *ptr)
{
    __free = (char *) ptr;
}
void malloc_init(char *free)
{
    __free = free;
}
```

Here `gcc` caught identical implementation of two functions.
Now on to the loop distribution optimization:

``` c
// lib.c:137:8: optimized: Loop 1 distributed: split to 0 loops and 1 library calls.

void * memset(void * s,int c,size_t count)
{
    char *xs = (char *) s;

    while (count--) /* line 137 */
        *xs++ = c;

    return s;
}
```

The exact message `"split to 0 loops and 1 library calls"` tells us
that the resulting code has no loops and one library call.

**Quiz time**: guess what library call was this loop replaced with? :)

Let's look at the intermediate compiler state to find out. `gcc` has
a set of `-fdump-*` options to peek at various phases. I never know
what I'm looking for and just dump all of them:

``` 
$ hppa2.0-unknown-linux-gnu-gcc -DIPL_LOADER -I. -I../lib -I../include \
    -O2 -mdisable-fpregs -Wall -fno-delete-null-pointer-checks -c -o lib.o lib.c \
    -fdump-tree-all-slim -fdump-rtl-all-slim
```

The command generates about 350 files with names of the form
`lib.c.nnn{t,r}.${pass_name}` for all passes. I'll show two most
relevant passes for our case. Just before loop distribution pass:

``` 
;; $ cat lib.c.148t.ivcanon
;; Function memset (memset, funcdef_no=45, decl_uid=796, cgraph_uid=46, symbol_order=46)

__attribute__((nothrow, leaf))
memset (void * s, int c, size_t count)
{
  char * xs;
  char _4;

  <bb 2> [local count: 118111600]:
  count_15 = count_6(D) + 4294967295;
  if (count_6(D) != 0)
    goto <bb 4>; [89.00%]
  else
    goto <bb 7>; [11.00%]

  <bb 7> [local count: 12992276]:

  <bb 3> [local count: 118111600]:
  return s_5(D);

  <bb 4> [local count: 105119324]:
  _4 = (char) c_10(D);

  <bb 5> [local count: 955630225]:
  # xs_16 = PHI <s_5(D)(4), xs_9(6)>
  # count_19 = PHI <count_15(4), count_8(6)>
  xs_9 = xs_16 + 1;
  *xs_16 = _4;
  count_8 = count_19 + 4294967295;
  if (count_19 != 0)
    goto <bb 6>; [89.00%]
  else
    goto <bb 8>; [11.00%]

  <bb 8> [local count: 105119324]:
  goto <bb 3>; [100.00%]

  <bb 6> [local count: 850510901]:
  goto <bb 5>; [100.00%]

}
```

It's a bit hard to read, but it's still the same loop in disguise:

- function prologue to check for exit condition early
- increment counter: `xs_9 = xs_16 + 1;`
- single memory store iteration: `*xs_16 = _4;`
- loop exit condition check: `if (count_19 != 0)`
- next iteration: `goto <bb 5>;`

Let's check what loop distribution pass did to it. After:

``` 
;; $ cat lib.c.149t.ldist
;; Function memset (memset, funcdef_no=45, decl_uid=796, cgraph_uid=46, symbol_order=46)

__attribute__((nothrow, leaf))
memset (void * s, int c, size_t count)
{
  char * xs;
  char _4;
  int _18;

  <bb 2> [local count: 118111600]:
  count_15 = count_6(D) + 4294967295;
  if (count_6(D) != 0)
    goto <bb 4>; [89.00%]
  else
    goto <bb 3>; [11.00%]

  <bb 3> [local count: 118111600]:
  return s_5(D);

  <bb 4> [local count: 105119324]:
  _4 = (char) c_10(D);
  _18 = (int) _4;
  __builtin_memset (s_5(D), _18, count_6(D));
  goto <bb 3>; [100.00%]

}
```

Now it's just two steps:

- `[unchanged]` a bit of function prologue to check exit condition early
- `[new]` all of loop code is transformed to ... a `memset()` call!

In C it would look similar to the following:

``` c
void * memset(void * s, int c, size_t count)
{
    char *xs = (char *) s;
    if (! count)
        return xs;

    return memset(s, c, count);
}
```

It's an infinite recursion without a chance to succeed.
Second loop distribution transformation is also enlightening:

``` c
// lib.c:208:8: optimized: Loop 1 distributed: split to 0 loops and 1 library calls.

void bzero(char *p, size_t len)
{
    /* slow but safe */
    while (len--) /* line 208 */
        *p++ = 0;
}
```

was transformed into:

``` c
void bzero(char *p, size_t len)
{
    if (! len)
        return;

    memset(p, 0, len);
}
```

The result is similar: a call to `memset()` and infinite recursion
there. Arfrever pointed out that `gcc-10` enabled
`-ftree-loop-distribute-patterns` option on `-O2` and above. While
previous version of `gcc` did it on `-O3` and above. This change
probably exposed this failure mode.

## Parting words

- `c` compiler does many unusual things behind your back and relies on
  runtime libraries to provide basic primitives like `memset()`.

  This means `memset()` and friends can't be naively implemented in
  standard C without tricks on compiler and build system side even if we
  ignore things like aliasing rule changes due to pointer casts.
- `hppa` booting process is very straightforward. This makes it
  somewhat debuggable without access to `hppa` machine :)

- boot loaders use advanced tricks to reuse standard compiler and linker
  to be able to write most code in `c`.

Have fun!
