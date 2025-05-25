---
title: GHC on m68k
date: March 12, 2016
---

This all started from this [`ghc` bug
report](https://ghc.haskell.org/trac/ghc/ticket/11395).

The bug stated that on `m68k` ABI return values of `int`-type are
passed in register `%d0` while `void *`-type are passed in register
`%a0`. `GHC` `c` code generation was not using return types
consistently.

I had zero knowledge of `m68k` at that time. I only had a vague
impression that CPU of this type is typically attached to `~1MB` RAM or
something. I expected the CPU to have 32 bit registers or less :)
Portability bugs are my favorite and I've started The Quest by
building a `c` cross-compiler. Gentoo allows you to do it with
`crossdev` script:

```
$ crossdev -t m68k-unknown-linux-gnu
# Done!

$ m68k-unknown-linux-gnu-gcc --version
m68k-unknown-linux-gnu-gcc (Gentoo 5.3.0 p1.0, pie-0.6.5) 5.3.0
```

## the assembly

`m68k` ISA is very simple. Smaller example from bug report:

``` c
long   f(long   a) { return a; }
void * g(void * p) { return p; }
```

``` asm
# m68k-unknown-linux-gnu-gcc -S -O2 -fomit-frame-pointer a.c
f:
    move.l 4(%sp),%d0
    rts
g:
    move.l 4(%sp),%a0
    move.l %a0,%d0
    rts
```

Real-world code example (just to get a feeling about patterns used by
`gcc`):

```
# m68k-unknown-linux-gnu-objdump -d /usr/m68k-unknown-linux-gnu/usr/bin/locale
80001754 <.text>:
80001754:       4e56 fff8       linkw %fp,#-8
80001758:       48e7 3f3c       moveml %d2-%d7/%a2-%a5,%sp@-
8000175c:       42b9 8000 96b0  clrl 800096b0 <stdout@@GLIBC_2.0+0x1c>
80001762:       42b9 8000 96ac  clrl 800096ac <stdout@@GLIBC_2.0+0x18>
80001768:       4879 8000 4258  pea 80004258 <_IO_stdin_used@@Base+0x178>
8000176e:       42a7            clrl %sp@-
80001770:       45f9 8000 163c  lea 8000163c <setlocale@plt>,%a2
80001776:       4e92            jsr %a2@
80001778:       508f            addql #8,%sp
8000177a:       4a88            tstl %a0
8000177c:       6700 02a2       beqw 80001a20 <calloc@plt+0x2e0>
80001780:       4879 8000 4258  pea 80004258 <_IO_stdin_used@@Base+0x178>
80001786:       4878 0005       pea 5 <strstr@plt-0x800011eb>
8000178a:       4e92            jsr %a2@
8000178c:       508f            addql #8,%sp
8000178e:       4a88            tstl %a0
80001790:       6700 02cc       beqw 80001a5e <calloc@plt+0x31e>
80001794:       4879 8000 968c  pea 8000968c <_libc_intl_domainname@@GLIBC_2.0>
8000179a:       4eb9 8000 131c  jsr 8000131c <textdomain@plt>
800017a0:       42a7            clrl %sp@-
800017a2:       486e fff8       pea %fp@(-8)
800017a6:       42a7            clrl %sp@-
800017a8:       2f2e 000c       movel %fp@(12),%sp@-
800017ac:       2f2e 0008       movel %fp@(8),%sp@-
```

## first attempt

"Should be easy to weed out all the bugs" was my thought :)
Having spent some time arguing with `gcc` devs about handling of
incomplete prototypes in [`gcc`
bugzilla](https://gcc.gnu.org/bugzilla/show_bug.cgi?id=69221) and on
[`gcc` mailing
list](http://comments.gmane.org/gmane.comp.gcc.devel/142294) it was time
to start fixing `GHC`.
I was familiar with the `GHC` code responsible for `c` code
generation and gave first shot without actually building `GHC`
compiler for `m68k`:
[patch](https://ghc.haskell.org/trac/ghc/attachment/ticket/11395/0001-c-codegen-split-external-symbol-prototypes-EF_.patch).

Adrian tried to test the patch and reported `GHC` still `SIGSEGV`ed
as before. It was not very clear if patch changed anything. Did it crash
later than used to or did I break something subtle?

## cross-`ghc` nano-howto

Next step was to try to actually build `GHC` as a cross-compiler.
It appears to be as simple as building cross-`gcc`:

```
$ ./configure --target=m68k-unknown-linux-gnu --enable-unregisterised \
              --enable-bfd-debug
```

That's it! The first line is enough but it's faster for subsequent
rebuilds to use system dependencies and not bundled libraries. Thus
I've installed dependencies to the `SYSROOT`:

```
$ ARCH=m68k emerge-m68k-unknown-linux-gnu -1 \
    sys-libs/ncurses \
    dev-libs/gmp \
    virtual/libffi \
    sys-libs/binutils-libs
```

And I got cross-`ghc`! Testing:

```
$ echo 'main = print 42' > /tmp/hi.hs

$ inplace/bin/ghc-stage1 --make /tmp/hi.hs

$ file /tmp/hi
/tmp/hi: ELF 32-bit MSB executable, Motorola m68k, 68020, version 1 (SYSV),
         dynamically linked, interpreter /lib/ld.so.1, for GNU/Linux 2.6.32, not stripped
```

Simple!

## `qemu-user` nano-howto

`qemu` allows you to run not only system images (`qemu-system`) but also
standalone executables (`qemu-user`) for foreign architectures and even
other kernels!
The simple usage example for `powerpc64` (it's slightly better
supported by `qemu`) would be:

``` c
#include <stdio.h>
int main() {
    printf ("Hello world!\n");
}
```

```
$ powerpc64-unknown-linux-gnu-gcc a.c -o a

$ /usr/bin/qemu-ppc64 -L /usr/powerpc64-unknown-linux-gnu ./a
Hello world!
```

With help of [`binfmt_misc`](https://en.wikipedia.org/wiki/Binfmt_misc)
kernel module you can run foreign binaries seamlessly as native
binaries. This allows running things like test suites for cross-compilers
as they would have produced binaries you can run on your machine.
Setup for `powerpc64`:

```
$ echo  ':ppc64:M::\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x15:\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff:/qemu-ppc64:'  > /proc/sys/fs/binfmt_misc/register

$ cat /qemu-ppc64 
#!/bin/bash

exec /usr/bin/qemu-ppc64 -L /usr/powerpc64-unknown-linux-gnu/ "$@"
```

Running a foreign binary:

```
$ file a
a: ELF 64-bit MSB executable, 64-bit PowerPC or cisco 7500, version 1 (SYSV),
   dynamically linked, interpreter /lib64/ld64.so.1, for GNU/Linux 2.6.32, not stripped
$ ./a
Hello world!
```

Gentoo has a [detailed wiki
page](https://wiki.gentoo.org/wiki/Crossdev_qemu-static-user-chroot) on
how to get `qemu-user` on your system.

## `m68k` hoops

I did the same for `m68k` but found out `qemu-m68k` can't even run
simplest programs:

```
$ /usr/bin/qemu-m68k -L /usr/m68k-unknown-linux-gnu ./a
qemu: fatal: Illegal instruction: ebc0 @ f67e36a8
D0 = 6ffffef5   A0 = f67fb6bc   F0 = 0000000000000000 (           0)
D1 = 0000010a   A1 = f67de000   F1 = 0000000000000000 (           0)
D2 = 0000000f   A2 = f6ffec04   F2 = 0000000000000000 (           0)
D3 = 00000000   A3 = 00000000   F3 = 0000000000000000 (           0)
D4 = 00000000   A4 = 00000000   F4 = 0000000000000000 (           0)
D5 = 00000000   A5 = f67fb774   F5 = 0000000000000000 (           0)
D6 = 00000000   A6 = f6ffee54   F6 = 0000000000000000 (           0)
D7 = 00000000   A7 = f6ffec04   F7 = 0000000000000000 (           0)
PC = f67e36a8   SR = 0000 ----- FPRESULT =            0
```

That means `qemu` does not support some instructions generated by `gcc`.
`objdump -d` can help us find out if it's a real opcode or `gcc`/`ld`
generated garbage:

```
$ m68k-unknown-linux-gnu-gcc -static a.c -o a

$ m68k-unknown-linux-gnu-objdump -d ./a  | grep ebc0

800180ec:       ebc0 105f       bfexts %d0,1,31,%d1
8001ebc0:       226e 0014       moveal %fp@(20),%a1
80037ab8:       ebc0 105f       bfexts %d0,1,31,%d1
8003ebc0:       6612            bnes 8003ebd4 <_dl_close_worker+0x96a>
80058168:       61ff ffff ebc0  bsrl 80056d2a <get_cie_encoding>
```

`ebc0` looks like a real `bfexts` instruction (Bit Field Extract
Signed). But Debian guys who reported a bug somehow can run `m68k`
`chroot`!

## hoop #1: `qemu` fork

Debian has a nice [wiki page about
it](https://wiki.debian.org/M68k/sbuildQEMU) Basically the trick to
emulate `m68k` binaries is to use Laurent's `qemu` `git` tree. It allowed
me to run `c` hello-world.

## hoop #2: minimum alignment

Time to try `haskell` hello-world:

```
$ echo 'main = print 42' > /tmp/hi.hs

$ inplace/bin/ghc-stage1 --make -debug /tmp/hi.hs

$ /tmp/hi
SIGSEGV
```

We are on the right track. `qemu` understands all the instructions it
sees. `qemu-user` is capable of generating nice `gdb`-readable core files:

```
$ gdb /tmp/hi qemu_hi*.core
Core was generated by `/tmp/hi +RTS -Ds -Di -Dw -DG -Dg -Db -DS -Dt -Dp -Da -Dl -Dm -Dz -Dc -Dr'.
Program terminated with signal SIGSEGV, Segmentation fault.
#0  0x80463b0a in LOOKS_LIKE_INFO_PTR_NOT_NULL (p=32858) at includes/rts/storage/ClosureMacros.h:248
248         return (info->type != INVALID_OBJECT && info->type < N_CLOSURE_TYPES) ? rtsTrue : rtsFalse;

(gdb) bt
#0  0x80463b0a in LOOKS_LIKE_INFO_PTR_NOT_NULL (p=32858) at includes/rts/storage/ClosureMacros.h:248
#1  0x80463b46 in LOOKS_LIKE_INFO_PTR (p=32858) at includes/rts/storage/ClosureMacros.h:253
#2  0x80463b6c in LOOKS_LIKE_CLOSURE_PTR (p=0x805aac6e <stg_dummy_ret_closure>) at includes/rts/storage/ClosureMacros.h:258
#3  0x80463e4c in initStorage () at rts/sm/Storage.c:121
#4  0x8043ffb4 in hs_init_ghc (argc=0xf6ffebf0, argv=0xf6ffebf4, rts_config=...) at rts/RtsStartup.c:181
#5  0x80455982 in hs_main (argc=1, argv=0xf6ffed54, main_closure=0x804b8f70 <ZCMain_main_closure>, rts_config=...)
    at rts/RtsMain.c:51
#6  0x80003c1c in main ()
```

These `+RTS -Ds -Di -Dw -DG -Dg -Db -DS -Dt -Dp -Da -Dl -Dm -Dz -Dc
-Dr` are all flags for debug runtime. Sanity checks of sorts.
This sanity failure basically says that it got tagged pointer where if
should not be tagged. `GHC` uses least significant bits for closure
pointers to distinct between evaluated and unevaluated closures. On
32-bit systems last 2 bits are used for tags assuming those will always
be zero.

```c
// includes/Cmm.h
#if SIZEOF_VOID_P == 4
#define W_ bits32
/* Maybe it's better to include MachDeps.h */
#define TAG_BITS                2
#elif SIZEOF_VOID_P == 8
#define W_ bits64
/* Maybe it's better to include MachDeps.h */
#define TAG_BITS                3
#else
#error Unknown word size
#endif

/*
 * The RTS must sometimes UNTAG a pointer before dereferencing it.
 * See the wiki page Commentary/Rts/HaskellExecution/PointerTagging
 */
#define TAG_MASK ((1 << TAG_BITS) - 1)
#define UNTAG(p) (p & ~TAG_MASK)
#define GETTAG(p) (p & TAG_MASK)
```

``` c
// includes/rts/storage/ClosureMacros.h
...
static inline StgClosure *
UNTAG_CLOSURE(StgClosure * p)
{
    return (StgClosure*)((StgWord)p & ~TAG_MASK);
}
```

Back to our backtrace. `stg_dummy_ret_closure` is an `int` array
(untagged) with currently evaluated closure. But its address is
clearly not aligned by 4-byte boundary:

``` haskell
Prelude> 0x805aac6e `mod` 4
2
```

Most architectures have `sizeof(int) == __alignof__(int)`
alignment for various reasons (performance, lack of support for
unaligned access in memory/cache unit).

```c
#include <stdio.h>
int main() {
    printf ( "sizeof (int) = %u, __alignof__ (int) = %u\n"
       , (unsigned) sizeof (int), (unsigned) __alignof__ (int));
    return 0;
}
```

Here is the result for a few targets:

  target                        `sizeof (int)` `__alignof__ (int)`
  ----------------------------- -------------- -----------------------
  x86_64-pc-linux-gnu           4              4
  i686-pc-linux-gnu             4              4
  powerpc-unknown-linux-gnu     4              4
  powerpc64-unknown-linux-gnu   4              4
  sparc-unknown-linux-gnu       4              4
  sparc64-unknown-linux-gnu     4              4
  ia64-unknown-linux-gnu        4              4
  `<you get the pattern>`       `...`          `...`
  m68k-unknown-linux-gnu        4              **2**(!)

The fix for this is simple: we need to add
`__attribute__((aligned(sizeof(int))))` to all closure
declarations in `.data` section. Thus [the
fix](https://git.haskell.org/ghc.git/commitdiff/ade1a461ab4ba3e6de3c4afe9fe9766b7b4e51b3)
does roughly that.
Python also had suspiciously similar problem [reported
here](http://bugs.python.org/issue17237).

## hoop #3: `qemu` bug

I did not expect `GHC` to work at this time as I've not fixed `int`
/ `void*` prototypes mismatch yet. But hello-world `haskell` program
was already working!
The next step is to try to run `GHC` test suite and see what will
break:

```
$ make fasttest TEST_HC=$(pwd)/inplace/bin/ghc-stage1 THREADS=12
```

`testsuite/mk/inplace_bin_ghc-stage1.mk` requires some manual tweaks
in auto-detected variables for our cross-compiler. Updated variables:

``` make
WORDSIZE=32 # was 64
TARGETPLATFORM=m68k-unknown-linux # was x86_64-unknown-linux
TargetARCH_CPP=m68k # was x86_64
GhcWithInterpreter=NO # was YES
```

What was unclear is why `qemu` itself did throw assertion errors on many
samples:

``` 
qemu-m68k/tcg/tcg.c:1774: tcg fatal error
```

The samples seemingly didn't do anything fancy. It was time to dive
into `qemu`!
I've picked one of small test cases that failed: `mul2`

``` haskell
-- testsuite/tests/numeric/should_run/mul2.hs
{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Prim
import GHC.Word
import Data.Bits

main :: IO ()
main = do f 5 6
          f 0xFD94E3B7FE36FB18 49
          f 0xFD94E3B7FE36FB18 0xFC1D8A3BFB29FC6A

f :: Word -> Word -> IO ()
f wx@(W# x) wy@(W# y)
    = do putStrLn "-----"
         putStrLn ("Doing " ++ show wx ++ " * " ++ show wy)
         case x `timesWord2#` y of
             (# h, l #) ->
                 do let wh = W# h
                        wl = W# l
                        r = shiftL (fromIntegral wh) (bitSize wh)
                          + fromIntegral wl
                    putStrLn ("High: " ++ show wh)
                    putStrLn ("Low: " ++ show wl)
                    putStrLn ("Result: " ++ show (r :: Integer))
```

Stepping aside a few words on how `qemu` works:
`qemu` is a tiny jit compiler which has the following passes:

1.  `target native instructions` -> `TCG IR`: `JIT` decodes
    target's instructions and translates them into intermediate
    representation (`TCG`).
2.  `TCG IR` -> `TCG IR`: a few basic optimisations like dead
    store elimination
3.  `TCG IR` -> `host-native instructions`: intermediate
    representation is then compiled to host's instruction set
4.  `execute host-native instructions`: executed host code; return
    status explains why generated code finished

`qemu` does these steps not on instruction-by-instruction basis but on
sequence-by-sequence basis. `qemu` (simplified) disassembles up to the
first branching instruction.
Our `qemu` assertion happens in pass `3.` [somewhere
here](https://github.com/qemu/qemu/blob/master/tcg/tcg.c#L1774)
To find out bad instruction sequence it's enough to run `qemu` with
`-d in_asm` flag. It dumps whatever is read by pass `1.`

``` 
$ qemu-m68k-git -d in_asm -L /usr/m68k-unknown-linux-gnu/ /tmp/mul2
----------------
IN:
0xf550c812:  moveq #32,%d5
0xf550c814:  subl %d4,%d5
0xf550c816:  movel %d6,%d0
0xf550c818:  asll #2,%d0
0xf550c81a:  addal %d0,%a0
0xf550c81c:  addal %d0,%a1
0xf550c81e:  movel %a0@-,%d2
0xf550c820:  movel %d2,%d0
0xf550c822:  lsrl %d5,%d0
0xf550c824:  lsll %d4,%d2
0xf550c826:  movel %d2,%d1
0xf550c828:  subql #1,%d6
0xf550c82a:  beqs 0xf550c856
#
qemu-m68k/tcg/tcg.c:1774: tcg fatal error
```

The offending instruction sequence was quite large but it was easy to
trim it down to 2 instructions:

``` asm
_start:
    asll #2,%d0
    movel %a0@,%d2
    rts
```

I copied original disassembly dump, pasted it in a text file
and built a tiny program from it (removing instructions one by one):

```
$ m68k-unknown-linux-gnu-gcc -nostdlib -nostartfiles m68k.S -o foo

$ qemu-m68k-git -d in_asm -L /usr/m68k-unknown-linux-gnu/ ./foo
#  fails as:
#    IN:
#    0x80000054:  asll #2,%d0
#    0x80000056:  movel %a0@,%d2
#    0x80000058:  rts
#    qemu-m68k/tcg/tcg.c:1774: tcg fatal error
```

We don't need to get a working program. It's enough to keep `qemu`
crashing in `TCG` pass.
Having explored failure a bit more I found out it's enough to have
single bad instruction to appear and crash `qemu`:

``` asm
# m68k.S
_start:
    asll #1,%d0
    br _start
```

Using `qemu`s `-d op` flag allows us to look at intermediate
representation of `TCG`:

```
$ m68k-unknown-linux-gnu-gcc -nostdlib -nostartfiles m68k.S -o foo

$ qemu-m68k-git -d in_asm,op -L ./foo

 # asll #1,%d0 muops:
 ---- 80000054 ffffffff
 movi_i32 tmp0,$0x0   # tmp0 = 0
 movi_i32 tmp1,$0x1f  # tmp1 = 31
 shr_i32 CC_C,D0,tmp1 # CC_C = D0 >> tmp1
 movi_i32 tmp1,$0x1   # tmp1 = 1
 shl_i32 CC_N,D0,tmp1 # CC_N = D0 << tmp1
 mov_i32 CC_V,tmp0    # CC_V = tmp0
 movi_i32 tmp1,$0x1f  # tmp1 = 31
 movi_i32 tmp3,$0x1e  # tmp3 = 30
 shr_i32 CC_V,D0,tmp3 # CC_V = D0 << tmp3
 sar_i32 tmp2,D0,tmp2 # tmp2 = D0 >> tmp2, but tmp2 is used uninitilalized!
 ...
```

Our tiny instruction got translated to a pile of intermediate `muop`s. An
interesting fact: `tmp2` (a temporary register) is used in `sar_i32`
without initialization.
It's clearly `qemu` decoding pass `1.` bug (could have been optimizing pass
`2.` but I disabled it manually) which generates read from freshly introduced
temp register.

``` c
// somewhere in target-m68k/translate.c
static inline void shift_im(DisasContext *s, uint16_t insn, int opsize) {
....
            TCGv t0 = tcg_temp_new();
            tcg_gen_shri_i32(QREG_CC_V, reg, bits - 1 - count);
            tcg_gen_sar_i32(t0, reg, t0); // that's our broken shift!
            tcg_gen_not_i32(t0, t0);
            ...
```

I fixed that with [the simple
patch](https://github.com/vivier/qemu-m68k/pull/7) and reran test suite
again.

## hoop #4: cross-compiling doubles is hard

Or not that hard :) A lot of floating-point related tests worked without
crashes but resulted in garbage outputs.
The broken test was to print a `Float` (32bit `float` type in
`c`-speak):

``` haskell
v :: Float
v = 43
main = print v -- prints "0.0"
```

This code printed `"0.0"` when was built with `ghc-stage1`. At this
point I looked up if `m68k` was a big-endian platform by chance.
And it was!
`GHC` does weird things to encode double values in generated code.
Basically it uses platform's pointer-sized integrals to encode
everything. `GHC` calls the type `StgWord`:

``` C
// somewhere in includes/stg/Types.h
#if SIZEOF_INT == 4
typedef unsigned int             StgWord32;
#elif SIZEOF_LONG == 4
typedef unsigned long            StgWord32;
#else
#error GHC untested on this architecture: sizeof(int) != 4
#endif
...
#if SIZEOF_VOID_P == 8
typedef StgWord64          StgWord;
#else
#if SIZEOF_VOID_P == 4
typedef StgWord32          StgWord;
#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif
#endif
```

Thus in the end of the day `GHC` encodes `double` and `float`
values as `StgWord closure[] = { (StgWord)&useful_callback,
0x00000000, 0x40458000, ... }`.
It's an interesting question: how `43.0` should look like in this encoding?
Let's write a small program:

```c
int main() {
    double d = 43.0;
    float f  = 43.0;
    int * i2 = &d;      printf ("i[2]: %08X %08X\n", i2[0], i2[1]);
    long long * l = &d; printf ("   l: %016llX\n",   l[0]);
    int * i = &f;       printf ("   i: %08X\n",      i[0]);
}
```

And run it:

```
# 64bit-LE
$ x86_64-pc-linux-gnu-gcc a.c -o a && ./a
i[2]: 00000000 40458000
   l: 4045800000000000
   i: 422C0000

# 32bit-LE
$ i686-pc-linux-gnu-clang a.c -o a && ./a
i[2]: 00000000 40458000
   l: 4045800000000000
   i: 422C0000

# 32bit-BE
$ m68k-unknown-linux-gnu-gcc a.c -o a && ./a
i[2]: 40458000 00000000
   l: 4045800000000000
   i: 422C0000

# 64bit-BE
$ powerpc64-unknown-linux-gnu-gcc a.c -o a && ./a
i[2]: 40458000 00000000
   l: 4045800000000000
   i: 422C0000

# 64bit-BE
$ sparc64-unknown-linux-gnu-gcc a.c -o a && ./a
i[2]: 40458000 00000000
   l: 4045800000000000
   i: 422C0000
```

The only thing that changes here is the order of elements of `i[2]`
array. Just as like you would see if you stored 64-bit integral value.
The problem in original code was the assumption of target having the
same endianness as a host. It took me a while to fix it cleanly in
`GHC`: [the
result](https://git.haskell.org/ghc.git/commitdiff/c42cdb7f6dcfd519d9607ac9fa53f049b2922fb8).

## hoop #5: `Cmm` call annotations

Even more tests started working better but some `SIGSEGV`s still
remained. I looked at one example and noticed stack squeezing code
consistently crashed garbage collector right after
`threadStackUnderflow()` function call.
It was another case of `int` versus `void*` return type in
`m68k`. One of the intermediate representations (or just pieces of
low-level code written to deal with closures) is [`Cmm`
(c-minus-minus)](https://en.wikipedia.org/wiki/C--)
Suppose we need to call external `c` function from `haskell`. Typical
`cmm` code would look like that:

``` c
foo(bits32 a) {
    bits32 r;
    (r) = foreign "C" cfun(a);
    return (r);
}
```

Generated `c` code for it looks like that:

``` c
// $ inplace/bin/ghc-stage1 -c -keep-tmp-files a.cmm
// $ cat /tmp/ghc22508_0/ghc_3.hc
/* GHC_PACKAGES
*/
#include "Stg.h"

EFF_(cfun);
FN_(foo) {
StgWord32 _c1;
StgWord32 _c2;
W_ _c3;
StgWord32 _c4;
_c5:
_c1 = R1.w;
_c3 = (W_)&cfun;
_c4 = _c1;
_c2 = ((StgWord32 (*)(StgWord32))_c3)(_c4);;
R1.w = _c2;
JMP_(*((P_)(*Sp)));
}
```

Let's tweak original example and add a hint to result type:

``` diff
@@ -1,6 +1,6 @@
     foo(bits32 a) {
         bits32 r;
-        (r) = foreign "C" cfun(a);
+        ("ptr" r) = foreign "C" cfun(a);
         return (r);
     }
```

```diff
@@ -1,18 +1,18 @@
     /* GHC_PACKAGES
     */
     #include "Stg.h"
 ....
     EFF_(cfun);
     FN_(foo) {
     StgWord32 _c1;
     StgWord32 _c2;
     W_ _c3;
     StgWord32 _c4;
     _c5:
     _c1 = R1.w;
     _c3 = (W_)&cfun;
     _c4 = _c1;
-    _c2 = ((StgWord32 (*)(StgWord32))_c3)(_c4);;
+    _c2 = (StgWord32)((void * (*)(StgWord32))_c3)(_c4);;
     R1.w = _c2;
     JMP_(*((P_)(*Sp)));
     }
```

On most 32-bit platforms this change does nothing. But on `m68k` it's
precisely what flips between `%d0` and `%a0` return registers.
The fix for this issue looks [like
that](https://git.haskell.org/ghc.git/commitdiff/e46742f5c51938bc7c992ac37fecc6df8cab7647).
I tried to eyeball most of calls' return values but could have easily
missed something.
At this point I got `GHC`i working:

```
$ inplace/bin/ghc-stage2 --interactive
GHCi, version 8.1.20160305: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from /home/slyfox/.ghci
```

## The result

Not all tests are passing yet. But the result is good enough for people
to try out `GHC` and fix the bugs themselves. I've triaged some of
these failures but did not bother fixing them yet.

Fun facts (as usual):

- building `GHC` as a cross-compiler is simple!
- `m68k` is a 32-bit big-endian architecture
- `m68k` has alignment 2 (less than natural alignment)
- `m68k` is a rare system which uses different registers for integral
  and pointer types
- `qemu` (and it's `TCG`) is a fun tiny project to play with :)
- `binfmt_misc` `linux` kernel feature can run arbitrary binaries as
  executables on your system
- `double`s are not as scary as I thought :)
- `GHC` is now friendlier for BE platforms both as a host and
  target for cross-compilation :)

Thanks!
