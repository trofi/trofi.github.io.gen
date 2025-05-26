---
title: "signed char or unsigned char?"
date: September 17, 2017
---

Yesterday I debugged an interesting
[bug](https://bugs.gentoo.org/630698): `sqlite` test suite was hanging up
on `csv` parsing test on `powerpc32` and `powerpc64` platforms. But
other platforms were fine! `ia64` was ok, `hppa` was ok, `sparc`
was ok. Thus it's not (just) endianness issue or stack growth
direction. What could it be?

It took me a while to debug the issue but it boiled down to an infinite
loop of trying to find `EOF` when reading a file. Let's look at the
simplified version of buggy code in `sqlite`:

``` c
#include <stdio.h> /* #define EOF (-1) */

int main() {
    int n = 0;
    FILE * f = fopen ("/dev/null", "rb"); // supposed to have 0 bytes
    for (;;) {
        char c = fgetc (f); // truncate 'int' to char
        if (c == EOF) break;
        ++n;
    }
    return n;
}
```

The code is supposed reach end of file (or maybe `'xFF'` symbol) and
finish. Normally it's exactly what happens:

``` 
$ x86_64-pc-linux-gnu-gcc a.c -o a && ./a && echo $?
0
```

But not on `powerpc64`:

``` 
$ powerpc64-unknown-linux-gnu-gcc a.c -o a && ./a && echo $?
<hung>
```

The bug here is simple: `c == EOF` promotes both operands `char c`
and `-1` to `int`. Thus for `EOF` case condition looks like that:

``` c
((int)(char)(-1) == -1)
```

So why does it never fire on `powerpc64`? Because it's an `unsigned
char` platform! As a result it looks like two different conditions:

``` c
((int)0xff == -1) // powerpc64

((int)(-1) == -1) // x86_64
```

Once we know the problem we can force `x86_64` to hang as well by
using `-funsigned-char` `gcc` option:

``` 
$ x86_64-pc-linux-gnu-gcc a.c -o a -funsigned-char && ./a && echo $?
<hung>
```

I did not encounter bugs related to char signedness for quite a while.
What are other platforms defaulting to `unsigned char`? I tried the simple
hack (I've encountered it
[today](https://github.com/taglib/taglib/pull/834) due to changes in
`c++11` to forbid narrowing conversion in initializers):

``` 
$ cat a.cc
char c[] = { -1 };

for cxx in /usr/bin/*-g++; do echo -n "$cxx "; $cxx -c a.cc 2>/dev/null && echo SIGNED || echo UNSIGNED; done | sort -k2

/usr/bin/afl-g++ SIGNED
/usr/bin/alpha-unknown-linux-gnu-g++ SIGNED
/usr/bin/hppa-unknown-linux-gnu-g++ SIGNED
/usr/bin/hppa2.0-unknown-linux-gnu-g++ SIGNED
/usr/bin/i686-w64-mingw32-g++ SIGNED
/usr/bin/ia64-unknown-linux-gnu-g++ SIGNED
/usr/bin/m68k-unknown-linux-gnu-g++ SIGNED
/usr/bin/mips64-unknown-linux-gnu-g++ SIGNED
/usr/bin/sh4-unknown-linux-gnu-g++ SIGNED
/usr/bin/sparc-unknown-linux-gnu-g++ SIGNED
/usr/bin/sparc64-unknown-linux-gnu-g++ SIGNED
/usr/bin/x86_64-HEAD-linux-gnu-g++ SIGNED
/usr/bin/x86_64-UNREG-linux-gnu-g++ SIGNED
/usr/bin/x86_64-pc-linux-gnu-g++ SIGNED
/usr/bin/x86_64-w64-mingw32-g++ SIGNED

/usr/bin/aarch64-unknown-linux-gnu-g++ UNSIGNED
/usr/bin/armv5tel-softfloat-linux-gnueabi-g++ UNSIGNED
/usr/bin/armv7a-hardfloat-linux-gnueabi-g++ UNSIGNED
/usr/bin/armv7a-unknown-linux-gnueabi-g++ UNSIGNED
/usr/bin/powerpc-unknown-linux-gnu-g++ UNSIGNED
/usr/bin/powerpc64-unknown-linux-gnu-g++ UNSIGNED
/usr/bin/powerpc64le-unknown-linux-gnu-g++ UNSIGNED
/usr/bin/s390x-unknown-linux-gnu-g++ UNSIGNED
```

Or in a shorter form:

- signed: `alpha`, `hppa`, `x86`, `ia64`, `m68k`, `mips`,
  `sh`, `sparc`
- unsigned: `arm`, `powerpc`, `s390`

Why would compiler prefer one signedness over another? The answer is the
underlying Instruction Set Architecture. Or ... not :), read on!

Let's look at generated code for two simple functions fetching single
char from memory into register and compare generated code:

``` c
signed   long sc2sl (signed   char * p) { return *p; }
unsigned long uc2ul (unsigned char * p) { return *p; }
```

## `Alpha`

`Alpha` is a 64-bit architecture. Does not support unaligned reads in its
basic ISA. **You have been warned**.

``` asm
; alpha-unknown-linux-gnu-gcc -O2 -c a.c && objdump -d a.o

sc2sl:
                     ;     example:  0x12345(BB address, p)
                     ;               |  0x12346(CC address, p+1)
                     ;               v  v
                     ; mem: [  .. AA BB CC DD .. ]
                     ;               a0 = 0x12345
    lda     t0,1(a0) ; load address: t0 = p+1
                     ;               t0 = 0x12346 (a0 + 1)
    ldq_u   v0,0(a0) ; load unaligned: v0 = *(long*)(align(p))
                     ;               v0 = *(long*)0x12340
                     ;               v0 = 0xDDCCBBAA????????
    extqh   v0,t0,v0 ; extract actual byte into MSB position
                     ;               v0 = v0 << 16
                     ;               v0 = 0xBBAA????????0000
    sra     v0,56,v0 ; get sign-extended byte using arithmetic shift-right
                     ;               v0 = v0 >> 56
                     ;               v0 = 0xFFFFFFFFFFFFFFBB
    ret              ; return

uc2ul:
   ldq_u   v0,0(a0)  ; load unaligned: v0 = *(long*)(align(p))
   extbl   v0,a0,v0  ; extract byte in v0
   ret
```

In this case `alpha` handles unsigned load slightly nicer (does not
require arithmetic shift and shift offset computation). It takes quite a
bit of time to understand `sc2sl` implementation.

`creemj` noted on `#gentoo-alpha` `BWX` ISA extension (enabled
with `-mbwx` in `gcc`):

``` asm
; alpha-unknown-linux-gnu-gcc -O2 -mbwx -c a.c && objdump -d a.o

sc2sl:
    ldbu    v0,0(a0)
    sextb   v0,v0 ; sign-extend-byte
    ret
uc2ul:
    ldbu    v0,0(a0)
    ret
```

Here signed load requires one instruction to amend default-unsigned load
semantics.

## `HPPA` (PA-RISC)

Currently `HPPA` userland supports only 32-bit mode on `linux`. Similar to
many RISC architectures its branching instructions take two clock cycles
to execute. By convention it means the next instruction right after
branch (`bv` instruction) is also executed.

``` asm
; hppa2.0-unknown-linux-gnu-gcc -O2 -c a.c && objdump -d a.o

sc2sl:
    ldb 0(r26),ret0         ; load byte
    bv r0(rp)               ; return
     extrw,s ret0,31,8,ret0 ; sign-extend 8 bits into 31

uc2ul:
    bv r0(rp)     ; return
     ldb 0(r26),ret0
```

Similar to `Alpha` signed chars require one more arithmetic operation.

## `x86`

64-bit mode:

``` asm
; x86_64-pc-linux-gnu-gcc -O2 -c a.c && objdump -d a.o

sc2sl:
    movsbq (%rdi),%rax ; load/sign-extend byte to quad
    retq
uc2ul:
    movzbl (%rdi),%eax ; load/zero-extend byte to long
    retq
```

Note the difference between target operands (64 vs. 32 bits). `x86_64`
implicitly zeroes out register part for us in 64-bit mode.

32-bit mode:

``` asm
; x86_64-pc-linux-gnu-gcc -O2 -m32 -c a.c && objdump -d a.o

sc2sl:
    mov    0x4(%esp),%eax
    movsbl (%eax),%eax
    ret

uc2ul:
    mov    0x4(%esp),%eax
    movzbl (%eax),%eax
    ret
```

No surprises here. Argument is passed through stack.

## `ia64`

`ia64` "instructions" are huge. They are 128-bit long and encode 3
real instructions. Result of memory fetch is not used in the same bundle
thus we need at least two bundles to fetch and shift. (I don't know why
yet, either in order to avoid memory stall in the same bundle or it's a
"Write ; Read-Write" conflict on `r8` in a single bundle)

``` asm
; ia64-unknown-linux-gnu-gcc -O2 -c a.c && objdump -d a.o

sc2sl:
  [MMI] nop.m 0x0
        ld1 r8=[r32]     # load byte (implicit zero-extend)
        nop.i 0x0;;
  [MIB] nop.m 0x0
        sxt1 r8=r8       # sign-extend
        br.ret.sptk.many b0;;
uc2ul:
  [MIB] ld1 r8=[r32]     # load byte (implicit zero-extend)
        nop.i 0x0
        br.ret.sptk.many b0;;
```

Unsigned char load requires fewer instructions (no additional shift
required).

## `m68k`

For some reason frame pointer is still preserved on `-O2`. I've
disabled it with `-fomit-frame-pointer` to make assembly shorter:

``` asm
; m68k-unknown-linux-gnu-gcc -O2 -fomit-frame-pointer -c a.c && objdump -d a.o

sc2sl:
    moveal %sp@(4),%a0 ; arguments are passed through stack (as would be in i386)
    moveb %a0@,%d0     ; load byte
    extbl %d0          ; sign-extend result
    rts

uc2ul:
    moveal %sp@(4),%a0
    clrl %d0          ; zero destination register
    moveb %a0@,%d0    ; load byte
    rts
```

Both functions are similar. Both require arithmetic fiddling.

## `mips`

Similar to `HPPA` has the same rule of executing one instruction after
branch instruction.

``` asm
; mips64-unknown-linux-gnu-gcc -O2 -fomit-frame-pointer -c a.c && objdump -d a.o

sc2sl:
    jr      ra
     lb      v0,0(a0) ; load byte (sign-extend)

uc2ul:
    jr      ra
     lbu     v0,0(a0) ; load byte (zero-extend)
```

Both functions are taking exactly one instruction.

## `SuperH`

Similar to `HPPA` has the same rule of executing one instruction after
branch instruction.

``` asm
; sh4-unknown-linux-gnu-gcc -O2 -fomit-frame-pointer -c a.c && objdump -d a.o

sc2sl:
    rts
     mov.b   @r4,r0 ; load byte (sign-extend)

uc2ul:
    mov.b   @r4,r0 ; load byte (sign-extend)
    rts
     extu.b  r0,r0 ; zero-extend result
```

Here unsigned load requires one instruction to amend default-signed load
semantics.

## `SPARC`

Similar to `HPPA` has the same rule of executing one instruction after
branch instruction.

``` asm
; sparc-unknown-linux-gnu-gcc -O2 -fomit-frame-pointer -c a.c && objdump -d a.o

sc2sl:
    retl
     ldsb  [ %o0 ], %o0

uc2ul:
    retl
     ldub  [ %o0 ], %o0
```

Both functions are taking exactly one instruction.

## `ARM`

``` asm
; armv5tel-softfloat-linux-gnueabi-gcc -O2 -fomit-frame-pointer -c a.c && armv5tel-softfloat-linux-gnueabi-objdump -d a.o

sc2sl:
    ldrsb   r0, [r0] ; load/sign-extend
    bx      lr

uc2ul:
    ldrb    r0, [r0] ; load/zero-extend
    bx      lr
```

Both functions are taking exactly one instruction.

## `PowerPC`

`PowerPC` generates quite inefficient code for `-fPIC` mode. Enabling
`-fno-PIC` by default.

``` asm
; powerpc-unknown-linux-gnu-gcc -O2 -fomit-frame-pointer -fno-PIC -c a.c && objdump -d a.o

sc2sl:
    lbz     r3,0(r3) ; load-byte/zero-extend
    extsb   r3,r3    ; sign-extend
    blr
    nop

uc2ul:
    lbz     r3,0(r3) ; load-byte/zero-extend
    blr
```

Here signed load requires one instruction to amend default-unsigned load
semantics.

## `S390`

64-bit mode:

``` asm
; s390x-unknown-linux-gnu-gcc -O2 -fomit-frame-pointer -fno-PIC -c a.c && objdump -d a.o

sc2sl:
    icmh    %r2,8,0(%r2) ; insert-characters-under-mask-64
    srag    %r2,%r2,56   ; shift-right-single-64
    br      %r14

uc2ul:
    llgc    %r2,0(%r2) ; load-logical-character
    br      %r14
```

Most esoteric instruction set :) It looks like unsigned loads are
slightly shorter here.

"31"-bit mode (note `-m31`):

``` asm
; s390x-unknown-linux-gnu-gcc -m31 -O2 -fomit-frame-pointer -fno-PIC -c a.c && objdump -d a.o

sc2sl:
    icm     %r2,8,0(%r2) ; insert-characters-under-mask-64
    sra     %r2,24       ; shift-right-single
    br      %r14

uc2ul:
    lhi     %r1,0        ; load-halfword-immediate
    ic      %r1,0(%r2)   ; insert-character
    lr      %r2,%r1      ; register-to-register(?) move
    br      %r14
```

Surprisingly in 31-bit mode signed stores are slightly shorter. But it
looks like `uc2ul` could be shorter by eliminating `lr`.

## Parting words

At least from ISA standpoint some architectures treat `signed char`
and `unsigned char` equally and could pick any signedness. Others
differ quite a bit.

Here is my silly table:

  architecture   signedness   preferred signedness   match
  -------------- ------------ ---------------------- --------
  alpha          SIGNED       UNSIGNED               **NO**
  arm            UNSIGNED     AMBIVALENT             YES
  hppa           SIGNED       UNSIGNED               **NO**
  ia64           SIGNED       UNSIGNED               **NO**
  m68k           SIGNED       AMBIVALENT             YES
  mips           SIGNED       AMBIVALENT             YES
  powerpc        UNSIGNED     UNSIGNED               YES
  s390(64)       UNSIGNED     UNSIGNED               YES
  sh             SIGNED       SIGNED                 YES
  sparc          SIGNED       AMBIVALENT             YES
  x86            SIGNED       AMBIVALENT             YES

What do we see here:

- `alpha` follows the majority of architecture in char signedness but
  pays for it a lot.
- `arm` could have been signed just fine (for this tiny silly test)
- `hppa` and `ia64` might be unsigned and balance the table a bit
  (**6/5** versus **8/3**) :)

Have fun!
