---
title: "GCC bit shift handling on ia64"
date: December 23, 2017
---

Over the past week I've spent quite some time poking at toolchain
packages in Gentoo and fixing fresh batch of bugs. The bugs popped up
after recent introduction of `gcc` profiles that enable `PIE`
(position independent executables) and `SSP` (stack smash protected
executables) by default in `gentoo`. One would imagine `SSP` is more
invasive as it changes on-stack layout of things. But somehow `PIE`
yields bugs in every direction you throw it at:

- (not really from last week) `i386` static binaries `SIGSEGV` at
  startup: [bug](https://sourceware.org/PR21913) (TL;DR: calling
  `vdso`-implemented `SYSENTER` syscall entry point needs `TLS` to
  be already initialized in `glibc`. In this case `brk()` syscall
  was called before `TLS` was initialized.)
- `ia64`'s `glibc` miscompiles `librt.so`:
  [bug](https://bugs.gentoo.org/641216) (Tl;DR: `pie-by-default`
  `gcc` tricked `glibc` into believing `binutils` supports `IFUNC`
  on `ia64`. It does not, exactly as in [sparc
  bug](https://bugs.gentoo.org/336792#c26) 7 years ago.)
- `sparc`'s `glibc` produces broken static binaries on
  `pie-by-default` `gcc`: [bug](https://bugs.gentoo.org/640966).
  (TL;DR: C startup files for `sparc` were `PIC`-unfriendly)
- `powerpc`'s `gcc-7.2.0` and older produces broken binaries:
  [bug](https://sourceware.org/PR22626) (TL;DR: two bugs: `binutils`
  generates relocations to non-existent symbols, `gcc` hardcoded wrong
  `c` startup files to be used)
- `mips64` `gcc` fails to produce `glibc` that `binutils` can
  link without `assert()` failures (TODO: debug and fix)
- `m68k` `glibc` fails to compile (TODO: debug and fix)
- `sh4` `gcc` fails to compile by crashing with stack smash
  detection (TODO: debug and fix)
- more I forgot about

These are only toolchain bugs, and I did not yet try things on `arm`.
I'm sure other packages will uncover more interesting corner cases once
we fix the toolchain.

I won't write about any of the above here but it's a nice reference
point if you want to backport something to older toolchains to get
`PIE`/`SSP` work better. The bugs might also be a good reference
point on how to debug that kind of bugs. Today's story will be about
`gcc` code generation bug on `ia64` that has nothing to do with `PIE` or
`SSP`.

## Bug

On `#gentoo-ia64` Jason Duerstock asked if `openssl` fails tests on
modern `gcc-7.2.0`. It used to pass on `gcc-6.4.0` for him.
Having successfully dealt with [`IFUNC`
bug](https://bugs.gentoo.org/641216) recently I was confident things are
mostly working in `ia64` land and gave it a try.
`openssl` `DES` tests were indeed failing, claiming that
encryption/decryption roundtrip did not yield original data with errors
like:

``` 
des_ede3_cbc_encrypt decrypt error ...
```

## What does `DES` test do?

The test lives at
<https://github.com/openssl/openssl/blob/OpenSSL_1_0_2-stable/crypto/des/destest.c#L421>
and looks like that (simplified):

``` c
printf("Doing cbcm\n");
DES_set_key_checked(&cbc_key, &ks);
DES_set_key_checked(&cbc2_key, &ks2);
DES_set_key_checked(&cbc3_key, &ks3);

i = strlen((char *)cbc_data) + 1;

DES_ede3_cbcm_encrypt(cbc_data, cbc_out, 16L, &ks, &ks2, &ks3, &iv3, &iv2, DES_ENCRYPT);
DES_ede3_cbcm_encrypt(&cbc_data[16], &cbc_out[16], i - 16, &ks, &ks2, &ks3, &iv3, &iv2, DES_ENCRYPT);

DES_ede3_cbcm_encrypt(cbc_out, cbc_in, i, &ks, &ks2, &ks3, &iv3, &iv2, DES_DECRYPT);
if (memcmp(cbc_in, cbc_data, strlen((char *)cbc_data) + 1) != 0) {

    printf("des_ede3_cbcm_encrypt decrypt error\n");
    // ...
}
```

Test encrypts a few bytes of data in two takes (1: encrypt 16 bytes, 2:
encrypt rest) and then decrypts the result in one go. It works on static
data. Very straightforward.

## Fixing variables

My first suspect was `gcc` change as `7.2.0` fails, `6.4.0` works
and I tried to build `openssl` with optimisations completely disabled:

``` 
# CFLAGS=-O1 FEATURES=test emerge -1 =dev-libs/openssl-1.0.2n
...
des_ede3_cbc_encrypt decrypt error ...

# CFLAGS=-O0 FEATURES=test emerge -1 =dev-libs/openssl-1.0.2n
...
OK!
```

Aha! At least `-O0` seems to work. It means it will be easier to
cross-check which optimization pass affects code generation and find out
if it's an `openssl` bug or something else.

## Setting up A/B test

Debugging cryptographic algorithms like `DES` is very easy from this
standpoint because they don't need any external state: no services
running, no files created, input data is not randomized. They are a pure
function of input bit stream(s).
I unpacked single `openssl` source tree and started building it in two
directories: one with `-O0` optimisations, another with `-O1`:

``` 
~/openssl/openssl-1.0.2n-O0/
    `openssl-1.0.2n-.ia64/
        `crypto/des/destest.c (file-1)
        ...
~/openssl/openssl-1.0.2n-O0/
    `openssl-1.0.2n-.ia64/
        `crypto/des/destest.c (symlink to file-1)
        ...
```

Then I started sprinkling `printf()` statements in `destest.c` and
other `crypto/des/` files, then ran `destest` and diffed text
outputs to find where exactly difference appears first.
Relatively quickly I nailed it down to the following trace:

- [first call of
  `DES_ede3_cbcm_encrypt()`](https://github.com/openssl/openssl/blob/OpenSSL_1_0_2-stable/crypto/des/destest.c#L403)
- [first call of
  `DES_encrypt1()`](https://github.com/openssl/openssl/blob/OpenSSL_1_0_2-stable/crypto/des/ede_cbcm_enc.c#L99)
- [first expansion of `D_ENCRYPT()`
  macro](https://github.com/openssl/openssl/blob/OpenSSL_1_0_2-stable/crypto/des/des_enc.c#L96)
- [fourth `XOR` element in `D_ENCRYPT()`
  macro](https://github.com/openssl/openssl/blob/OpenSSL_1_0_2-stable/crypto/des/des_locl.h#L365)

``` c
unsigned int u;
...
#   define D_ENCRYPT(LL,R,S) {\
    LOAD_DATA_tmp(R,S,u,t,E0,E1); \
    t=ROTATE(t,4); \
    LL^=\
            DES_SPtrans[0][(u>> 2L)&0x3f]^ \
            DES_SPtrans[2][(u>>10L)&0x3f]^ \
            DES_SPtrans[4][(u>>18L)&0x3f]^ \
            DES_SPtrans[6][(u>>26L)&0x3f]^ \
            DES_SPtrans[1][(t>> 2L)&0x3f]^ \
            DES_SPtrans[3][(t>>10L)&0x3f]^ \
            DES_SPtrans[5][(t>>18L)&0x3f]^ \
            DES_SPtrans[7][(t>>26L)&0x3f]; }
```

See an error? Me neither.

## Minimizing test

`printf()` debugging suggested `DES_SPtrans[6][(u>>26L)&0x3f]`
returns different data in `-O0` and `-O1` cases. Namely the
following expression did change:

- `-O0`: `(u>>26L)&0x3f` yielded `0x33`
- `-O1`: `(u>>26L)&0x3f` yielded `0x173`

Note how it's logically infeasible to get anything more than `0x3f`
from that expression. And yet here we are with our `0x173` value.
I spent some time deleting lines one by one from all the macros as long
as the result kept producing the diff. Removing lines is safe in most of
`DES` code because all the test does is flipping a few bits and
rotating them within a single `unsigned int u` local variable.
After a while I came up with the following minimal reproducer:

``` c
#include <stdio.h>
typedef unsigned int u32;
u32 bug (u32 * result) __attribute__((noinline));
u32 bug (u32 * result)
{
    // non-static and volatile to inhibit constant propagation
    volatile u32 ss = 0xFFFFffff;
    volatile u32 d  = 0xEEEEeeee;
    u32 tt = d & 0x00800000;
    u32 r  = tt << 8;

    // rotate
    r = (r >> 31)
      | (r <<  1);

    u32 u = r^ss;
    u32 off = u >> 1;

    // seemingly unrelated but bug-triggering side-effect
    *result = tt;
    return off;
}

int main() {
    u32 l;
    u32 off = bug(&l);
    printf ("off>>: %08x\n", off);
    return 0;
}
```

``` 
$ gcc -O0 a.c -o a-O0 && ./a-O0 > o0
$ gcc -O1 a.c -o a-O1 && ./a-O1 > o1
$ diff -U0 o0 o1

-off>>: 7fffffff
+off>>: ffffffff
```

The test itself is very straightforward: it does only 32-bit
arithmetic on `unsigned int r` and prints the result. This is a very
fragile test: if you try to remove seemingly unrelated code like
`*result = tt;` the bug will disappear.

## What `gcc` actually does

I tried to look at the assembly code. If I could spot an obvious problem
I could inspect intermediate `gcc` steps to get the idea which pass
precisely introduces wrong resulting code. Despite being `Itanium` the
code is not that complicated (added detailed comments):

``` asm
Dump of assembler code for function bug:

mov r14=r12                 ; r12: register holding stack pointer
                            ; r14 = r12 (new temporary variable)
mov r15=-1                  ; r15=0xFFFFffff ('ss' variable)
st4.rel [r14]=r15,4         ; write 'ss' on stack address 'r14 - 4'

movl r15=0xffffffffeeeeeeee ; r15=0xEEEEeeee ('d' variable)
st4.rel [r14]=r15           ; write 'd' on stack address 'r14'
ld4.acq r15=[r14]           ; and quickly read 'd' back into 'r15' :)

movl r14=0x800000           ; r14=0x00800000
and r15=r14,r15             ; u32 tt = d & 0x00800000;

                            ; doing u32 r  = tt << 8;
dep.z r14=r15,8,24          ; "deposit" 24 bits from r14 into 15
                            ; starting at offset 8 and zeroing the rest.
                            ; Or in pictures (with offsets):
                            ; r14 = 0xAABBCCDD11223344
                            ;   r15 = 0x0000000022334400
ld4.acq r8=[r12]            ; read 'ss' into r8
st4 [r32]=r15               ; *result = tt

                            ; // rotate
                            ; r = (r >> 31)
                            ;   | (r <<  1);
mix4.r r14=r14,r14          ; This one is tricky: mix duplicates lower32
                            ; bits into lower and upper 32 bits of 14.
                            ; Or in pictures:
                            ;   r14 = 0xAABBCCDDF1223344 ->
                            ;   r14 = 0xF1223344F1223344
shr.u r14=r14,31            ; And shift right for 31 bit (with zero padding)
                            ;   r14 = 0x00000001E2446688
                            ; Note how '1' is in a position of 33-th bit
xor r8=r8,r14               ; u32 u = r^ss;

extr.u r8=r8,1,32           ; u32 off = u >> 1
                            ; "extract" 32 bits at offset 1 from r8 and put them to r8
                            ; Or in pictures:
                            ;     r8 = 0x0000000100000000 -> (note how all lower 32 bits are 0)
                            ;     r8 = 0x0000000080000000
br.ret.sptk.many b0         ; return r8 as a computation result
```

Tl;DR: `extr.u r8=r8,1,32` extracts 32 bits from 64-bit register at
offset 1 from `r8` and puts them into `r8` back. The problem is that
for `u32 off = u >> 1` to work correctly it should extract only
`31` bit, not `32`.
If I patch assembly file to contain `extr.u r8=r8,1,31` the sample
will work correctly.
At this point I shared my sample with Jason and filed [a `gcc`
bug](https://gcc.gnu.org/PR83565) as it was clear that compiler does
something very unexpected. Jason pulled in James and they produced a
working `gcc` patch for me while I was having dinner(!) :)

## `gcc` passes

What surprised me is the fact that` gcc` recognized "rotate" pattern and
used clever `mix4.r`/`shr.u` trick to achieve the bit rotation effect.
Internally `gcc` has a few frameworks to perform optimisations:

- high-level (relatively) target independent "tree" optimisations
  (working on `GIMPLE` representation)
- low-level target-specific "register" optimisations (working on
  `RTL` representation)

`GIMPLE` passes run before `RTL` passes. Let's check how many
passes are being ran on our small sample by using `-fdump-tree-all`
and `-fdump-rtl-all`:

``` 
$ ia64-unknown-linux-gnu-gcc -O1 -fdump-tree-all -fdump-rtl-all -S a.c && ls -1 | nl
1   a.c
2   a.c.001t.tu
3   a.c.002t.class
4   a.c.003t.original
5   a.c.004t.gimple
6   a.c.006t.omplower
7   a.c.007t.lower
8   a.c.010t.eh
9   a.c.011t.cfg
10  a.c.012t.ompexp
11  a.c.019t.fixup_cfg1
12  a.c.020t.ssa
13  a.c.022t.nothrow
14  a.c.027t.fixup_cfg3
15  a.c.028t.inline_param1
16  a.c.029t.einline
17  a.c.030t.early_optimizations
18  a.c.031t.objsz1
19  a.c.032t.ccp1
20  a.c.033t.forwprop1
21  a.c.034t.ethread
22  a.c.035t.esra
23  a.c.036t.ealias
24  a.c.037t.fre1
25  a.c.039t.mergephi1
26  a.c.040t.dse1
27  a.c.041t.cddce1
28  a.c.046t.profile_estimate
29  a.c.047t.local-pure-const1
30  a.c.049t.release_ssa
31  a.c.050t.inline_param2
32  a.c.086t.fixup_cfg4
33  a.c.091t.ccp2
34  a.c.094t.backprop
35  a.c.095t.phiprop
36  a.c.096t.forwprop2
37  a.c.097t.objsz2
38  a.c.098t.alias
39  a.c.099t.retslot
40  a.c.100t.fre3
41  a.c.101t.mergephi2
42  a.c.105t.dce2
43  a.c.106t.stdarg
44  a.c.107t.cdce
45  a.c.108t.cselim
46  a.c.109t.copyprop1
47  a.c.110t.ifcombine
48  a.c.111t.mergephi3
49  a.c.112t.phiopt1
50  a.c.114t.ch2
51  a.c.115t.cplxlower1
52  a.c.116t.sra
53  a.c.118t.dom2
54  a.c.120t.phicprop1
55  a.c.121t.dse2
56  a.c.122t.reassoc1
57  a.c.123t.dce3
58  a.c.124t.forwprop3
59  a.c.125t.phiopt2
60  a.c.126t.ccp3
61  a.c.127t.sincos
62  a.c.129t.laddress
63  a.c.130t.lim2
64  a.c.131t.crited1
65  a.c.134t.sink
66  a.c.138t.dce4
67  a.c.139t.fix_loops
68  a.c.167t.no_loop
69  a.c.170t.veclower21
70  a.c.172t.printf-return-value2
71  a.c.173t.reassoc2
72  a.c.174t.slsr
73  a.c.178t.dom3
74  a.c.182t.phicprop2
75  a.c.183t.dse3
76  a.c.184t.cddce3
77  a.c.185t.forwprop4
78  a.c.186t.phiopt3
79  a.c.187t.fab1
80  a.c.191t.dce7
81  a.c.192t.crited2
82  a.c.194t.uncprop1
83  a.c.195t.local-pure-const2
84  a.c.226t.nrv
85  a.c.227t.optimized
86  a.c.229r.expand
87  a.c.230r.vregs
88  a.c.231r.into_cfglayout
89  a.c.232r.jump
90  a.c.233r.subreg1
91  a.c.234r.dfinit
92  a.c.235r.cse1
93  a.c.236r.fwprop1
94  a.c.243r.ce1
95  a.c.244r.reginfo
96  a.c.245r.loop2
97  a.c.246r.loop2_init
98  a.c.247r.loop2_invariant
99  a.c.249r.loop2_doloop
100 a.c.250r.loop2_done
101 a.c.254r.dse1
102 a.c.255r.fwprop2
103 a.c.256r.auto_inc_dec
104 a.c.257r.init-regs
105 a.c.259r.combine
106 a.c.260r.ce2
107 a.c.262r.outof_cfglayout
108 a.c.263r.split1
109 a.c.264r.subreg2
110 a.c.267r.asmcons
111 a.c.271r.ira
112 a.c.272r.reload
113 a.c.273r.postreload
114 a.c.275r.split2
115 a.c.279r.pro_and_epilogue
116 a.c.280r.dse2
117 a.c.282r.jump2
118 a.c.286r.ce3
119 a.c.288r.cprop_hardreg
120 a.c.289r.rtl_dce
121 a.c.290r.bbro
122 a.c.296r.alignments
123 a.c.298r.mach
124 a.c.299r.barriers
125 a.c.303r.shorten
126 a.c.304r.nothrow
127 a.c.306r.final
128 a.c.307r.dfinish
129 a.c.308t.statistics
130 a.s
```

128 passes! Quite a few. `t` letter after pass number means `"tree"`
pass, `r` is an `"rtl"` pass. Note how all `"tree"` passes precede
`"rtl"` ones. Latest `"tree"` pass outputs the following:

``` 
;; cat a.c.227t.optimized
;; Function bug (bug, funcdef_no=23, decl_uid=2078, cgraph_uid=23, symbol_order=23)

__attribute__((noinline))
bug (u32 * result)
{
  u32 off;
  u32 u;
  u32 r;
  u32 tt;
  volatile u32 d;
  volatile u32 ss;
  unsigned int d.0_1;
  unsigned int ss.1_2;

  <bb 2> [100.00%]:
  ss ={v} 4294967295;
  d ={v} 4008636142;
  d.0_1 ={v} d;
  tt_6 = d.0_1 & 8388608;
  r_7 = tt_6 << 8;
  r_8 = r_7 r>> 31;
  ss.1_2 ={v} ss;
  u_9 = ss.1_2 ^ r_8;
  off_10 = u_9 >> 1;
  *result_11(D) = tt_6;
  return off_10;

}



;; Function main (main, funcdef_no=24, decl_uid=2088, cgraph_uid=24, symbol_order=24) (executed once)

main ()
{
  u32 off;
  u32 l;

  <bb 2> [100.00%]:
  off_3 = bug (&l);
  __printf_chk (1, "off>>: %08x\n", off_3);
  l ={v} {CLOBBER};
  return 0;

}
```

Code does not differ much from originally written code because I
specifically tried to write something that won't trigger any high-level
transformations.
`RTL` dumps are even more verbose. I'll show an incomplete snippet of
`bug()` function in `a.c.229r.expand` file:

``` lisp
;;
;; Full RTL generated for this function:
;;
(note 1 0 4 NOTE_INSN_DELETED)
(note 4 1 2 2 [bb 2] NOTE_INSN_BASIC_BLOCK)
(insn 2 4 3 2 (set (reg/v/f:DI 347 [ result ])
        (reg:DI 112 in0 [ result ])) "a.c":5 -1
     (nil))
(note 3 2 6 2 NOTE_INSN_FUNCTION_BEG)
(insn 6 3 7 2 (set (reg:SI 348)
        (const_int -1 [0xffffffffffffffff])) "a.c":7 -1
     (nil))
(insn 7 6 8 2 (set (mem/v/c:SI (reg/f:DI 335 virtual-stack-vars) [1 ss+0 S4 A128])
        (reg:SI 348)) "a.c":7 -1
     (nil))
(insn 8 7 9 2 (set (reg:DI 349)
        (reg/f:DI 335 virtual-stack-vars)) "a.c":8 -1
     (nil))
(insn 9 8 10 2 (set (reg/f:DI 350)
        (plus:DI (reg/f:DI 335 virtual-stack-vars)
            (const_int 4 [0x4]))) "a.c":8 -1
     (nil))
(insn 10 9 11 2 (set (reg:SI 351)
        (const_int -286331154 [0xffffffffeeeeeeee])) "a.c":8 -1
     (nil))
(insn 11 10 12 2 (set (mem/v/c:SI (reg/f:DI 350) [1 d+0 S4 A32])
        (reg:SI 351)) "a.c":8 -1
     (nil))
(insn 12 11 13 2 (set (reg:DI 352)
        (reg/f:DI 335 virtual-stack-vars)) "a.c":9 -1
     (nil))
(insn 13 12 14 2 (set (reg/f:DI 353)
        (plus:DI (reg/f:DI 335 virtual-stack-vars)
            (const_int 4 [0x4]))) "a.c":9 -1
     (nil))
(insn 14 13 15 2 (set (reg:SI 340 [ d.0_1 ])
        (mem/v/c:SI (reg/f:DI 353) [1 d+0 S4 A32])) "a.c":9 -1
     (nil))
(insn 15 14 16 2 (set (reg:DI 355)
        (const_int 8388608 [0x800000])) "a.c":9 -1
     (nil))
(insn 16 15 17 2 (set (reg:DI 354)
        (and:DI (subreg:DI (reg:SI 340 [ d.0_1 ]) 0)
            (reg:DI 355))) "a.c":9 -1
     (nil))
(insn 17 16 18 2 (set (reg/v:SI 342 [ tt ])
        (subreg:SI (reg:DI 354) 0)) "a.c":9 -1
     (nil))
(insn 18 17 19 2 (set (reg/v:SI 343 [ r ])
        (ashift:SI (reg/v:SI 342 [ tt ])
            (const_int 8 [0x8]))) "a.c":10 -1
     (nil))
(insn 19 18 20 2 (set (reg:SI 341 [ ss.1_2 ])
        (mem/v/c:SI (reg/f:DI 335 virtual-stack-vars) [1 ss+0 S4 A128])) "a.c":16 -1
     (nil))
(insn 20 19 21 2 (set (mem:SI (reg/v/f:DI 347 [ result ]) [1 *result_11(D)+0 S4 A32])
        (reg/v:SI 342 [ tt ])) "a.c":20 -1
     (nil))
(insn 21 20 22 2 (set (reg:SI 357 [ r ])
        (rotate:SI (reg/v:SI 343 [ r ])
            (const_int 1 [0x1]))) "a.c":13 -1
     (nil))
(insn 22 21 23 2 (set (reg:DI 358)
        (xor:DI (subreg:DI (reg:SI 357 [ r ]) 0)
            (subreg:DI (reg:SI 341 [ ss.1_2 ]) 0))) "a.c":16 -1
     (nil))
(insn 23 22 24 2 (set (reg:DI 359)
        (zero_extract:DI (reg:DI 358)
            (const_int 31 [0x1f])
            (const_int 1 [0x1]))) "a.c":17 -1
     (nil))
(insn 24 23 25 2 (set (subreg:DI (reg:SI 356 [ off ]) 0)
        (reg:DI 359)) "a.c":17 -1
     (nil))
(insn 25 24 29 2 (set (reg/v:SI 346 [ <retval> ])
        (reg:SI 356 [ off ])) "a.c":21 -1
     (nil))
(insn 29 25 30 2 (set (reg/i:SI 8 r8)
        (reg/v:SI 346 [ <retval> ])) "a.c":22 -1
     (nil))
(insn 30 29 0 2 (use (reg/i:SI 8 r8)) "a.c":22 -1
     (nil))
```

The above is `RTL` representation of our `bug()` function.
`S`-expressions look like machine instructions but not quite.
For example the following snippet (single `S`-expression) introduces new
virtual register `351` which should receive literal value of
`0xeeeeeeee`. `SI` means `SImode`, or 32-bit signed integer:
More on modes is
[here](https://gcc.gnu.org/onlinedocs/gccint/Machine-Modes.html).

``` lisp
(insn 10 9 11 2 (set (reg:SI 351)
        (const_int -286331154 [0xffffffffeeeeeeee])) "a.c":8 -1
     (nil))
```

Or just `r351 = 0xeeeeeeee` :) Note it also mentions source file line
numbers. Useful when mapping `RTL` logs back to source files (and I
guess `gcc` uses the same to emit `dwarf` debugging sections).
Another example of `RTL` instruction:

``` lisp
(insn 22 21 23 2 (set (reg:DI 358)
        (xor:DI (subreg:DI (reg:SI 357 [ r ]) 0)
            (subreg:DI (reg:SI 341 [ ss.1_2 ]) 0))) "a.c":16 -1
     (nil))
```

Here we see an `RTL` equivalent of `u_9 = ss.1_2 ^ r_8;` code
(`GIMPLE`). There is more subtlety here: `xor` itself operates on
64-bit integers (`DI`) that contain 32-bit values in lower 32-bits
(`subreg`s) which I don't really understand.
Upstream [bug](https://gcc.gnu.org/PR83565) attempts to decide which
assumption is violated in generic `RTL` optimization pass (or backend
implementation). At the time of writing this post a few candidate
patches were posted to address the bug.

## `gcc` `RTL` optimisations

I was interested in the optimization that converted `extr.u
r8=r8,1,31` (valid) to `extr.u r8=r8,1,32` (invalid).
Its `GIMPLE` representation is:

``` 
// ...
off_10 = u_9 >> 1;
*result_11(D) = tt_6;
return off_10;
```

Let's try to find `RTL` representation of this construct in the very
first `RTL` dump (`a.c.229r.expand` file):

``` lisp
(insn 23 22 24 2 (set (reg:DI 359)
        (zero_extract:DI (reg:DI 358)
            (const_int 31 [0x1f])
            (const_int 1 [0x1]))) "a.c":17 -1
     (nil))
;; ...
(insn 24 23 25 2 (set (subreg:DI (reg:SI 356 [ off ]) 0)
        (reg:DI 359)) "a.c":17 -1
     (nil))
(insn 25 24 29 2 (set (reg/v:SI 346 [ <retval> ])
        (reg:SI 356 [ off ])) "a.c":21 -1
     (nil))
(insn 29 25 30 2 (set (reg/i:SI 8 r8)
        (reg/v:SI 346 [ <retval> ])) "a.c":22 -1
     (nil))
(insn 30 29 0 2 (use (reg/i:SI 8 r8)) "a.c":22 -1
     (nil))
```

Or in a slightly more concise form:

``` 
reg359 = zero_extract(reg358, offset=1, length=31)
reg356 = (u32)reg359;
reg346 = (u32)reg356;
reg8   = (u32)reg346;
```

Very straightforward (and still correct). `zero_extract` is some
`RTL` virtual operation that will have to be expressed as real
instruction at some point.
In the absence of other optimisations it's done with the following rule
(at
<https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/config/ia64/ia64.md;h=b7cd52ba366ba3d63e98479df5f4be44ffd17ca6;hb=HEAD#l1381>)

``` lisp
(define_insn "extzv"
  [(set (match_operand:DI 0 "gr_register_operand" "=r")
        (zero_extract:DI (match_operand:DI 1 "gr_register_operand" "r")
                         (match_operand:DI 2 "extr_len_operand" "n")
                         (match_operand:DI 3 "shift_count_operand" "M")))]
  ""
  "extr.u %0 = %1, %3, %2"
  [(set_attr "itanium_class" "ishf")])
```

In unoptimized case we see all the intermediate assignments are stored
at `r14` address and loaded back.

``` asm
;; gcc -O0 a.c
...
extr.u r15 = r15, 1, 31
;;
st4 [r14] = r15
mov r14 = r2
;;
ld8 r14 = [r14]
adds r16 = -32, r2
;;
ld4 r15 = [r16]
;;
st4 [r14] = r15
adds r14 = -20, r2
;;
ld4 r14 = [r14]
;;
mov r8 = r14
.restore sp
mov r12 = r2
br.ret.sptk.many b0
```

To get rid of all the needless operations `RTL` applies extensive list
of optimisations. Each `RTL` dump contains summary of the pass effect
on the file. Let's look at the `a.c.232r.jump`:

``` 
Deleted 2 trivially dead insns
3 basic blocks, 2 edges.
```

Around `a.c.257r.init-regs` pass our `RTL` representation shrinks
into the following:

``` lisp
(insn 23 22 24 2 (set (reg:DI 359)
        (zero_extract:DI (reg:DI 358)
            (const_int 31 [0x1f])
            (const_int 1 [0x1]))) "a.c":17 159 {extzv}
     (expr_list:REG_DEAD (reg:DI 358)
        (nil)))
(insn 24 23 29 2 (set (subreg:DI (reg:SI 356 [ off ]) 0)
        (reg:DI 359)) "a.c":17 6 {movdi_internal}
     (expr_list:REG_DEAD (reg:DI 359)
        (nil)))
(insn 29 24 30 2 (set (reg/i:SI 8 r8)
        (reg:SI 356 [ off ])) "a.c":22 5 {movsi_internal}
     (expr_list:REG_DEAD (reg:SI 356 [ off ])
        (nil)))
(insn 30 29 0 2 (use (reg/i:SI 8 r8)) "a.c":22 -1
     (nil))
```

Or in a slightly more concise form:

``` 
reg359 = zero_extract(reg358, offset=1, length=31)
reg356 = (u32)reg359;
reg8   = (u32)reg356;
```

Note how `reg346 = (u32)reg356;` is already gone away by now.
The most magic happened in a single `a.c.259r.combine` pass. Here is
it's report:

``` 
;; Function bug (bug, funcdef_no=23, decl_uid=2078, cgraph_uid=23, symbol_order=23)

starting the processing of deferred insns
ending the processing of deferred insns
df_analyze called
insn_cost 2: 4
insn_cost 6: 4
insn_cost 32: 4
insn_cost 7: 4
insn_cost 10: 4
insn_cost 11: 4
insn_cost 14: 4
insn_cost 15: 4
insn_cost 16: 4
insn_cost 18: 4
insn_cost 19: 4
insn_cost 20: 4
insn_cost 21: 4
insn_cost 22: 4
insn_cost 23: 4
insn_cost 24: 4
insn_cost 29: 4
insn_cost 30: 0
allowing combination of insns 2 and 20
original costs 4 + 4 = 8
replacement cost 4
deferring deletion of insn with uid = 2.
modifying insn i3    20: [in0:DI]=r354:DI#0
      REG_DEAD in0:DI
      REG_DEAD r354:DI
deferring rescan insn with uid = 20.
allowing combination of insns 23 and 24
original costs 4 + 4 = 8
replacement cost 4
deferring deletion of insn with uid = 23.
modifying insn i3    24: r356:SI#0=r358:DI 0>>0x1
  REG_DEAD r358:DI
deferring rescan insn with uid = 24.
allowing combination of insns 24 and 29
original costs 4 + 4 = 8
replacement cost 4
deferring deletion of insn with uid = 24.
modifying insn i3    29: r8:DI=zero_extract(r358:DI,0x20,0x1)
      REG_DEAD r358:DI
deferring rescan insn with uid = 29.
starting the processing of deferred insns
rescanning insn with uid = 20.
rescanning insn with uid = 29.
ending the processing of deferred insns
```

The essential output relevant to our instruction is:

``` 
allowing combination of insns 23 and 24
allowing combination of insns 24 and 29
```

This combines three instructions:

``` 
reg359 = zero_extract(reg358, offset=1, length=31)
reg356 = (u32)reg359;
reg8   = (u32)reg356;
```

Into one:

``` 
reg8 = zero_extract(reg358, offset=1, length=32)
```

The problem is in combiner that decided higher bit is zero anyway (false
assumption) and applied `zero_extract` to full 32-bits. Why exactly it
decided 32 upper bits are zero (they are not) is not clear to me :) It
happens somewhere in
<https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/combine.c;h=31e6a4f68254fab551300252688a52d8c3dcaaa4;hb=HEAD#l2628>
where some `if()` statements take about a page. Hopefully `gcc`
`RTL` and `ia64` maintainers will help and guide us here.

## Parting words

- Having good robust tests in packages is always great as it helps
  gaining confidence package is not completely broken on new target
  platforms (or toolchain versions).
- `printf()` is still good enough tool to trace compiler bugs :)
- `-fdump-tree-all` and `-fdump-rtl-all` are useful `gcc` flags to
  get the idea why optimisations fire (or don't).
- `gcc` performs very unobvious optimisations!

Have fun!
