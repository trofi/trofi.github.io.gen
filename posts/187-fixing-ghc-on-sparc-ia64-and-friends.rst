---
title: fixing ghc on sparc ia64 and friends
date: Sep 13, 2014
---

:PostID: 187
:Title: fixing ghc on sparc ia64 and friends
:Keywords: ghc, haskell, sparc, ia64
:Categories: news

The **ghc-7.8.1** `release <http://gentoohaskell.wordpress.com/2014/02/08/7-8-1-rc1-gentoo-experience/>`_ was the first release
where **ghc** binary (and tools like **ghc-pkg**, **hpc**, **hsc2hs**, etc.)
was dynamically linked against haskell libraries shipped with **ghc**.

At that time:

- **ia64** (unreported, not easy to build :]),
- **sparc** (`#8857 <https://ghc.haskell.org/trac/ghc/ticket/8857>`_),
- **ppc** (`#8024 <https://ghc.haskell.org/trac/ghc/ticket/8024>`_)

all crashed when trying to use dynamic linking in **ghc-7.8**.

Having `upstreamed <https://ghc.haskell.org/trac/ghc/ticket/8748>`_
simplest build fixes for unreg arches I foolishly hoped all other
architectures would Just Work, but fun stuff only started to happen.

To know all the gory details read on! :]

.. raw:: html

   <!--more-->

amd64 bugs:
===========

Being bitten by `#8748 <https://ghc.haskell.org/trac/ghc/ticket/8748>`_
before trying real exotics like **sparc** I first decided to
check how good (or bad) **UNREG** **amd64** build was.

To get a feeling about state of some platform
you only need to build ghc and run regression tests:

.. code-block:: bash

    ./configure --enable-unregisterised
    make
    make fulltest # run regtest

There was about 200 tests of 4000 broken (which is quite nice, but not ideal).
The first thing caught my eye was broken arithmetics in rare cases
like `integerConversions <https://git.haskell.org/ghc.git/blob/master:/testsuite/tests/lib/integer/integerConversions.hs>`_.

Tests gave incorrect results for code like

.. code-block:: haskell

    Prelude> 4 - 2^64 == 0
    True

Thinking about it as The Root Cause of all **SIGSEGV**s I have ever seen
I've started trimming bits down to minimal **.cmm** snippet.

And got this:

.. code-block:: c

    CInt a = -1;
    return (a == -1)
    # returns False

At that stage exploring generated C code is trivial which gave:

.. code-block:: c

    StgWord64 a = (StgWord32)0xFFFFffffFFFFffffu;
    return (a == 0xFFFFffffFFFFffffu)

The bug here is in pretty-printing 32-bit constant, which was `easy to fix <https://git.haskell.org/ghc.git/commitdiff/43f1b2ecd1960fa7377cf55a2b97c66059a701ef>`_
with Reid's help.

sparc bug:
==========

But integer-literals fix didn't help dynamic binaries on **sparc** and I moved to explore
on that box.

The immediate symptom on **sparc** was a **SIGSEGV** somewhere before
program's **main()** entry point. And that was very weird.

I expected C part of haskell runtime to run first and crash later.
My plan was to break on **main()** and step-by-step get basic understanding
on what was going on.

Thus I tried to update **binutils**, **gcc** and **glibc** first in hope
of some toolchain bug.

No luck, the bug persisted in exactly the same form.

Backtrace of core dump suggested crash was happening somewhere in
**foreignExportStablePtr** function, which gets registered
at binary (or library) load time `before <https://git.haskell.org/ghc.git/blob/master:/compiler/deSugar/DsForeign.lhs#l667>`_ your **main()** entry point.
That **__attribute__((constructor));** makes the magic happen.

For such haskell code

.. code-block:: haskell

    module M where
    import Foreign.C.Types
    foreign export ccall f :: CInt -> CInt
    f :: CInt -> CInt
    f n = n + 1

**ghc** basically generates the following stub:

.. code-block:: c

    extern StgClosure M_zdfstableZZC0ZZCmainZZCMZZCf_closure;
    HsInt32 f(HsInt32 a1)
    {
        Capability *cap;
        HaskellObj ret;
        HsInt32 cret;
        cap = rts_lock();
        rts_evalIO(&cap,rts_apply(cap,(HaskellObj)runNonIO_closure,rts_apply(cap,&M_zdfstableZZC0ZZCma
        inZZCMZZCf_closure,rts_mkInt32(cap,a1))) ,&ret);
        rts_checkSchedStatus("f",cap);
        cret=rts_getInt32(ret);
        rts_unlock(cap);
        return cret;
    }
    /* our static constructor */
    static void stginit_export_M_zdfstableZZC0ZZCmainZZCMZZCf() __attribute__((constructor));
    static void stginit_export_M_zdfstableZZC0ZZCmainZZCMZZCf()
    {foreignExportStablePtr((StgPtr) &M_zdfstableZZC0ZZCmainZZCMZZCf_closure);}

sparc solution:
===============

Having smaller program it's once againt simpler to explore the breakage.

**RISC** CPUs are fun creatures.

To feel all the delight of looking at the **sparc** assembly I propose to look at
the generated code for the following C snippet:

.. code-block:: c

    unsigned unt f(void)
    {
        return 0x1234ABCD;
    }


**i386** easily puts a value into a register:

.. code-block:: asm

    movl $0x1234ABCD, %eax

while **sparc32** has hard time:

.. code-block:: asm

    sethi %hi(0x1234A800), %o0
    or    %o0, 0x3CD, %o0

You can't encode 32-bit immediate in a 32-bit instruction
containing a tuple of **(opcode, dest-reg, imm)**.
Opcode usually takes 5 bits, register 5 bits (32 regs) and imm gets only 22 bits.
On 2-operand instructions **(opcode, src-reg, imm)** we get even less.

Thus **sparc32** had to add special instruction to their ISA setting high bits for a
given reg (**o0** in our case).

Things are even worse for **sparc64** and **ppc64** where instructions are
32-bit, but registers are 64-bit wide.

x86_64 easily puts a 64-bit value into a register:

.. code-block:: asm

    movabs $0x1234ABCD5678DCBA, %rax

while sparc64 does something completely awful:

.. code-block:: asm

    sethi   %hi(0x1234a800), %o0
    sethi   %hi(0x5678dc00), %g1
    or      %o0, 0x3CD, %o0
    or      %g1, 0xBA, %g1
    sllx    %o0, 32, %o0
    add     %o0, %g1, %o0

The above is important because sometimes
those constants are not known at compile (and assembly)
time, but known only at link time.

ghc's driver pipeline
=====================

What ghc basically does when compiler **.hs** file on **via-C** (aka **UNREG**) arch
is the following:

1. .hs -> ... -> .hc file (haskell-to-C pass)
2. .hc -> .s (haskell-to-asm pass)
3. .s -> .s (asm-to-asm mangling pass, no-op in **UNREG** mode)
4. .s -> .o (asm-to-object pass)

The bug was introduced into pipeline when **-dynamic** way was added to ghc
(to build dynamic haskell libraries or position-independent executables).

On many architectures libraries require position independent
code layout (so called **PIC**). It's controlled by **-fpic** / **-fPIC** set of **gcc** (and **ghc**) flags.

**ghc** passed **-fPIC** option to **1.** and **2.**,
but not to **4.**(!) where assembler needs to generate
either absolute or relative relocation types for the
following asm snippet:

.. code-block:: asm

        ; load GOT address into %l7
        sethi   %hi(_GLOBAL_OFFSET_TABLE_-8), %l7
        add     %l7, %lo(_GLOBAL_OFFSET_TABLE_-4), %l7

The bug was easily `fixed <https://git.haskell.org/ghc.git/commitdiff/a93ab43ab5f40cadbedea2f6342b93c245e91434>`_ when identified.
Now shared haskell libs work on **sparc**!

a bit more on sparc's relocations
=================================

Sometimes there is many ways to generate PIC code even for a single given arch.
On **sparc** for example there is at least:

- **-fpic** option (22-bit relocations)
- **-fPIC** (32-bit relocations)

If you are curious:

.. code-block:: c

    extern int g_i;
    int * f(void)
    {
        return &g_i;
    }

Generates the following assembly for **-fpic**:

.. code-block:: asm

    f:
        save    %sp, -96, %sp
        sethi   %hi(_GLOBAL_OFFSET_TABLE_-8), %l7
        add     %l7, %lo(_GLOBAL_OFFSET_TABLE_-4), %l7
        call    __sparc_get_pc_thunk.l7
         nop
        ld      [%l7+g_i], %g1
        mov     %g1, %i0
        restore
        jmp     %o7+8
         nop

and for **-fPIC**:

.. code-block:: asm

    f:
        save    %sp, -96, %sp
        sethi   %hi(_GLOBAL_OFFSET_TABLE_-8), %l7
        add     %l7, %lo(_GLOBAL_OFFSET_TABLE_-4), %l7
        call    __sparc_get_pc_thunk.l7
         nop
        sethi   %gdop_hix22(g_i), %g1
        xor     %g1, %gdop_lox10(g_i), %g1
        ld      [%l7 + %g1], %g1, %gdop(g_i)
        mov     %g1, %i0
        restore
        jmp     %o7+8
         nop

The difference here is what is used to access **GOT**:

.. code-block:: asm

        ; -fpic: immediate offset in 'ld'
        ld      [%l7+g_i], %g1
        ;
        ; -fPIC: loading full 32-bit offset into %l7 register
        sethi   %gdop_hix22(g_i), %g1
        xor     %g1, %gdop_lox10(g_i), %g1
        ld      [%l7 + %g1], %g1, %gdop(g_i)

ia64 and integer-gmp
====================

Having dealt with **sparc** I've decided to have a look at **ia64**
once again (i didn't touch it much after **ghc-7.4**).

**ghc** on **ia64** has many problems. One of them is known as **gprel**
`addressing overflow <https://gcc.gnu.org/ml/gcc/2011-01/msg00079.html>`_.
You just can't link static **ghc-7.6** binary on **ia64**.

Thus one of the ways to fix it is to get dynamic linking working (and a working ghci as a side effect).

Luckily, **sparc** fix was enough to unbreak it!

Another long-standing problem was non-working **integer-gmp** library
(any call to that library resulted in **SIGSEGV**). The workaround
was to use pure-haskell **integer-simple** library.

It broke around **ghc-7.0** release and was not touched since.

After some painful debugging (linking **libghc** takes 2 hours on **ia64**, bulding from scratch - 10 hours on 4-core box)
I've found a cause: **C** codegen generates **data**-like prototypes for **function**-like objects.

Consider the following snippet:

.. code-block:: C

    // a.c
    void f(void) {}
    // b.c
    extern void f(void);
    void * g (void)
    {
        return (void*)&f;
    }
    // c.c
    extern int f;
    void * g (void)
    {
        return (void*)&f;
    }

For most popular arches you would expect the same code to be generated
for **b.c** and **c.c** files. But it's not the case for **ia64**.
Function pointers there are not pointers to code, but pointers to
a structure called **function descriptor** (a structure of 2 "pointers": a pointer to code and a new **gp** value).

As usual once spotted the problem was easy to `fix <http://git.haskell.org/ghc.git/commitdiff/e18525fae273f4c1ad8d6cbe1dea4fc074cac721>`_.

You would not normally write code like **c.c**, but in this case it was a stupid bug.

I have an idea to clean C codegen to a state when **gcc**'s **LTO** will be able to build **ghc**
on amd64 and will be able to find such kinds of bugs at compile time.

For curious here is how assembly looks for **b.c** on **ia64**:

.. code-block:: asm

    g:
        .mmi
        nop 0
        addl r8 = @ltoff(@fptr(f#)), gp
        nop 0
        ;;
        .mib
        ld8 r8 = [r8]
        nop 0
        br.ret.sptk.many rp

and for **c.c**:

.. code-block:: asm

    g:
        .mmi
        nop 0
        addl r8 = @ltoffx(f#), r1
        nop 0
        ;;
        .mib
        ld8.mov r8 = [r8], f#
        nop 0
        br.ret.sptk.many rp

Here **f#** and @fptr(f#) are different objects
with different access rules. @fptr for example needs one more dereference
to be called/compared/whatever.

how I've spent an august
========================

What works better now in **ghc-HEAD**

- 64-bit **UNREG** arches make less mistakes in integer operations
- shared libraries (and **ghci**) now do work at least on **sparc** and **ia64** (i hope on **ppc** as well)
- **integer-gmp** now works on **ia64**

Have fun!
