---
title: dynamic linking ABI is hard
date: December 3, 2016
---

:PostID: 195
:Title: dynamic linking ABI is hard
:Keywords: C, ABI, arrays, pointers, glibc, linker, relocations
:Categories: notes


Today on **#gentoo-haskell** Ke shown an example of subtle
ABI breakage. nettle library exports as part of it's API
**NULL**-terminated array of functions **nettle_hashes**:

.. code-block:: c

    // in nettle-meta.h
    extern const struct nettle_hash * const nettle_hashes[];

and defines that array as

.. code-block:: c

    // nettle-meta-hashes.c
    const struct nettle_hash * const nettle_hashes[] = {
        &nettle_md2,
        &nettle_md4,
        &nettle_md5,
        &nettle_ripemd160,
        &nettle_sha1,
        &nettle_sha224,
        &nettle_sha256,
        &nettle_sha384,
        &nettle_sha512,
        NULL
    };

Quiz question!
==============

Will **ABI** change if we add or remove a few entries in array? (`like this patch <https://git.lysator.liu.se/nettle/nettle/commit/2a4acddcd8e004b0632b292f1a32d8ec86278a30>`_)

Would you expect existing binaries to start crashing after library upgrade on your system?

.. raw:: html

   <!--more-->


TL;DR: yes, things will break.

Tiny trigger
============

To understand how exactly things break let's dive into simpler
example of a library exporting only constant strings and nothing else.

Public library interface:

.. code-block:: c

    // l.h:
    extern const char s1[];
    extern const char s2[];

Full library implementation:

.. code-block:: c

    // l.c:
    #include "l.h"
    
    #ifdef V1
    const char s1[] = "v1 s1";
    const char s2[] = "v1 s2";
    #endif
    
    #ifdef V2
    const char s1[] = "v2 s1 <V2 addition>";
    const char s2[] = "v2 s2 <V2 addition>";
    #endif

And library user:

.. code-block:: c

    // exe.c:
    #include <stdio.h>
    #include "l.h"
    
    int main() {
        printf ("s1='%s'\n", s1);
        printf ("s2='%s'\n", s2);
        return 0;
    }

Here we just print array values from executable. Nothing fancy.

Now let's try to do the following sequence of actions:

1. build a library in **V1** mode (shorter strings)
2. build an executable against **V1** library
3. run executable (linked against **V1**) against **V1** library
4. build a library in **V2** mode (longer strings)
5. run executable (linked against **V1**) against **V2** library
6. build an executable against **V2**
7. run executable (linked against **V2**) against **V2** library

Doing **1-3** steps:

.. code-block::

    $ gcc -O2 -DV1 -shared -fPIC l.c -o libl.so
    $ gcc -O2 exe.c -o exe -L. -ll '-Wl,-rpath=$ORIGIN'
    $ echo 'Runnig exe/V1'
    Runnig exe/V1
    $ ./exe
    s1='v1 s1'
    s2='v1 s2'
    $ cp exe exe-v1

No surprises here. Let's update library to **V2** (steps **4-5**):

.. code-block::

    $ gcc -O2 -DV2 -shared -fPIC l.c -o libl.so
    $ echo 'Runnig exe/V2'
    Runnig exe/V2
    $ ./exe
    ./exe: Symbol `s2' has different size in shared object, consider re-linking
    ./exe: Symbol `s1' has different size in shared object, consider re-linking
    s1='v2 s1 '
    s2='v2 s2 v2 s1 '

Hah! Data corruption! **glibc**'s runtime dynamic linker even hints us
to relink an executable. Let's do that (steps **6-7**):

.. code-block::

    $ gcc -O2 exe.c -o exe -L. -ll '-Wl,-rpath=$ORIGIN'
    $ echo 'Runnig exe/V2 (relinked)'
    Runnig exe/V2 (relinked)
    $ ./exe
    s1='v2 s1 <V2 addition>'
    s2='v2 s2 <V2 addition>'
    $ cp exe exe-v2

Recovered.

The clues
=========

So how could executable change when linked against **V1** and **V2** versions?
The easiest way to see it is to dump all the ELF information we have:

.. code-block:: bash

    $ readelf -a exe-v1 > v1
    $ readelf -a exe-v2 > v2
    $ diff -u v1 v2

.. code-block:: diff

    --- v1  2016-12-03 14:39:09.475769368 +0000
    +++ v2  2016-12-03 14:39:11.510768031 +0000
    @@ -1,3 +1,3 @@
     Section Headers:
       [Nr] Name              Type             Address           Offset
            Size              EntSize          Flags  Link  Info  Align
    ...
       [24] .bss              NOBITS           0000000000601030  00001030
    -       0000000000000010  0000000000000000  WA       0     0     1
    +       0000000000000038  0000000000000000  WA       0     0     16
    ...
     Program Headers:
       Type           Offset             VirtAddr           PhysAddr
                      FileSiz            MemSiz              Flags  Align
    ...
       LOAD           0x0000000000000de8 0x0000000000600de8 0x0000000000600de8
    -                 0x0000000000000248 0x0000000000000258  RW     200000
    +                 0x0000000000000248 0x0000000000000280  RW     200000
    ...
     Relocation section '.rela.dyn' at offset 0x498 contains 4 entries:
       Offset          Info           Type           Sym. Value    Sym. Name + Addend
    ...
     000000601030  000900000005 R_X86_64_COPY     0000000000601030 s2 + 0
    -000000601036  000600000005 R_X86_64_COPY     0000000000601036 s1 + 0
    +000000601050  000600000005 R_X86_64_COPY     0000000000601050 s1 + 0
    ...
     Symbol table '.dynsym' contains 11 entries:
        Num:    Value          Size Type    Bind   Vis      Ndx Name
    ...
    -     6: 0000000000601036     6 OBJECT  GLOBAL DEFAULT   24 s1
    +     6: 0000000000601050    20 OBJECT  GLOBAL DEFAULT   24 s1
    ...
    -     9: 0000000000601030     6 OBJECT  GLOBAL DEFAULT   24 s2
    +     9: 0000000000601030    20 OBJECT  GLOBAL DEFAULT   24 s2
    ...
     Symbol table '.symtab' contains 60 entries:
        Num:    Value          Size Type    Bind   Vis      Ndx Name
    ...
    -    43: 0000000000601030     6 OBJECT  GLOBAL DEFAULT   24 s2
    +    43: 0000000000601030    20 OBJECT  GLOBAL DEFAULT   24 s2
    ...
    -    54: 0000000000601036     6 OBJECT  GLOBAL DEFAULT   24 s1
    +    54: 0000000000601050    20 OBJECT  GLOBAL DEFAULT   24 s1

We see here a lot of interesting facts:

- **s1** and **s2** symbols have known sizes
- the sizes change from 6 bytes ("v1 s1\\0") to 20 bytes ("v2 s1 <V2 addition>\\0")
- both **s1** and **s2** have mysterious **R_X86_64_COPY** relocation type
- **.bss** section size increased for +40 bytes
- LOAD read/write segment increased for +40 bytes

It means array contents is copied from library **.data** section to an executable
**.bss** section at each execution startup time.

Why does it behave like that?
=============================

But why copy? Arrays might be huge in size and copying them would take a while.
Why not just map the library and use it's symbols?

For that we need to understand what drives the process of binary generation.

All starts from **exe.c** file being converted to the assembly form.
Let's look at it:

.. code-block:: asm

    ; gcc -O2 exe.c -S -o exe.S

        .file   "exe.c"
        .section        .rodata.str1.1,"aMS",@progbits,1
    .LC0:
        .string "s1='%s'\n"
    .LC1:
        .string "s2='%s'\n"
        .section        .text.startup,"ax",@progbits
        .p2align 4,,15
        .globl  main
        .type   main, @function
    main:
    .LFB23:
        .cfi_startproc
        subq    $8, %rsp
        .cfi_def_cfa_offset 16
        movl    $s1, %edx
        movl    $.LC0, %esi
        movl    $1, %edi
        xorl    %eax, %eax
        call    __printf_chk
        movl    $s2, %edx
        movl    $.LC1, %esi
        movl    $1, %edi
        xorl    %eax, %eax
        call    __printf_chk
        xorl    %eax, %eax
        addq    $8, %rsp
        .cfi_def_cfa_offset 8
        ret
        .cfi_endproc
    .LFE23:
        .size   main, .-main
        .ident  "GCC: (Gentoo 6.2.0-r1 p1.1) 6.2.0"
        .section        .note.GNU-stack,"",@progbits

The relevant piece of code here is how **s1** gets propagated to **printf** call:

.. code-block:: asm

    ;
        .file   "exe.c"
        .section        .rodata.str1.1,"aMS",@progbits,1
    .LC0:
        .string "s1='%s'\n"
    ...
    main:
    ...
        movl    $s1, %edx
        movl    $.LC0, %esi
        movl    $1, %edi
        xorl    %eax, %eax
        call    __printf_chk

**$s1** is an absolute address to **s1** symbol. It is not known at **exe**
link time as it's storage is in external library. There is no indirection used.

One way of adjusting this address is to use a relocation in code segment (also known at **TEXTREL**).
But such relocations are unwelcome in linux systems. They have a few disadvantages:

- [insecurity] **.text** sections that contain **TEXTREL**s need to be mapped with **RWX** permissions.
- [inefficiency] Fixing up these relocation has to be done before before program takes control.
  Even if code will never be executed.
- [inefficiency] Each fixed relocation unshares code page where relocation was fixed up.

**s1** and **s2** object size is known at link time:
**ld** accepts both **exe.c** and **libl.so** files to resolve all used symbols in final **exe**.
Thus linker decides to provide storage for such data in **exe**'s own writable **.bss** section
and generates special **COPY** relocations as if external data would be local to **exe**.

When we update **libl.so** with new **s1** object size **exe** still contains
**COPY** relocation of symbol **s1** of the old size. This leads to partial
symbol copying at **exe** startup.

In case of **nettle** that means **NULL**-terminated array will be copied only partially
(missing 4 last elements including **NULL**) which causes occasional **SIGSEGV**s.

A fun workaround
================

This absolute relocation problem is well known when writing shared libraries.
Compiler has a special position independent mode (**-fPIC**) that generates
non-absolute access to each symbol in the library.

We can workaround the problem by building **exe.c** with **-fPIC**:

.. code-block:: bash

    $ gcc -O2 -DV1 -shared -fPIC l.c -o libl.so
    $ gcc -O2 -fPIC exe.c -o exe -L. -ll '-Wl,-rpath=$ORIGIN'
    $ echo 'Runnig exe/V1'
    Runnig exe/V1
    $ ./exe
    s1='v1 s1'
    s2='v1 s2'
    $ gcc -O2 -DV2 -shared -fPIC l.c -o libl.so
    $ echo 'Runnig exe/V2'
    Runnig exe/V2
    $ ./exe
    s1='v2 s1 <V2 addition>'
    s2='v2 s2 <V2 addition>'

It just works. Let's look at the changes in generated code for **exe.c**:

.. code-block:: asm

    ; gcc -fPIC -O2 exe.c -S -o exe-fPIC.S
        .file   "exe.c"
        .section        .rodata.str1.1,"aMS",@progbits,1
    .LC0:
        .string "s1='%s'\n"
    .LC1:
        .string "s2='%s'\n"
        .section        .text.startup,"ax",@progbits
        .p2align 4,,15
        .globl  main
        .type   main, @function
    main:
    .LFB23:
        .cfi_startproc
        subq    $8, %rsp
        .cfi_def_cfa_offset 16
        movq    s1@GOTPCREL(%rip), %rdx
        leaq    .LC0(%rip), %rsi
        movl    $1, %edi
        xorl    %eax, %eax
        call    __printf_chk@PLT
        movq    s2@GOTPCREL(%rip), %rdx
        leaq    .LC1(%rip), %rsi
        movl    $1, %edi
        xorl    %eax, %eax
        call    __printf_chk@PLT
        xorl    %eax, %eax
        addq    $8, %rsp
        .cfi_def_cfa_offset 8
        ret
        .cfi_endproc
    .LFE23:
        .size   main, .-main
        .ident  "GCC: (Gentoo 6.2.0-r1 p1.1) 6.2.0"
        .section        .note.GNU-stack,"",@progbits

Or in a diff form:

.. code-block:: diff

    --- exe.S       2016-12-03 17:51:28.229898505 +0000
    +++ exe-fPIC.S  2016-12-03 18:09:38.341060805 +0000
    @@ -16,2 +16,2 @@
    -       movl    $s1, %edx
    -       movl    $.LC0, %esi
    +       movq    s1@GOTPCREL(%rip), %rdx
    +       leaq    .LC0(%rip), %rsi
    @@ -20,3 +20,3 @@
    -       call    __printf_chk
    -       movl    $s2, %edx
    -       movl    $.LC1, %esi
    +       call    __printf_chk@PLT
    +       movq    s2@GOTPCREL(%rip), %rdx
    +       leaq    .LC1(%rip), %rsi
    @@ -25 +25 @@
    -       call    __printf_chk
    +       call    __printf_chk@PLT

Access to **s1** is now done via separate global offset table (aka **.got**).
This way we get another layer of indirection (memory dereference) and get
our **s1** contents without copies.

A few takeaways
===============

- Be careful when exporting any objects from libraries (arrays, structs, integral constants)
- Exporting a pointer (const char *) instead of an array (const char []) would be not so devastating
- Dynamic linking is hard :)
- To learn more it's worth reading Ulrich Drepper's `DSO howto <https://software.intel.com/sites/default/files/m/a/1/e/dsohowto.pdf>`_
- Another good book is `Linkers and Loaders <https://wh0rd.org/books/linkers-and-loaders/>`_ by Jonh R. Levine.

Have fun!
