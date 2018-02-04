---
title: "Linker script weird tricks or EFI on ia64"
date: February 2, 2018
---

:PostID: 207
:Title: "Linker script weird tricks or EFI on ia64"
:Keywords: gcc, ia64, EFI, gentoo, ELF, **PE32**, **PE32+**, linker scripts
:Categories: notes

One of **gentoo** `bugs <https://bugs.gentoo.org/579278>`_
was about **gentoo** install CD not being able to boot **Itanium** boxes.

The bug
=======

The symptom is **EFI** loader error:

.. code-block::

    Loading.: Gentoo Linux
    ImageAddress: pointer is outside of image
    ImageAddress: pointer is outside of image
    LoadPe: Section 1 was not loaded
    Load of Gentoo Linux failed: Not Found
    Paused - press any key to continue

which looks like a simple error if you have the hardware and
source code of the loader that prints the error.

I had neither.

My plan was to extend **ski** emulator to be able to handle
early **EFI** stuff. I started reading `docs on early itanium boot life <https://www.thailand.intel.com/content/dam/www/public/us/en/documents/specification-updates/itanium-system-abstraction-layer-specification.pdf>`_:
on **SAL**s (system abstraction layers), **PAL**s (platform abstration layers), monarch processors, rendevouz protocols
to handle **MCA**s (machine check aborts) and expected system state when hand off to **EFI**
happens.

But **SAL** spec touches **EFI** only at **SAL**->**EFI** interface
boundary. I dreaded to open **EFI** spec before finishing reading
**SAL** paper.

How ia64 actually boots
=======================

But recently I got access to real management processor (sometimes also called **MP**,
**BMC** (baseboard managenet controller), **iLO** (integrated lights-out)) on **rx3600** machine and I gave up on extending **ski** for a while.

The very first thing I attempted is to get to serial console of operating system
from **iLO**. It happened to be not as straightforward as passing **console=ttyS0** to kernel.
I ended up learning a bit about **EFI** shell, **ia64**-specific `serial port numbering <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/Documentation/ia64/serial.txt>`_
and **elilo**.

Eventually I was able to boot arbitrary kernel directly from **EFI** shell and get to kernel's
console! I needed that as a fallback in case I screw default boot loader, boot loader config
or default boot options as I don't have physical access to **ia64** machine. 

Here is an example of booting from **CD** and controlling it from **iLO** virtual serial console:

.. code-block::

    fs0:\efi\boot\bootia64.efi -i gentoo.igz gentoo initrd=gentoo.igz root=/dev/ram0 init=/linuxrc dokeymap looptype=squashfs loop=/image.squashfs cdroot console=ttyS1,115200n8

here **fs0** is a **FAT** file system (on CD, could be on HDD) with **EFI** applications.
**bootia64.efi** is a **sys-boot/elilo** application in disguse. **console=ttyS1,115200n8**
matches **EFI** setup of second serial console.

After successful boot from **CD** of known good old kernel (from 2009) I was not
afraid of messing with main **HDD** install or even upgrading to brand new kernels,
I even managed to squash another **ia64**-specific kernel and GCC bug! I'll try to
write another post about that.

This poking also helped me to understand boot process in more detail. **ia64**
boots in the following way:

1. **SAL**: when machine powers on it executes **SAL** (and **PAL**) code to initialize
   **S**ystem and **P**rocessors. This code is not easily updateable
   and usually stays the same across OS updates.
2. **EFI**: Then **SAL** hands control to **EFI** (also not easily updateable) where
   I can interactively pick boot device or even get **EFI** shell to run
   **EFI** applications (programs for **EFI** environment).
3. **Bootloader (EFI application)**:  Normally **EFI** is set up to start boot loader
   after a few seconds. It's up to the user (finally!) to provide a bootloader implementation.
   `Gentoo ia64 handbook <https://wiki.gentoo.org/wiki/Handbook:IA64>`_ suggests
   **sys-boot/elilo** package.
4. **OS kernel**: bootloader finds OS kernel, loads it and hands control off to kernel.

Searching for the clues
=======================

Our problem happened at stage **3** where **EFI** failed to load **elilo** application.

Gentoo builds **.iso** images automatically and continuously. Here you can find `ia64 isos <http://distfiles.gentoo.org/releases/ia64/autobuilds/>`_.
Older disks are available on actual builder machines.

As **elilo** used to work on images from 2008 and even **2016** (!) I
passed through a few autobuilt **ISO** images and collected a few working
and non-working samples and started comparing them.

I extracted **elilo.efi** files from 3 disks:

- **elilo-2014-works.efi**: good, picked from machine I have access to
- **elilo-2016-works.efi**: good, picked from **2016** install CD
- **elilo-2018-does-not-work.efi**: bad, freshly built on modern software

Normally I would start from running **readelf -a** on each executable
and diff for suspicious changes. The files however are not **ELF**s:

.. code-block::

    $ file *.efi
    elilo-2014-works.efi:         PE32+ executable (EFI application) Intel Itanium (stripped to external PDB), for MS Windows
    elilo-2016-works.efi:         PE32 executable (EFI application) Intel Itanium (stripped to external PDB), for MS Windows
    elilo-2018-does-not-work.efi: PE32+ executable (EFI application) Intel Itanium (stripped to external PDB), for MS Windows

One of them is not even **PE32+** but still happens to boot.

Binutils has more generic **readelf -a** equivalent: it's **objdump -x**.
Comparing two good files:

.. code-block:: diff

    $ objdump -x elilo-2014-works.efi > 2014.good
    $ objdump -x elilo-2016-works.efi > 2016.good
    --- 2014.good   2018-01-27 23:34:10.118197637 +0000
    +++ 2016.good   2018-01-27 23:34:23.590191456 +0000
    @@ -2,2 +2,2 @@
    -elilo-2014-works.efi:     file format pei-ia64
    -elilo-2014-works.efi
    +elilo-2016-works.efi:     file format pei-ia64
    +elilo-2016-works.efi
    @@ -6 +6 @@
    -start address 0x0000000000043a20
    +start address 0x000000000003a6a0
    @@ -14,2 +14,2 @@
    -Time/Date              Tue Jun 24 22:05:17 2014
    -Magic                  020b    (PE32+)
    +Time/Date              Mon Jan  9 21:18:46 2006
    +Magic                  010b    (PE32)
    @@ -17,3 +17,3 @@
    -MinorLinkerVersion     23
    -SizeOfCode             00036e00
    -SizeOfInitializedData  00020800
    +MinorLinkerVersion     56
    +SizeOfCode             0002e000
    +SizeOfInitializedData  00028a00
    @@ -21 +21 @@
    -AddressOfEntryPoint    0000000000043a20
    +AddressOfEntryPoint    000000000003a6a0
    @@ -34,2 +34,2 @@
    -SizeOfHeaders          000002c0
    -CheckSum               00067705
    +SizeOfHeaders          00000400
    +CheckSum               00069054

There is a lot of odd going on here: the file on 2016 live CD is actually from 2006
and it's actually older than file from 2014. It has different PE type and as a result
different file alignment. Thus I discarded **elilo-2016-works.efi** as too old.

Comparing bad/good:

.. code-block:: diff

    $ objdump -x elilo-2014-works.efi > 2014.good
    $ objdump -x elilo-2018-does-not-work.efi > 2018.bad
    $ diff -U0 2014.good 2018.bad
    --- 2014.good   2018-01-27 23:42:58.355002114 +0000
    +++ 2018.bad    2018-01-27 23:43:02.042000991 +0000
    @@ -2,2 +2,2 @@
    -elilo-2014-works.efi:     file format pei-ia64
    -elilo-2014-works.efi
    +elilo-2018-does-not-work.efi:     file format pei-ia64
    +elilo-2018-does-not-work.efi
    @@ -6 +6 @@
    -start address 0x0000000000043a20
    +start address 0x0000000000046d80
    @@ -14 +14 @@
    -Time/Date              Tue Jun 24 22:05:17 2014
    +Time/Date              Thu Jan  1 01:00:00 1970
    @@ -17,3 +17,3 @@
    -MinorLinkerVersion     23
    -SizeOfCode             00036e00
    -SizeOfInitializedData  00020800
    +MinorLinkerVersion     29
    +SizeOfCode             0003a200
    +SizeOfInitializedData  00020e00
    @@ -21,2 +21,2 @@
    -AddressOfEntryPoint    0000000000043a20
    -BaseOfCode             0000000000001000
    +AddressOfEntryPoint    0000000000046d80
    +BaseOfCode             0000000000000000
    @@ -33 +33 @@
    -SizeOfImage            0005c000
    +SizeOfImage            0005f000
    @@ -35 +35 @@
    -CheckSum               00067705
    +CheckSum               0005f6a3
    @@ -51 +51 @@
    -Entry 5 0000000000058000 0000000c Base Relocation Directory [.reloc]
    +Entry 5 000000000005b000 0000000c Base Relocation Directory [.reloc]
    @@ -66,3 +66,3 @@
    -Virtual Address: 00043a20 Chunk size 12 (0xc) Number of fixups 2
    -       reloc    0 offset    0 [43a20] DIR64
    -       reloc    1 offset    8 [43a28] DIR64
    +Virtual Address: 00046d80 Chunk size 12 (0xc) Number of fixups 2
    +       reloc    0 offset    0 [46d80] DIR64
    +       reloc    1 offset    8 [46d88] DIR64
    ...
    @@ -87,571 +87,585 @@
    -[  0](sec  3)(fl 0x00)(ty   0)(scl   3) (nx 0) 0x0000000000006a04 edd30_guid
    -[  1](sec  2)(fl 0x00)(ty   0)(scl   3) (nx 0) 0x00000000000001f8 done_fixups
    ...
    -[570](sec  2)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000000110 Optind
    +[  0](sec  3)(fl 0x00)(ty   0)(scl   3) (nx 0) 0x0000000000006ccc edd30_guid
    +[  1](sec  2)(fl 0x00)(ty   0)(scl   3) (nx 0) 0x0000000000000208 done_fixups
    ...
    +[584](sec  2)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000000110 Optind

This looks better. A few notable things:

- **Time/Date** field is not initialized for the bad file: sane date vs. all zeros
- **BaseOfCode** looks uninitialized: **0x1000** vs. **0x0000**
- **MinorLinkerVersion** says good file was built with **binutils-2.23**, bad file
  was built with **binutils-2.29**
- File has only 2 relocations: **Number of fixups 2**. We could check them manually!
- And a lot of symbols: **570** vs. **580**

I tried to build **elilo** with **binutils-2.25**: it produced the same
binary as **elilo-2018-does-not-work.efi**.

My only clue was that **BaseOfCode** is zero. It felt like something used to
reside in the first page before code section and now it does not anymore.
What could it be?

GNU binutils linker scripts
===========================

Time to look at how decision is made what to put into the first page at link time!

The build process of **elilo.efi** is truly unusual. Let's run **emerge -1 sys-boot/elilo**
and check what commands are being executed to yield it:

.. code-block:: bash

    # emerge -1 sys-boot/elilo
    ...
    make -j1 ... ARCH=ia64
    ...
    ia64-unknown-linux-gnu-gcc \
        -I. -I. -I/usr/include/efi -I/usr/include/efi/ia64 -I/usr/include/efi/protocol -I./efi110  \
        -O2  -fno-stack-protector -fno-strict-aliasing -fpic -fshort-wchar \
        -Wall \
        -DENABLE_MACHINE_SPECIFIC_NETCONFIG -DCONFIG_LOCALFS -DCONFIG_NETFS -DCONFIG_CHOOSER_SIMPLE \
        -DCONFIG_CHOOSER_TEXTMENU \
        -frename-registers -mfixed-range=f32-f127 \
        -DCONFIG_ia64  \
        -c glue_netfs.c -o glue_netfs.o
    ia64-unknown-linux-gnu-ld \
        -nostdlib -znocombreloc \
        -T /usr/lib/elf_ia64_efi.lds \
        -shared -Bsymbolic \
        -L/usr/lib -L/usr/lib \
        /usr/lib/crt0-efi-ia64.o elilo.o getopt.o strops.o loader.o fileops.o util.o vars.o alloc.o \
        chooser.o config.o initrd.o alternate.o bootparams.o gunzip.o console.o fs/fs.o choosers/choosers.o \
        devschemes/devschemes.o ia64/sysdeps.o glue_localfs.o glue_netfs.o \
        \
        -o elilo.so \
        \
        -lefi -lgnuefi \
        /usr/lib/gcc/ia64-unknown-linux-gnu/7.2.0/libgcc.a
    ia64-unknown-linux-gnu-ld: warning: creating a DT_TEXTREL in a shared object.
    objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym -j .rel \
            -j .rela -j .reloc --target=efi-app-ia64 elilo.so elilo.efi
    >>> Source compiled.

Here we see the following steps:

- With exception of **-frename-registers -mfixed-range=f32-f127** the process of building **EFI** application
  is almost like building normal shared library.
- Non-standard **/usr/lib/elf_ia64_efi.lds** linker script is used.
- **objcopy --target=efi-app-ia64** is used to create **PE32+** file out of **ELF64**. Very dark magic.

Luckily **gnu-efi** and **elilo** have `superb documentation <https://sourceforge.net/p/gnu-efi/code/ci/master/tree/README.gnuefi>`_
(10 pages). All the obscure corners are explained in every detail. **Part 2: Inner Workings** is the short description
of how every dynamic linker works with a light touch of **ELF** -> **PE32+** conversion. I wish I have
seen this doc years ago :)

Let's looks at the **elf_ia64_efi.lds** linker script to get full understanding
of where every byte comes from when **elilo.so** is being linked
(`sourceforge viewer <https://sourceforge.net/p/gnu-efi/code/ci/master/tree/gnuefi/elf_ia64_efi.lds>`_):

.. code-block:: c++

    OUTPUT_FORMAT("elf64-ia64-little")
    OUTPUT_ARCH(ia64)
    ENTRY(_start_plabel)
    SECTIONS
    {
      . = 0;
      ImageBase = .;
      .hash : { *(.hash) }/* this MUST come first! */
      . = ALIGN(4096);
      .text :
      {
       _text = .;
       *(.text)
       *(.text.*)
       *(.gnu.linkonce.t.*)
       . = ALIGN(16);
      }
      _etext = .;
      _text_size = . - _text;
      . = ALIGN(4096);
      __gp = ALIGN (8) + 0x200000;
      .sdata :
      {
       _data = .;
       *(.got.plt)
       *(.got)
       *(.srodata)
       *(.sdata)
       *(.sbss)
       *(.scommon)
      }
      . = ALIGN(4096);
      .data :
      {
       *(.rodata*)
       *(.ctors)
       *(.data*)
       *(.gnu.linkonce.d*)
       *(.plabel)/* data whose relocs we want to ignore */
       /* the EFI loader doesn't seem to like a .bss section, so we stick
          it all into .data: */
       *(.dynbss)
       *(.bss)
       *(COMMON)
      }
      .note.gnu.build-id : { *(.note.gnu.build-id) }
    
      . = ALIGN(4096);
      .dynamic  : { *(.dynamic) }
      . = ALIGN(4096);
      .rela :
      {
        *(.rela.text)
        *(.rela.data*)
        *(.rela.sdata)
        *(.rela.got)
        *(.rela.gnu.linkonce.d*)
        *(.rela.stab)
        *(.rela.ctors)
      }
      _edata = .;
      _data_size = . - _etext;
      . = ALIGN(4096);
      .reloc :/* This is the PECOFF .reloc section! */
      {
        *(.reloc)
      }
      . = ALIGN(4096);
      .dynsym   : { *(.dynsym) }
      . = ALIGN(4096);
      .dynstr   : { *(.dynstr) }
      /DISCARD/ :
      {
        *(.rela.plabel)
        *(.rela.reloc)
        *(.IA_64.unwind*)
        *(.IA64.unwind*)
      }
    }

Even though the file is very big it's easy to read. Linker script defines symbols (as **symbol = expression**,
**.** (dot) means "current address") and output section (as **.output-section : { expressions }**) in terms of
input sections.

Here is what linker script tries to achieve:

- **. = 0;** sets load address to **0**. It does not really matter. At load time module will be relocated anyway.

- **ImageBase = .** means that new symbol **ImageBase** is created and pointed at current address:
  the very start of the whole output as nothing was collected yet.

- **.hash : { \*(.hash) }** means to collect all **DT_HASH** input symbol sections into output **DT_HASH** section.
  **DT_HASH** defines mandatory section of fast symbol lookup. Linker uses that section to resolve symbol name to
  symbol type and offset in the file.

- **. = ALIGN(4096)** forces linker to pad output (with zeros) to **4K** boundary (**EFI** defines page size as **4K**).

- Only then goes **.text : ...** section. **BaseOfCode** is the very first byte of **.text** section.

- Other things go below.

GNU hash mystery
================

Later **objcopy** is used to produce final (**PE32+**) binary by copying whitelisted sections passed via **-j**:

.. code-block::

    objcopy -j .text -j .sdata -j .data -j .dynamic -j .dynsym -j .rel \
            -j .rela -j .reloc --target=efi-app-ia64 elilo.so elilo.efi

While **objcopy** does not copy **.hash** section into final binary it's mere presence in **elilo.so** file
changes **.text** offset as linker already allocated space for it in **elilo.so** and resolved other reloactions
taking offset into account.

So why offset disappeared? Simple! Because **gentoo** does not generate **.hash** sections `since 2014 <https://bugs.gentoo.org/575300#c20>`_!
**.gnu.hash** (**DT_GNU_HASH**) is being used instead. **DT_GNU_HASH** was `added to binutils/glibc <https://sourceware.org/ml/binutils/2006-06/msg00418.html>`_
around 2006 as an optional mechanism to speed up dynamic linking and dynamic loading.

But linker script does not deal with **.gnu.hash** sections!

It's easy to mimic handling of both section types:

.. code-block:: diff

    --- a/gnuefi/elf_ia64_efi.lds
    +++ b/gnuefi/elf_ia64_efi.lds
    @@ -7,2 +7,3 @@ SECTIONS
       ImageBase = .;
       .hash : { *(.hash) } /* this MUST come first! */
    +  .gnu.hash : { *(.gnu.hash) }

This fix alone was enough to restore **elilo.efi**! A few other architectures
did not handle it either. See full `upstream fix <https://sourceforge.net/p/gnu-efi/code/ci/2cc0b085fb82e80d43cc08c8376dff9f9532a72d/>`_.

Breakage mechanics
==================

But why does it matter? What does it mean to drop **.hash** section completely?
**PE** format does not have a **.hash** equivalent.

Let's inspect what actually changes in **elilo.so** file before and after the patch:

.. code-block:: diff

    $ objdump -x elilo.efi.no.gnu.hash > elilo.efi.no.gnu.hash.od
    $ objdump -x elilo.efi.gnu.hash > elilo.efi.gnu.hash.od
    --- elilo.efi.no.gnu.hash.od    2018-01-29 23:05:25.776000000 +0000
    +++ elilo.efi.gnu.hash.od       2018-01-29 23:05:31.700000000 +0000
    @@ -2,2 +2,2 @@
    -elilo.efi.no.gnu.hash:     file format pei-ia64
    -elilo.efi.no.gnu.hash
    +elilo.efi.gnu.hash:     file format pei-ia64
    +elilo.efi.gnu.hash
    @@ -6 +6 @@
    -start address 0x0000000000046d80
    +start address 0x0000000000047d80
    @@ -21,2 +21,2 @@
    -AddressOfEntryPoint    0000000000046d80
    -BaseOfCode             0000000000000000
    +AddressOfEntryPoint    0000000000047d80
    +BaseOfCode             0000000000001000
    @@ -33 +33 @@
    -SizeOfImage            0005f000
    +SizeOfImage            00060000
    ...
    @@ -633,39 +633,38 @@
    -[546](sec  1)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000000000 ImageBase
    -[547](sec  3)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000007560 Udp4ServiceBindingProtocol
    ...
    -[584](sec  2)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000000110 Optind
    +[546](sec  3)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000007560 Udp4ServiceBindingProtocol
    ...
    +[583](sec  2)(fl 0x00)(ty   0)(scl   2) (nx 0) 0x0000000000000110 Optind
    ...


Here internal **ImageBase** symbol became external symbol! Which means **EFI**
would have to resolve **ImageBase** at application startup. That's why we have
seen loader error (as opposed to **elilo.efi** runtime errors or kernel boot
errors).

**ELF** spec says shared objects and dynamic executables are required to have
**.hash** (or **.gnu.hash**) section to be valid executable but linker script
did not maintain this requirement and all hell broke loose.

Perhaps linker could be tweaked to report warnings when symbol table is missing from output file.
But as you can see linker scripts are much more powerful than just implementing fixed output format.

Fun details
===========

**gnu-efi** has a lot of other jewels!

For example, entry point has to point to **ia64** function
descriptor (**FDESCR**). **FDESCR** is a pair of pointers: pointer
to code section (actual entry point) and value of **gp** register
(global pointer, base pointer used by **PIC** code).

These two pointers are both absolute addresses. But **EFI** application
needs to be relocatable (loadable at different addresses).

Entry point **FDESCR** needs to be relocated by **EFI** loader.

How would you inject **ia64** relocation to two 64-bit pointers in
**PE32+** format? **gnu-efi** does a very crazy thing (even more crazy
than relying on **objcopy** to Just Work)): it injects
**PE32+** relocation directly into **ia64** **ELF** code! That's how
it does the trick (the snippet below is the very tail of `crt0-efi-ia64.S file <https://sourceforge.net/p/gnu-efi/code/ci/master/tree/gnuefi/crt0-efi-ia64.S>`_):

.. code-block:: asm

    // PE32+ wants a PLABEL, not the code address of the entry point:
    .align 16
    .global _start_plabel
    .section .plabel, "a"
    _start_plabel:
        data8    _start
        data8    __gp
    
    // hand-craft a .reloc section for the plabel:
    
    #define IMAGE_REL_BASED_DIR64 10
    
    .section .reloc, "a"
        data4    _start_plabel                    // Page RVA
        data4    12                               // Block Size (2*4+2*2)
        data2    (IMAGE_REL_BASED_DIR64<<12) +  0 // reloc for plabel's entry point
        data2    (IMAGE_REL_BASED_DIR64<<12) +  8 // reloc for plabel's global pointer

This code generates two sections:

- **.plabel** with yet unrelocated **_start** and **_gp** pointers (crafts **FDESCR** itself)
- **.reloc** with synthesised raw bytes that look like **PE32+** relocation. It's format is
  slightly more complicated and is defined by **PE32+** spec:
  - [4 bytes] 32-bit offset to some blob where relocations are to be applied, in our case it's **_start_plabel**
  - [4 bytes] full size of this relocation entry
  - [2 bytes * N relocations] list of tuples: (4 bits for relocation type, 12-bit offset to data to relocate)

Here we have two relocations that add **ImageBase**: to **_start** and to **_gp**.
And it's precisely these two relocations that **EFI** loader reported as invalid:

.. code-block::

    ImageAddress: pointer is outside of image
    ImageAddress: pointer is outside of image
    LoadPe: Section 1 was not loaded

Before **.gnu.hash** fix **ImageBase** (**ImageAddress** in **EFI** terminology) was indeed pointing somewhere else.

How about searching internets for source of this **EFI** loader error? **tianocode** has
`one hit <https://github.com/tianocore/edk2/blob/master/DuetPkg/EfiLdr/PeLoader.c#L324>`_
in commented out code:

.. code-block:: c

    //...
    Base = EfiLdrPeCoffImageAddress (Image, (UINTN)Section->VirtualAddress);
    End = EfiLdrPeCoffImageAddress (Image, (UINTN)(Section->VirtualAddress + Section->Misc.VirtualSize));
    
    if (EFI_ERROR(Status) || !Base || !End) {
    //      DEBUG((D_LOAD|D_ERROR, "LoadPe: Section %d was not loaded\n", Index));
        PrintHeader ('L');
          return EFI_LOAD_ERROR;
    }
    //...

Not very useful but still fun :)

Parting words
=============

Itanium was the first system **EFI** was targeted at and was later morphed into **UEFI**.
Surprisingly I managed to ignore **(U)EFI**-based boot on modern machines and this bug
was my first experience to deal with it. And it was not too bad! :)

I found out a few things along the way:

- **ia64** **EFI** is a rich interface to **OS** and **iLO** to control the machine remotely
- Linker scripts can do a lot more than I realized:
  - merge sections
  - discard sections
  - inject symbols
  - handle alignments
  - rename sections

- With certain care **objcopy** can convert binaries across different object file formats.
  In this case **ELF64** to **PE32+**
- Assembler allows you to inject absolutely arbitrary sections into object files. Just make
  sure you handle those in your linker script.
- Modern GNU toolchain still can breathe life into decade old hardware to teach us a trick or two.
- **objdump -x** is a cool equivalent of **readelf**.

Have fun!
