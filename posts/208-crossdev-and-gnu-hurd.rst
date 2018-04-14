---
title: "crossdev and GNU Hurd"
date: April 14, 2018
---

:PostID: 208
:Title: "crossdev and GNU Hurd"
:Keywords: gcc, gentoo, crossdev, hurd, ghc, debian
:Categories: notes

Tl;DR: **crossdev** is a tool to generate a cross-compiler for you in **gentoo**
and with some hacks (see below) you can even cross-compile to **hurd**!

**FOSDEM** 2018 conference happened recently and a lot of cool talks
tool place there. The full list counts `689 events <https://fosdem.org/2018/schedule/events/>`_!

`Hurd's PCI arbiter <https://fosdem.org/2018/schedule/event/microkernel_hurd_pci_arbiter/>`_
was a nice one. I never actually tried **hurd** before and thought to give it a try in a VM.

Debian already provides full **hurd** installer (`installation manual <https://www.debian.org/ports/hurd/hurd-install>`_)
and I picked it. Hurd works surprisingly well for a such an understaffed project!
Installation process is very simple: it's a typical **debian** CD which asks you
for a few details about final system (same as for **linux**) and you get your OS booted.

**Hurd** has a ton of **debian** software already built and working
(like 80% of the whole repo). Even **GHC** is ported there. While at
it I grabbed all the tiny **GHC** patches related to **hurd** from
Debian and pushed them upstream:

- `PIC support <http://git.haskell.org/ghc.git/commitdiff/0693b0b0500236a3dec933350a13f1b0e8c1cb54>`_
- `allow canonical hurd tuple <http://git.haskell.org/ghc.git/commitdiff/1522cf05c9c3e9afd3ef9c7f3f419460c41488d9>`_

Now plain **./configure && make && make test** just works.

Hurd supports only 32-bit x86 CPUs and does not support SMP (only one CPU is available).
That makes building heaviweight stuff (like **GHC**) in a virtual machine a slow process.

To speed things up a bit I decided to build a cross-compiler from
**gentoo linux** to **hurd** with the help of **crossdev**. What
does it take to support bootstrap like that? Let's see!

To get the idea of how to cross-compile to another OS let's check
how typical **linux** to **linux** case looks like.

Normally aiming **gcc** at another **linux**-**glibc** target takes the following steps:

.. code-block::

   - install cross-binutils
   - install system headers (kernel headers and glibc headers):
   - install minimal gcc without glibc support (not able to link final executables yet)
   - install complete glibc (gcc will need crt.o files)
   - install full gcc (able to link final binaries for C and C++)

In **gentoo** **crossdev** does all the above automatically by running
**emerge** a few times for you. I wrote a more up-to-date `crossdev
README <https://gitweb.gentoo.org/proj/crossdev.git/plain/README>`_ to
describe a few details of what is happening when you run **crossdev -t <target>**.

**hurd-glibc** is not fundamentally different from **linux-glibc** case.
Only a few packages need to change their names, namely:

.. code-block::

   - install cross-binutils
   - install gnumach-headers (kernel headers part 1)
   - [NEW] install cross-mig tool (Mach Interface Generator, a flavour of IDL compiler)
   - install hurd-headers and glibc-headers (kernel headers part 2 and libc headers)
   - install minimal gcc without libc support (not able to link final executables yet)
   - install complete libc (gcc will need crt.o files)
   - install full gcc (able to link final binaries for C and C++)

The only change from **linux** is the **cross-mig** tool. I've collected ebuilds needed
in `gentoo-hurd overlay <https://github.com/trofi/gentoo-hurd>`_.

Here is how one gets hurd **cross-compiler** with today's **crossdev-99999999**:

.. code-block:: bash

    git clone https://github.com/trofi/gentoo-hurd.git
    
    HURD_OVERLAY=$(pwd)/gentoo-hurd
    CROSS_OVERLAY=$(portageq get_repo_path / crossdev)
    TARGET_TUPLE=i586-pc-gnu
    
    # this will fail around glibc, it's ok we'll take on manually from there
    crossdev --l 9999 -t crossdev -t ${TARGET_TUPLE}
    
    ln -s "${HURD_OVERLAY}"/sys-kernel/gnumach ${CROSS_OVERLAY}/cross-${TARGET_TUPLE}/
    ln -s "${HURD_OVERLAY}"/dev-util/mig ${CROSS_OVERLAY}/cross-${TARGET_TUPLE}/
    ln -s "${HURD_OVERLAY}"/sys-kernel/hurd ${CROSS_OVERLAY}/cross-${TARGET_TUPLE}/
    
    emerge -C cross-${TARGET_TUPLE}/linux-headers
    
    ACCEPT_KEYWORDS='**' USE=headers-only  emerge -v1 cross-${TARGET_TUPLE}/gnumach
    ACCEPT_KEYWORDS='**' USE=headers-only  emerge -v1 cross-${TARGET_TUPLE}/mig
    ACCEPT_KEYWORDS='**' USE=headers-only  emerge -v1 cross-${TARGET_TUPLE}/hurd
    ACCEPT_KEYWORDS='**' USE=headers-only  emerge -v1 cross-${TARGET_TUPLE}/glibc
    
                         USE="-*"          emerge -v1 cross-${TARGET_TUPLE}/gcc
    ACCEPT_KEYWORDS='**' USE=-headers-only emerge -v1 cross-${TARGET_TUPLE}/glibc
                         USE="-sanitize"   emerge -v1 cross-${TARGET_TUPLE}/gcc

Done!

A few things to note here:

- gentoo-hurd overlay is used for new **gnumach**, **mig** and **hurd** packages
- symlinks to new packages are created manually (crossdev fix pending, wait on packages to get into ::gentoo)
- uninstall linux-headers as our target is not linux (crossdev fix pending)
- use ACCEPT_KEYWORDS='\*\*' for many packages (need to decide on final keywords, maybe x86-hurd)
- all crossdev phases are ran manually
- only glibc git version is working as changes were merged
  upstream `very recently <https://sourceware.org/ml/libc-alpha/2018-04/msg00017.html>`_.
- USE="sanitize" is disabled in final gcc because it's broken for hurd

Now you can go to **/usr/${TARGET_TUPLE}/etc/portage/** and tweak the defaults for **ELIBC**, **KERNEL**
and other things.

Basic sanity check for a toolchain:

.. code-block::

    $ i586-pc-gnu-gcc main.c -o main
    $ file main
    main: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /lib/ld.so, for GNU/Hurd 0.0.0, with debug_info, not stripped

Copying to the target **hurd** VM and runnig there also works as expected.

I use **crossdev -t x86_64-HEAD-linux-gnu** to have **GHC** built against **HEAD** in parallel
to system's **GHC**. Let's use that for more heavyweight test to build a **GHC** cross-compiler to hurd:

.. code-block::

    $ EXTRA_ECONF=--with-ghc=x86_64-HEAD-linux-gnu-ghc emerge -v1 cross-i586-pc-gnu/ghc --quiet-build=n

This fails as:

.. code-block::

    rts/posix/Signals.c:398:28: error:
         note: each undeclared identifier is reported only once for each function it appears in
        |
    398 |         action.sa_flags |= SA_SIGINFO;
        |                            ^

Which hints at lack of **SA_SIGINFO** support in upstream **glibc.git**.
**Debian** as an out-of-tree
`tg-hurdsig-SA_SIGINFO.diff patch <https://sources.debian.org/patches/glibc/2.27-3/hurd-i386/tg-hurdsig-SA_SIGINFO.diff/>`_
to provide these defines (as least it's not our local toolchain breakage).
The outcome is positive: we have got very far into cross-compiling and
hit real portability issues. Woohoo!

Final words
-----------

As long as underlying toolchains are not too complicated building cross-compilers in **gentoo** is trivial.
Next tiny step is to cross-build hurd kernel itself and run it in qemu. Ebuilds in **gentoo-hurd** are not
yet ready for it but tweaking them should be easy.

Have fun!
