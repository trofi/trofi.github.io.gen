---
title: c-reduce nano howto
date: March 5, 2017
---

:PostID: 198
:Title: c-reduce nano howto
:Keywords: c, gcc, howto, ICE, guile, c-reduce
:Categories: notes

What is c-reduce? The `home page <http://embed.cs.utah.edu/creduce/>`_ gives
very nice explanation:

.. code-block::

    C-Reduce is a tool that takes a large C, C++, or OpenCL file that has a property of interest
    (such as triggering a compiler bug) and automatically produces a much smaller C/C++ file that
    has the same property. It is intended for use by people who discover and report bugs in compilers
    and other tools that process source code.


Yesterday Amynka shared an interesting guile build
failure: `bug #610052 <https://bugs.gentoo.org/show_bug.cgi?id=610052>`_

The build error is of rare and cryptic sort:

.. code-block::

    ../libguile/guile-snarf -o vm.x vm.c -DHAVE_CONFIG_H -DBUILDING_LIBGUILE=1 -I.. -I.. -I../lib -I../lib -I/usr/lib64/libffi-3.2.1/include  -I/var/tmp/portage/dev-scheme/guile-2.0.14/work/guile-2.0.14  -march=native -mtune=native -Os -pipe -ggdb
    ...
    libtool: compile:  gcc -DHAVE_CONFIG_H -DBUILDING_LIBGUILE=1 -I.. -I.. -I../lib -I../lib -I/usr/lib64/libffi-3.2.1/include -I/var/tmp/portage/dev-scheme/guile-2.0.14/work/guile-2.0.14 -pthread -Wall -Wmissing-prototypes -Wdeclaration-after-statement -Wpointer-arith -Wswitch-enum -fno-strict-aliasing -fwrapv -fvisibility=hidden -march=native -mtune=native -Os -pipe -ggdb -c vm.c  -fPIC -DPIC -o .libs/libguile_2.0_la-vm.o
    ...
    Unable to coalesce ssa_names 48 and 3288 which are marked as MUST COALESCE.
    sp_48(ab) and  sp_3288(ab)
    vm-engine.c: In function 'vm_debug_engine':
    vm.c:666:19: internal compiler error: SSA corruption
     #define VM_NAME   vm_debug_engine
    ...
    Please submit a full bug report,
    with preprocessed source if appropriate.
    See <https://bugs.gentoo.org/> for instructions.

**internal compiler error: SSA corruption** says the thing we see is a **GCC** bug.

User provided plenty information like:

- gcc version: 4.9.4
- CPU: AMD A10 7850K Radeon R7
- CFLAGS="-march=native -mtune=native -Os -pipe -ggdb"

I've tried to reproduce it on **i7-2700K** CPU and did not succeed.
**-march=native** is a flag that picks instructions
available on currently running CPU model+GCC version.

To find out what **AMD A10 7850K** translates to I've looked
at available **-march=** options in **man gcc**.

That is a huge list even for x86_64. I had to pick from an extensive **AMD** subset (starts at **-march=k6**).
Something like **-march=amdfam10** or later (**-march=bdver1**, **-march=bdver2**, etc.)
looked like a good match. I chose **-march=bdver2** mostly at random :)

.. code-block::

  bdver2
      AMD Family 15h core based CPUs with x86-64 instruction set support.  (This supersets BMI, TBM, F16C, FMA, FMA4, AVX, XOP, LWP, AES, PCL_MUL, CX16, MMX, SSE,
      SSE2, SSE3, SSE4A, SSSE3, SSE4.1, SSE4.2, ABM and 64-bit instruction set extensions.)

Reran build command changing **-march=native** to **-march=bdver2**

.. code-block::

    gcc -DHAVE_CONFIG_H -DBUILDING_LIBGUILE=1 -I.. -I.. -I../lib -I../lib -I/usr/lib64/libffi-3.2.1/include -I/var/tmp/portage/dev-scheme/guile-2.0.14/work/guile-2.0.14 -pthread -Wall -Wmissing-prototypes -Wdeclaration-after-statement -Wpointer-arith -Wswitch-enum -fno-strict-aliasing -fwrapv -fvisibility=hidden -march=bdver2 -mtune=bdver2 -Os -pipe -ggdb -c vm.c  -fPIC -DPIC -o .libs/libguile_2.0_la-vm.o

and got the same **GCC** crash \\o/. Yay! Now we have something to look at.

**vm.c** relies on local (to **guile**) and system headers. It's not clear what
triggered the error, so I wanted to get a selfcontained example. **GCC** bug reporting manual `suggests <https://gcc.gnu.org/bugs/>`_ 
using **-save-temps** option to bundle everything required into a single file. Single files are
easier to deal with.

.. code-block::

    gcc ...<skip>... -march=bdver2 -mtune=bdver2 -Os -pipe -ggdb -c vm.c  -fPIC -DPIC -o .libs/libguile_2.0_la-vm.o -save-temps

The result is stored in **vm.i**. We can shrink our compile command down to:

.. code-block::

    gcc -march=bdver2 -mtune=bdver2 -Os -c vm.i

**vm.i** file is 1052623 bytes long (like a book of decent size). Who knows what went wrong there.

Googling for **internal compiler error: SSA corruption** revealed quite a few bugs
related to **SSA** bugs. One of bugs hinted at **c-reduce** as a tool to shrink
original **C** source files.

I've decided to give it a try. **c-reduce** is very easy to use: we need to construct
a wrapper script that has 2 exit codes:

- **0** - interesting case (or "bug is triggered" in our case)
- **1** - uninteresting case (or "bug is gone")

.. code-block:: bash

    # cat test.sh
    LANG=C gcc-4.9.4 -march=bdver2 -O1 -c vm.i -o vm.o >gcc_out.txt 2>&1
    grep 'internal compiler error' gcc_out.txt >/dev/null 2>&1

That's it! Let's start the process:

.. code-block:: bash

    $ time creduce ./test.sh vm.i
    ===< 2043 >===
    running 4 interestingness tests in parallel
    ===< pass_includes :: 0 >===
    ===< pass_unifdef :: 0 >===
    ===< pass_comments :: 0 >===
    (0.0 %, 1052583 bytes)
    ===< pass_blank :: 0 >===
    (0.8 %, 1044718 bytes)
    (5.1 %, 998478 bytes)
    ===< pass_clang_binsrch :: replace-function-def-with-decl >===
    (6.4 %, 985456 bytes)
    (8.3 %, 965433 bytes)
    (8.7 %, 960939 bytes)
    ...
    ...<takes a while>...
    ...
          ******** /tmp/y/vm.i ********
    
    *a;
    b() {
      char *c;
      static d;
      e();
      d = c++;
      f();
      _setjmp();
      goto *a[*c];
    }
    
    real    7m53,030s
    user    19m0,687s
    sys     3m37,214s


The final result is stored in original **vm.i** file:

.. code-block:: c

    *a;
    b() {
      char *c;
      static d;
      e();
      d = c++;
      f();
      _setjmp();
      goto *a[*c];
    }

Only 88 bytes! Not the best piece of **C** ever written but still perfectly valid.
And way better than original size-wise :)

This code is portable as it does not contain any gcc version-specific intrinsics
like **__builtin_va_list** or anything like that.

Now we more or less know what to look for. **GCC** had a
`similar bug <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=57584>`_ where
**-O1 -ftree-vectorize** was enough to trigger internal compiler error (aka **ICE**).

Our case is even simpler. It's not CPU model specific anymore:

.. code-block::

    $ gcc-4.9.4 -c vm.i -O1
    ...
    Unable to coalesce ssa_names 2 and 9 which are marked as MUST COALESCE.
    c_2(ab) and  c_9(ab)

So, why wasn't the original bug triggered on my **-march=corei7-avx** machine? I don't know!
Perhaps **GCC**'s intermediate representation slightly depends on target CPU model.

None of **gcc-5.4.0** or **gcc-6.3.0** crash on that small snippet of code.
The bug was likely fixed but not merged to **gcc-4.9** branch.
**gcc-4.9.4** was `the final release <https://gcc.gnu.org/ml/gcc/2016-07/msg00084.html>`_ in **4.9** branch.

Random findings:

- **save-temps** and **c-reduce** automates away large part of test simplification!
  We need to build similar tools for haskell. It's quite tedious to do shrinking by hands.
- old compilers have scary bugs :)
- It is tricky to figure out what **-march=native** expands to. **gcc -v** can help you in that :)

Have fun!
