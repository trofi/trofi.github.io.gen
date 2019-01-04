---
title: "page fault handling on alpha"
date: January 4, 2019
---

:PostID: 211
:Title: "page fault handling on alpha"
:Keywords: alpha, palcode, gcc, linux, gentoo, qemu, gdb
:Categories: notes

Bug
---

This was a quiet evening on **#gentoo-alpha**. Matt Turner
shared an unusual
`kernel crash report <https://bugs.gentoo.org/672040>`_ seen by Dmitry V. Levin.

Dmitry noticed that one of **AlphaServer ES40** machines does
not handle **strace** test suite and generates kernel oopses:

.. code-block::

    Unable to handle kernel paging request at virtual address ffffffffffff9468
    CPU 3 
    aio(26027): Oops 0
    pc = [<fffffc00004eddf8>]  ra = [<fffffc00004edd5c>]  ps = 0000    Not tainted
    pc is at sys_io_submit+0x108/0x200
    ra is at sys_io_submit+0x6c/0x200
    v0 = fffffc00c58e6300  t0 = fffffffffffffff2  t1 = 000002000025e000
    t2 = fffffc01f159fef8  t3 = fffffc0001009640  t4 = fffffc0000e0f6e0
    t5 = 0000020001002e9e  t6 = 4c41564e49452031  t7 = fffffc01f159c000
    s0 = 0000000000000002  s1 = 000002000025e000  s2 = 0000000000000000
    s3 = 0000000000000000  s4 = 0000000000000000  s5 = fffffffffffffff2
    s6 = fffffc00c58e6300
    a0 = fffffc00c58e6300  a1 = 0000000000000000  a2 = 000002000025e000
    a3 = 00000200001ac260  a4 = 00000200001ac1e8  a5 = 0000000000000001
    t8 = 0000000000000008  t9 = 000000011f8bce30  t10= 00000200001ac440
    t11= 0000000000000000  pv = fffffc00006fd320  at = 0000000000000000
    gp = 0000000000000000  sp = 00000000265fd174
    Disabling lock debugging due to kernel taint
    Trace:
    [<fffffc0000311404>] entSys+0xa4/0xc0

Oopses should never happen against userland workloads.

Here crash happened right in the **io_submit()** syscall.
"Should be a very simple arch-specific bug. Can't take much
time to fix." was my thought. Haha.

Reproducer
----------

Dmitry provided very nice reproducer of the problem (extracted
from **strace** test suite):

.. code-block:: c

    // $ cat aio.c
    #include <err.h>
    #include <unistd.h>
    #include <sys/mman.h>
    #include <asm/unistd.h>
    int main(void)
    {
        unsigned long ctx = 0;
        if (syscall(__NR_io_setup, 1, &ctx))
            err(1, "io_setup");
        const size_t page_size = sysconf(_SC_PAGESIZE);
        const size_t size = page_size * 2;
        void *ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (MAP_FAILED == ptr)
            err(1, "mmap(%zu)", size);
        if (munmap(ptr, size))
            err(1, "munmap");
        syscall(__NR_io_submit, ctx, 1, ptr + page_size);
        syscall(__NR_io_destroy, ctx);
        return 0;
    }

The idea of this test is simple: create valid context for
asynchronous IO and pass invalid pointer **ptr** to it.
**mmap()**/**munmap()** trick makes sure the **ptr** is
pointing at an invalid non-**NULL** user memory location.

To reproduce and explore the bug locally I picked
**qemu** alpha system emulation. To avoid complexity of
searching for proper IDE driver for root filesystem I built
minimal linux kernel with only initramfs support without
filesystem or block device support.

Then I put statically linked reproducer and busybox into
initramfs:

.. code-block::

    $ LANG=C tree root/
    root/
    |-- aio (statically linked aio.c)
    |-- aio.c (source above)
    |-- bin
    |   |-- busybox (statically linked busybox)
    |   `-- sh -> busybox
    |-- dev (empty dir)
    |-- init (script below)
    |-- proc (empty dir)
    `-- sys (empty dir)
    
    4 directories, 5 files
    
    $ cat root/init
    #!/bin/sh
    
    mount -t proc none /proc
    mount -t sysfs none /sys
    exec bin/sh

To run **qemu** system emulation against the above I used
the following one-liner:

.. code-block:: bash

    #!/bin/sh
    #  run-qemu.sh
    
    alpha-unknown-linux-gnu-gcc root/aio.c -o root/aio -static
    ( cd root && find . -print0 | cpio --null -ov --format=newc; ) | gzip -9 > initramfs.cpio.gz
    qemu-system-alpha -kernel vmlinux -initrd initramfs.cpio.gz -m 1G "$@"

**run-qemu.sh** builds **initramfs** image and runs kernel
against it.

Cross-compiling **vmlinux** on **alpha** is also
straightforward:

.. code-block:: bash

    #!/bin/sh
    #  mk.sh
    
    ARCH=alpha                      \
    CROSS_COMPILE=alpha-unknown-linux-gnu- \
    make -C ../linux.git        \
    O="$(pwd)"                  \
                                \
    "$@"


I built kernel and started a VM as: 

.. code-block::

    # build kernel
    $ ./mk.sh -j$(nproc)
    
    # run kernel
    $ ./run-qemu.sh -curses
    ...
    [    0.650390] input: AT Translated Set 2 keyboard as /devices/platform/i8042/serio0/input/input0
    / #

That was simple. I got the prompt! Then I ran statically
linked **/aio** reproducer as:

.. code-block::

    / # /aio
    Unable to handle kernel paging request at virtual address 0000000000000000
    aio(26027): Oops -1
    ...

Woohoo! Crashed \\o/ This allowed me to explore failure in more detail.

I used **-curses** (instead of default **-sdl**) to ease copying
of text back from VM.

Fault address pattern was slightly different from the original
report. I hoped it's a manifestation of the same bug. Worst case
I would find another bug to fix and get back to original one again :)

Into the rabbit hole
--------------------

Oops was happening every time I ran **/aio** on **4.20**
kernel. **io_submit(2)** man page claims it's an old system
call from **2.5** kernel era. Thus it should not be a recent addition.

How about older kernels? Did they also fail?

I was still not sure I had correct qemu/kernel setup. I
decided to pick older **4.14** kernel version known to run
without major problems on our **alpha** box. **4.14** kernel
version did not crash in **qemu** either. This reassured me I
have not completely broken setup.

I got first suspect: kernel regression.

Reproducer was very stable. Kernel bisection got me
to `first regressed commit <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=95af8496ac48263badf5b8dde5e06ef35aaace2b>`_:

.. code-block::

    commit 95af8496ac48263badf5b8dde5e06ef35aaace2b
    Author: Al Viro <viro@zeniv.linux.org.uk>
    Date:   Sat May 26 19:43:16 2018 -0400
    
        aio: shift copyin of iocb into io_submit_one()
    
        Reviewed-by: Christoph Hellwig <hch@lst.de>
        Signed-off-by: Al Viro <viro@zeniv.linux.org.uk>
    
    :040000 040000 20dd44ac4706540b1c1d4085e4269bd8590f4e80 05d477161223e5062f2f781b462e0222c733fe3d M      fs

The commit clearly touched **io_submit()** syscall handling.
But there is a problem: the change was not alpha-specific at all.
If commit had any problems it also should have caused problems
on other systems.

To get better understanding of probable cause I decided to
look at failure mechanics. Actual values of local variables
in **io_submit()** right before crash might get me somewhere.
I started adding **printk()** statements around
`SYSCALL_DEFINE3(io_submit, ...) <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/aio.c#n1923>`_
implementation.

At some point after enough **printk()** calls added crashes
disappeared. This confirmed it was not just a logical bug but
something more subtle.

I also was not able to analyze the generated code difference
between **printk()**/no-**printk()** versions.

Then I attempted to isolate faulty code into a separate function
but not much success here either. Any attempt to factor out a
subset of **io_submit()** into a separate function made bug to
go away.

It was time for a next hypothesis: mysterious incorrect compiler
code generation or invalid **__asm__** constraints for some kernel
macro exposed after minor code motion.

Single stepping through kernel
------------------------------

How to get an insight into the details without affecting
original code too much?

Having failed at minimal code snippet I attempted to catch exact
place of page fault by single-stepping through kerenel using **gdb**.

For **qemu**-loadable kernels the procedure very straightforward:

- start gdb server on qemu side with **-s** option
- start gdb client on host side with **target remote localhost:1234**

The same procedure in exact commands (I'm hooking into **sys_io_submit()**):

.. code-block::

    <at tty1>
    $ ./run-qemu.sh -s
    
    <at tty2>
    $ gdb --quiet vmlinux
    (gdb) target remote localhost:1234
      Remote debugging using localhost:1234
      0xfffffc0000000180 in ?? ()
    (gdb) break sys_io_submit 
      Breakpoint 1 at 0xfffffc000117f890: file ../linux-2.6/fs/aio.c, line 1890.
    (gdb) continue
      Continuing.
    
    <at qemu>
      # /aio
    
    <at tty2 again>
      Breakpoint 1, 0xfffffc000117f89c in sys_io_submit ()
    (gdb) bt
      Breakpoint 1, __se_sys_io_submit (ctx_id=2199023255552, nr=1, iocbpp=2199023271936) at ../linux-2.6/fs/aio.c:1890
      1890    SYSCALL_DEFINE3(io_submit, aio_context_t, ctx_id, long, nr,
    (gdb) bt
      #0  __se_sys_io_submit (ctx_id=2199023255552, nr=1, iocbpp=2199023271936) at ../linux-2.6/fs/aio.c:1890
      #1  0xfffffc0001011254 in entSys () at ../linux-2.6/arch/alpha/kernel/entry.S:476

Now we can single-step through every instruction with
**nexti** and check where things go wrong.

To poke around efficiently I kept looking at these cheat sheets:

- `alpha register names and meaning <https://www2.cs.arizona.edu/projects/alto/Doc/local/alpha.register.html>`_ (1 page)
- `alpha instruction names and meaning <https://www2.cs.arizona.edu/projects/alto/Doc/local/alpha.instruction.html>`_ (3 pages)

Register names are especially useful as each **alpha** register
has two names: numeric and mnemonic. Source code might use one
form and gdb disassembly might use another. For example
**$16**/**a0** for **gas** (**$r16**/**$a0** for **gdb**)
is a register to pass first integer argument to function.

After many backs and forths I found the suspicious behaviour
when handling single instruction:

.. code-block::

   (gdb) disassemble
     => 0xfffffc000117f968 <+216>:   ldq     a1,0(t1)
        0xfffffc000117f96c <+220>:   bne     t0,0xfffffc000117f9c0 <__se_sys_io_submit+304>
   (gdb) p $gp
       $1 = (void *) 0xfffffc0001c70908 # GOT
   (gdb) p $a1
       $2 = 0
   (gdb) p $t0
       $3 = 0
   (gdb) nexti
        0xfffffc000117f968 <+216>:   ldq     a1,0(t1)
     => 0xfffffc000117f96c <+220>:   bne     t0,0xfffffc000117f9c0 <__se_sys_io_submit+304>
   (gdb) p $gp
       $4 = (void *) 0x0
   (gdb) p $a1
       $5 = 0
   (gdb) p $t0
      $6 = -14 # -EFAULT

The above **gdb** session executes single **ldq a1,0(t1)** instruction
and observes effect on the registers **gp**, **a1**, **t0**.

Normally **ldq a1, 0(t1)** would read 64-bit value pointed by **t1**
into **a1** register and leave **t0** and **gp** untouched.

The main effect seen here that causes later OOps is sudden
**gp** change. **gp** is supposed to point to **GOT** (global
offset table) table in current "program" (kernel in this case).
Something managed to corrupt it.

By **/aio** test case construction instruction **ldq a1,0(t1)**
is not supposed to read any valid data: our test case passes
invalid memory location there. All the register changing
effects are the result of page fault handling.

The smoking gun
---------------

Grepping around **arch/alpha** directory I noticed
`entMM page fault handling entry <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n199>`_.

It claims to handle page faults and keeps **gp** value on stack.
Let's trace the fate of that on-stack value as page fault happens:

.. code-block::

   (gdb) disassemble
     => 0xfffffc000117f968 <+216>:   ldq     a1,0(t1)
        0xfffffc000117f96c <+220>:   bne     t0,0xfffffc000117f9c0 <__se_sys_io_submit+304>
   (gdb) p $gp
       $1 = (void *) 0xfffffc0001c70908 # GOT
   
   (gdb) break entMM
       Breakpoint 2 at 0xfffffc0001010e10: file ../linux-2.6/arch/alpha/kernel/entry.S, line 200
   (gdb) continue
       Breakpoint 2, entMM () at ../linux-2.6/arch/alpha/kernel/entry.S:200
   (gdb) x/8a $sp
       0xfffffc003f51be78:     0x0     0xfffffc000117f968 <__se_sys_io_submit+216>
       0xfffffc003f51be88:     0xfffffc0001c70908 <# GOT> 0xfffffc003f4f2040
       0xfffffc003f51be98:     0x0     0x20000004000 <# userland address>
       0xfffffc003f51bea8:     0xfffffc0001011254 <entSys+164> 0x120001090
   (gdb) watch -l *0xfffffc003f51be88
       Hardware watchpoint 3: -location *0xfffffc003f51be88
   (gdb) continue
       Old value = 29821192
       New value = 0
       0xfffffc00010319d0 in do_page_fault (address=2199023271936, mmcsr=<optimized out>, cause=0, regs=0xfffffc003f51bdc0)
          at ../linux-2.6/arch/alpha/mm/fault.c:199
       199                     newpc = fixup_exception(dpf_reg, fixup, regs->pc);

Above **gdb** session does the following:

- **break entMM**: break at page fault
- **x/8a $sp**: print **8** top stack values at **entMM** call time
- spot **gp** value at **0xfffffc003f51be88** (**sp**+16) address
- **watch -l *0xfffffc003f51be88**: set hardware watchpoint at
  a memory location where **gp** is stored.

Watch triggers at seemingly relevant place:
`fixup_exception() <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/include/asm/extable.h#n39>`_
where exception handler adjusts registers before resuming the
faulted task.

Looking around I found an off-by-two bug in page fault handling
code. The fix was simple:

.. code-block:: c

    --- a/arch/alpha/mm/fault.c
    +++ b/arch/alpha/mm/fault.c
    @@ -80,2 +80,2 @@ __load_new_mm_context(struct mm_struct *next_mm)
            (((unsigned long *)regs)[(r) <= 8 ? (r) : (r) <= 15 ? (r)-16 :  \
    -                                (r) <= 18 ? (r)+8 : (r)-10])
    +                                (r) <= 18 ? (r)+10 : (r)-10])

Patch is proposed upstream as https://lkml.org/lkml/2018/12/31/83.

Effect of the patch is to write **0** into on-stack location
of **a1** (**$17** register) instead of location of **gp**.

That's it!

Page fault handling magic
-------------------------

I always wondered how kernel reads data from userspace when
it's needed. How does it do swap-in if data is not available?
How does it check for permission privilege access? That kind
of stuff.

The above investigation covers most of involved components:

- **ldq** instruction is used to force the read from userspace
  (as one would read from kernel's memory)
- **entMM**/**do_page_fault()** handles the userspace fault as
  if fault would not happen

The few minor missing details are:

- How does kernel know which instructions are known to generate
  user page faults?
- What piece of hardware holds a pointer to page fault handler
  on **alpha**?

Let's expand the code involved in page fault handling. Call site:

.. code-block:: c

    SYSCALL_DEFINE3(io_submit, aio_context_t, ctx_id, long, nr,
                    struct iocb __user * __user *, iocbpp)
    {
        // ...
        struct iocb __user *user_iocb;
        if (unlikely(get_user(user_iocb, iocbpp + i))) {
        // ...

which is translated to already familiar pair of instructions:

.. code-block::

     => 0xfffffc000117f968 <+216>:   ldq     a1,0(t1)
        0xfffffc000117f96c <+220>:   bne     t0,0xfffffc000117f9c0 <__se_sys_io_submit+304>

Fun fact: **get_user()** has two return values: normal function
return value (stored into **t0** register) and **user_iocb**
effect (stored into **a1** register).

Let's expand `get_user() implementation <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/include/asm/uaccess.h#n59>`_
on **alpha**:

.. code-block:: c

    // somewhere at arch/alpha/include/asm/uaccess.h:
    #define get_user(x, ptr) \
        __get_user_check((x), (ptr), sizeof(*(ptr)))
    
    #define __get_user_check(x, ptr, size)                       \
        ({                                                       \
            long __gu_err = -EFAULT;                             \
            unsigned long __gu_val = 0;                          \
            const __typeof__(*(ptr)) __user *__gu_addr = (ptr);  \
            if (__access_ok((unsigned long)__gu_addr, size)) {   \
                __gu_err = 0;                                    \
                switch (size) {                                  \
                  case 1: __get_user_8(__gu_addr); break;        \
                  case 2: __get_user_16(__gu_addr); break;       \
                  case 4: __get_user_32(__gu_addr); break;       \
                  case 8: __get_user_64(__gu_addr); break;       \
                  default: __get_user_unknown(); break;          \
                }                                                \
            }                                                    \
            (x) = (__force __typeof__(*(ptr))) __gu_val;         \
            __gu_err;                                            \
        })

A lot of simple code above does two things:

1. use **__access_ok()** to check for address to be a userspace address to prevent
   data exfiltration from kernel.
2. dispatch across different supported sizes to do the rest of work. Our case is a simple 64-bit read.


Looking at **__get_user_64()** in more detail:

.. code-block:: c

    struct __large_struct { unsigned long buf[100]; };
    #define __m(x) (*(struct __large_struct __user *)(x))
    
    #define __get_user_64(addr)                    \
        __asm__("1: ldq %0,%2\n"                   \
                "2:\n"                             \
                EXC(1b,2b,%0,%1)                   \
               : "=r"(__gu_val), "=r"(__gu_err)    \
               : "m"(__m(addr)), "1"(__gu_err))
    
    #define EXC(label,cont,res,err)                \
    ".section __ex_table,\"a\"\n"                  \
    ".long "#label"-.\n"                           \
    "lda "#res","#cont"-"#label"("#err")\n"        \
    ".previous\n"

A few observations:

- The actual check for address validity is done by CPU: load-8-bytes instruction (**ldq %0,%2**) is executed and MMU handles a page fault
- There is no explicit code to recover from the exception. All auxiliary information it put into **__ex_table** section.
- **ldq %0,%2** instruction uses only parameters "0" (**__gu_val**) and "2"(**addr**) but does not use "1"(**__gu_err**) parameter directly.
- **__ex_table** uses cool **lda** instruction hack to encode auxiliary data:
  - **__gu_err** error register
  - pointer to next instruction after faulty instrustion: **cont-label** (or **2b-1b**)
  - result register

Page fault handling mechanism knows how to get to
**__ex_table** data where "1"(**__gu_err**) is encoded and is
able to reach that data to use it later in mysterious
**fixup_exception()** we saw before.

In case of **alpha** (and many other targets) **__ex_table**
collection is defined by **arch/alpha/kernel/vmlinux.lds.S**
linker script using `EXCEPTION_TABLE() macro <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/include/asm-generic/vmlinux.lds.h#n562>`_:

.. code-block::

    #define EXCEPTION_TABLE(align)                         \
        . = ALIGN(align);                                  \
        __ex_table : AT(ADDR(__ex_table) - LOAD_OFFSET) {  \
            __start___ex_table = .;                        \
            KEEP(*(__ex_table))                            \
            __stop___ex_table = .;                         \
        }
    //...

Here all **__ex_table** sections are gathered between **__start___ex_table** and **__stop___ex_table** symbols.
Those are handled by generic `kernel/extable.c <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/kernel/extable.c>`_ code:

.. code-block:: c

    const struct exception_table_entry *search_exception_tables(unsigned long addr);

**search_exception_tables()** resolves faut address to relevant **struct exception_table_entry**.

Let's look at the definition of `struct exception_table_entry <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/include/asm/extable.h#n25>`_:

.. code-block:: c

    /* Once again part of __get_user_64() responsible for __ex_table:
    
      #define EXC(label,cont,res,err)                    \
      ".section __ex_table,\"a\"\n"                      \
          ".long "#label"-.\n"                           \
          "lda "#res","#cont"-"#label"("#err")\n"        \
      ".previous\n"
    */
    
    struct exception_table_entry
    {
        signed int insn;                  /* .long #label-. */
        union exception_fixup {
            unsigned unit;                /* lda #res,#cont-#label(#err) */
            struct {
                signed int nextinsn : 16; /*   #cont-#label part */
                unsigned int errreg : 5;  /*   #err part */
                unsigned int valreg : 5;  /*   #res part */
            } bits;
        } fixup;
    };
    
    /* Returns the new pc */
    #define fixup_exception(map_reg, _fixup, pc)               \
    ({                                                         \
        if ((_fixup)->fixup.bits.valreg != 31)                 \
            map_reg((_fixup)->fixup.bits.valreg) = 0;          \
        if ((_fixup)->fixup.bits.errreg != 31)                 \
            map_reg((_fixup)->fixup.bits.errreg) = -EFAULT;    \
        (pc) + (_fixup)->fixup.bits.nextinsn;                  \
    })

Note how **lda** in-memory instruction format is used to encode
all details needed by **fixup_exception()**! In case of our
**sys_io_submit()** case it would be **lda a1, 4(t0)** (**lda r17, 4(r1)**):

.. code-block::

    (gdb) bt
      #0  0xfffffc00010319d0 in do_page_fault (address=2199023271936, mmcsr=<optimized out>, cause=0, 
          regs=0xfffffc003f51bdc0) at ../linux-2.6/arch/alpha/mm/fault.c:199
      #1  0xfffffc0001010eac in entMM () at ../linux-2.6/arch/alpha/kernel/entry.S:222
    (gdb) p *fixup
        $4 = {insn = -2584576, fixup = {unit = 572588036, bits = {nextinsn = 4, errreg = 1, valreg = 17}}}

Note how page fault handling also advances **pc** (program counter or instruction pointer)
**nextinsn=4** bytes forward to skip failed **ldq** instruction.

`arch/alpha/mm/fault.c <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/mm/fault.c#n84>`_
does all the heavy-lifting of handling page faults. Here is a
small snippet that handles our case of faults covered by
exception handling:

.. code-block:: c

    asmlinkage void
    do_page_fault(unsigned long address, unsigned long mmcsr,
                  long cause, struct pt_regs *regs)
    {
        // ...
      no_context:
        /* Are we prepared to handle this fault as an exception?  */
        if ((fixup = search_exception_tables(regs->pc)) != 0) {
            unsigned long newpc;
            newpc = fixup_exception(dpf_reg, fixup, regs->pc);
            regs->pc = newpc;
            return;
        }
        // ...
    }
    /* ...
     * Registers $9 through $15 are saved in a block just prior to `regs' and
     * are saved and restored around the call to allow exception code to
     * modify them.
     */
    /* Macro for exception fixup code to access integer registers.  */
    #define dpf_reg(r)                                                  \
        (((unsigned long *)regs)[(r) <= 8 ? (r) : (r) <= 15 ? (r)-16 :  \
                                 (r) <= 18 ? (r)+8 : (r)-10])


**do_page_fault()** also does a few other page-fault related things
I carefully skipped here:

- page fault accounting
- handling of missing support for "prefetch" instruction
- stack growth
- OOM handling
- **SIGSEGV**, **SIGBUS** propagation

Once **do_page_fault()** gets control it updates **regs** struct
in memory for faulted task using **dpf_reg()** macro. It looks
unusual:

- refers to negative offsets sometimes: **(r) <= 15 ? (r)-16** (out of **struct pt_regs**)
- defines not one but a few ranges of registers: 0-8, 9-15, 16-18, 19-...

**struct pt_regs** as is:

.. code-block:: c

    struct pt_regs {
        unsigned long r0;  // 0
        unsigned long r1;
        unsigned long r2;
        unsigned long r3;
        unsigned long r4;
        unsigned long r5;  // 5
        unsigned long r6;
        unsigned long r7;
        unsigned long r8;
        unsigned long r19;
        unsigned long r20; // 10
        unsigned long r21;
        unsigned long r22;
        unsigned long r23;
        unsigned long r24;
        unsigned long r25; // 15
        unsigned long r26;
        unsigned long r27;
        unsigned long r28;
        unsigned long hae;
    /* JRP - These are the values provided to a0-a2 by PALcode */
        unsigned long trap_a0; // 20
        unsigned long trap_a1;
        unsigned long trap_a2;
    /* These are saved by PAL-code: */
        unsigned long ps;
        unsigned long pc;
        unsigned long gp; // 25
        unsigned long r16;
        unsigned long r17;
        unsigned long r18;
    };

Now meaning of **dpf_reg()** should be more clear. As **pt_regs**
keeps only a subset of registers is has to account for gaps and offsets.

Here I noticed the bug: **r16-r18** range is handled incorrectly by **dpf_reg()**:
**r16** "address" is **regs**+10 (26-16), not **regs**+8.

The implementation also means that **dpf_reg()** can't handle
**gp**(**r29**) and **sp**(**r30**) registers as value registers.
That should not normally be a problem as **gcc** never assigns
those registers for temporary computations and keeps them to
hold **GOT** pointer and stack pointer at all times. But one
could write assembly code to do it :)

If all the above makes no sense to you it's ok. Check
`kernel documentation for x86 exception handling <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/Documentation/x86/exception-tables.txt>`_
instead which uses very similar technique.

To be able to handle all registers we need to bring in **r9-r15**.
Those are written right before **struct pt_regs**
`right at entMM <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n199>`_
entry:

.. code-block:: asm

    CFI_START_OSF_FRAME entMM
        SAVE_ALL
        /* save $9 - $15 so the inline exception code can manipulate them.  */
        subq $sp, 56, $sp
        stq $9, 0($sp)   // push r9
        stq $10, 8($sp)
        stq $11, 16($sp)
        stq $12, 24($sp)
        stq $13, 32($sp)
        stq $14, 40($sp)
        stq $15, 48($sp) // push r15
        addq $sp, 56, $19
        /* handle the fault */
        lda $8, 0x3fff
        bic $sp, $8, $8
        jsr $26, do_page_fault
        /* reload the registers after the exception code played.  */
        ldq $9, 0($sp) // pop r9
        ldq $10, 8($sp)
        ldq $11, 16($sp)
        ldq $12, 24($sp)
        ldq $13, 32($sp)
        ldq $14, 40($sp)
        ldq $15, 48($sp) // pop r15
        addq $sp, 56, $sp
        /* finish up the syscall as normal.  */
        br ret_from_sys_call
    CFI_END_OSF_FRAME entMM

Here there are a few subtle things going on:

1. at entry **entMM** already has a frame of last 6 values: **ps,pc,gp,r16-r18**.
2. then `SAVE_ALL <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n52>`_
   (not pasted bove) stores **r0-r8,r19-r28,hae,trap_a0-trap-a2**
3. and only then **r9-r15** are stored (note the **subq $sp, 56, $sp** to place them before).

In **C** land only **2.** and **3.** constitute **struct pt_regs**.
**1.** happens to be outside and needs negative addressing we saw in **dpf_reg()**.

As I understand the original idea was to share
**ret_from_sys_call** part across various kernel entry points:

- system calls: `entSys <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n434>`_
- arithmetic exceptions: `entArith <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n190>`_
- external interrupts: `entInt <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n181>`_
- internal faults (bad opcode, FPU failures, breakpoint traps, ): `entIF <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n244>`_
- page faults: entMM
- handling of unaligned access: `entUna <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n253>`_
- MILO debug break: `entDbg <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/entry.S#n425>`_

Of the above only page faults and unaligned faults need
read/write acceess to every register.

In practice **entUna** uses different layout and simpler code patching.

The last step to get **entMM** executed at a fault handler is
to register it in alpha's **PALcode** subsystem (Privileged
Architecture Library code).

It's done in `trap_init() <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/alpha/kernel/traps.c#n976>`_.
along with other handlers. Simple!

Or not so simple. What is that **PALcode** thing (`wiki's link <https://en.wikipedia.org/wiki/PALcode>`_)?
It looks like a tiny hypervisor that provides service points for CPU
you can access with **call_pal <number>** instruction.

It puzzled me a lot of what **call_pal** was supposed to do.
Should it transfer control somwehre else or is it a normal call?

Actually given it's a generic mechanism to do "privileged service calls"
it can do both. I was not able to quickly find the details on how
different service calls affect registers and found it simplest
to navigate through qemu's `PAL source <https://repo.or.cz/qemu-palcode.git/blob/HEAD:/pal.S>`_.

AFAIU **PALcode** of real alpha machine is a proprietary
process-specific blob that could have it's own quirks.

Back to out **qemu-palcode** let's looks at a few examples.

First is function-like **call_pal PAL_swpipl** used in
**entMM** and others:

.. code-block:: asm

    CallPal_SwpIpl:
        mfpr    v0, qemu_ps
        and     a0, PS_M_IPL, a0
        and     v0, PS_M_IPL, v0
        mtpr    a0, qemu_ps
        hw_rei
    ENDFN   CallPal_SwpIpl

I know almost nothing about **PAL** but I suspect **mfpr** means
move-from-physical-register. **hw_rei/hw_ret** is a branch from
**PAL** service routine back to "unprivileged" user/kernel.

**hw_rei** does normal return from **call_pal**
to the instruction next to **call_pal**.

Here **call_pal PAL_rti** is an example of task-switch-like routine:

.. code-block:: asm

    CallPal_Rti:
        mfpr    p6, qemu_exc_addr       // Save exc_addr for machine check

        ldq     p4, FRM_Q_PS($sp)       // Get the PS
        ldq     p5, FRM_Q_PC($sp)       // Get the return PC
        ldq     $gp, FRM_Q_GP($sp)      // Get gp
        ldq     a0, FRM_Q_A0($sp)       // Get a0
        ldq     a1, FRM_Q_A1($sp)       // Get a1
        ldq     a2, FRM_Q_A2($sp)       // Get a2
        lda     $sp, FRM_K_SIZE($sp)    // Pop the stack

        andnot  p5, 3, p5               // Clean return PC<1:0>

        and     p4, PS_M_CM, p3
        bne     p3, CallPal_Rti_ToUser

        and     p4, PS_M_IPL, p4
        mtpr    p4, qemu_ps
        hw_ret  (p5)
    ENDFN   CallPal_Rti

Here target (**p5**, some service only hardware register)
was passed on stack in **FRM_Q_PC($sp)**.

That **PAL_rti** managed to confused me a lot as I was
trying to single-step through it as a normal function.
I did not notice how I was jumping from page fault
handling code to timer interrupt handling code.

But all became clear once I found it's definition.

Parting words
-------------

- **qemu** can emulate **alpha** good enough to debug obscure kernel bugs
- **gdb** server is very powerful for debugging unmodified kernel code including
  hardware watchpoints, dumping registers, watching after interrupt handling
  routines
- My initial guesses were all incorrect: it was not a kernel regression,
  not a compiler deficiency and not an **__asm__** constraint annotation bug.
- **PALcode** while a nice way to abstract low-level details
  of CPU implementation complicates debugging of operating system. **PALcode**
  also happens to be OS-dependent!
- This was another one-liner fix :)
- The bug has been always present in kernel (for about 20 years?).

Have fun!
