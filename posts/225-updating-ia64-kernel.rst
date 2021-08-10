---
title: "ia64 ptrace: off-by-one yak"
date: August 7, 2021
---

:PostID: 225
:Title: "ia64 ptrace: off-by-one yak"
:Keywords: ia64, gentoo, linux, kernel
:Categories: notes

Gentoo has one alive **ia64** machine: **HP rx3600** nicknamed **guppy**.
It's an old box, and yet it's specs are not too different from my
10 years old desktop: 32 GB RAM, 2 cores x 2 hyperthreads each.

Thus it's suitable for on-host debugging of all sorts up to building
and running **gcc** testsuites or remote X client apps like pre-rust
version of firefox.

kernel upgrade
--------------

There were a few major upstream kernel changes that made me a bit
nervous to update kernel on that machine from **4.9**:

- **cciss** SCSI driver was removed in favour of **hpsa** one
  (which among other things changed **/dev/** paths for device)
- large scale memory management cleanups broke kernel build on
  **ia64** for some memory models and that slipped to release tarball
  prompting questions of **ia64** support status.

Fast forward a few years and we were running **4.9** kernel in 2021.
Whoops. No fancy new **statx()** syscalls or modern container whistles.

Also John Paul Adrian started asking question if Gentoo were seeing
NUMA-related boot failures. I decided to upgrade a kernel to one of
recent releases.

First I attempted to upgrade straight to then most recent **5.10** (it
took me a while to write about it). Built **5.10** and booted
it without any problems (**grub**++ for ease of messing with **root=**
over BMC). I rebuilt a few hevyweight packages to check basic ability
to roll back if needed. No problems! Phew! End of the story!

Or not.

the quest of strace
-------------------

A few days after kernel upgrade Dmitry (**strace** upstream maintainer)
reported a bug uncovered by **strace** testsuite: https://bugs.gentoo.org/769614.
Many tests started failing for invalid syscall parameters as seen by
**ptrace()**.

Dmitry noticed that sometimes syscall parameters were off-by-one:

- expected: sys_foo(a1,a2,a3,...)
- observed: sys_foo(__NR_foo,a1,a2,a3,...)

Looks like a trivial bug, right?

Quiz: pause here for a minute and try to guess the nature of this bug.
It's ok (and encouraged!) not to have any prior knowledge of kernels,
**ptrace()** facility or **ia64** architecture.

What kind of kernel change could trigger failures and what was the real
cause of the observed discrepancy? A few distractors to chose for you:

- mechanical coding bug in recent kernel (like using wrong register during
  unwind)
- subtle failed assumption like stack handling direction or syscall argument
  passing
- a dormant **strace** bug
- CPU hardware failure
- something else

the clues
---------

At first I thought it to be a simpler form of
`alpha's off-by-one bug </posts/211-page-fault-handling-on-alpha.html>`_ where we
encoded incorrect offsets to **struct pt_regs**.

The fun and scary thing about **ia64** compared to many other arches
is that it's use of **pt_regs** is a lot more `involved </posts/210-ptrace-and-accidental-boot-fix-on-ia64.html>`_:
register access requires kernel stack unwindind up to the userspace boundary.
But complex unwinnding also happens to make it more uniform compared to other arches:
most of the time you just use slow unwinder instead of hardcoding anything
fast or special cased.

I hoped the fix would be a simpler variant of the alpha bug. With that bit of
experience I hoped it would help.

The presence of unexpected syscall number in syscall parameters for
**ptrace()** bothered me a bit. Why does it appear in such a strange
place?

I skimmed through **arch/ia64** kernel commits in **4.9..5.10** range related
to **ptrace()**, found 5 of them, stared a lot at them and did not find anything
suspicious.

Then I skimmed through kernel's side of intercepting syscall parameters. I looked
at a **break** trapping instruction handling. **break** instruction is similar
to **x86**'s **int**: it trigger interrupt style switch to kernel context. With
a caveat that everything on **ia64** is special.

**ptrace()** handling path looked straightforward:

Tracee:

- entry point is **ENTRY(break_fault)** (at **arch/ia64/kernel/ivt.S**)
  * save only part of **struct pt_regs**
  * **GLOBAL_ENTRY(ia64_trace_syscall)** (at **arch/ia64/kernel/entry.S**):
    + **PT_REGS_SAVES**: create new frame worth of **struct pt_regs**
    + **f6-f11** FPUs regs are saved
    + **syscall_trace_enter** (asm->C boundary is crossed here, registers are
      passed as arguments) (at **arch/ia64/kernel/ptrace.c**)
      * **tracehook_report_syscall_entry** communicates state to tracer
      * tracer resumes and gets data

Tracer:

- **ptrace_get_syscall_info_entry**
  * **info->entry.nr = syscall_get_nr(child, regs);** // regs->r15

The complication here is to find out which of two paths is buggy:
tracer path or tracee path? Tracee path looks more involved and
has a higher chance to fail. I needed to poke at a real example
to match expected and observed states.

I attempted to reproduce the bug on **strace** test suite and
instantly got the same result. Yay! Sharing the machine with
the reporter is very convenient :)

I started sprinkling **printk()** statement all over the kernel.
Most interesting result was for **syscall_get_set_args_cb()** in
**arch/ia64/kernel/ptrace.c**.

.. code-block::

    syscall_get_set_args_cb: krbs: 0xe000000103800ec0
    syscall_get_set_args_cb: ndirty: 0x0
    syscall_get_set_args_cb: count: 6
    syscall_get_set_args_cb: krbs[0]: 0x40a
    syscall_get_set_args_cb: krbs[1]: 0x2000000800122590
    syscall_get_set_args_cb: krbs[2]: 0xbad1fed1
    syscall_get_set_args_cb: krbs[3]: 0xbad2fed2
    syscall_get_set_args_cb: krbs[4]: 0xbad3fed3
    syscall_get_set_args_cb: krbs[5]: 0xbad4fed4
    syscall_get_set_args_cb:args[0]: 0x40a
    syscall_get_set_args_cb:args[1]: 0x2000000800122590
    syscall_get_set_args_cb:args[2]: 0xbad1fed1
    syscall_get_set_args_cb:args[3]: 0xbad2fed2
    syscall_get_set_args_cb:args[4]: 0xbad3fed3
    syscall_get_set_args_cb:args[5]: 0xbad4fed4

Here **krbs** is kernel's register backing store in memory.

**RBS** memory area (pointed at **ar.bsp** and **ar.bspstore** registers)
is where rotating registers are spilled on user's request or automatically.
In case of **ptrace()** registers are spilled on kernel's request with
**flushrs** instruction to make sure we can read them from memory.

Normally **krbs** should contain syscall arguments and maybe
local function variables. **args** array is actual syscall args.
We still see **0x402** (**__NR_read**) as first argument. That is probably
a manifestation of the bug we trace.

Fun fact: for some reason **ia64** linux syscalls start from 1024
(**0x400**). Perhaps lower numbers are reserved for binary
compatibility with **HPUX**?

bisecting the kernel
--------------------

I still did not get anything obvious. Why did syscall number kept
getting in the list? I decided to bisect the kernel.

I rebooted back to **4.9** kernel and made sure that bug disappeared.
Ok, that meant it's not at least some parallel userspace update.

I bisected the kernel down to the following commit:

.. code-block::

  commit 201766a20e30f982ccfe36bebfad9602c3ff574a
  Author: Elvira Khabirova <lineprinter@altlinux.org>
  Date:   Tue Jul 16 16:29:42 2019 -0700

    ptrace: add PTRACE_GET_SYSCALL_INFO request

    PTRACE_GET_SYSCALL_INFO is a generic ptrace API that lets ptracer obtain
    details of the syscall the tracee is blocked in.

   include/linux/tracehook.h                     |   9 ++++++---
   include/uapi/linux/ptrace.h                   |  35 +++++++++++++++++++++++++++++++++++
   kernel/ptrace.c                               | 101 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++-
   tools/testing/selftests/seccomp/seccomp_bpf.c |  13 +++++++++----
   4 files changed, 150 insertions(+), 8 deletions(-)

Tl;DR is: it's a new **ptrace()** API to fetch traced syscall arguments.

This means it's not strictly a regression of existing code:

- before the kernel change **strace** used to manually
  extract syscall arguments from **ptrace(PTRACE_GETREGS)**
  knowing the syscall ABI for an architecture
- after the change it calls **ptrace(PTRACE_GET_SYSCALL_INFO)** and gets
  all the arguments as a nice portable array. No need to deal with register
  numbers!

That looked even more confusing! Arch-specific **ptrace()** code was not changed
in that commit and yet some part of **ptrace()** is somehow broken.
It meant that something was probably broken forever.

What next? I had to fall back to debugging the issue from the first
principles: carefully trace syscall entry into the kernel to debug
**ptrace()** hook being called and check how syscall arguments are
extracted.

Sounds tedious but straightforward.

syscalls on ia64
----------------

So what IS the syscall ABI on **ia64**? I never looked into too much details
besides knowing that "it looks very close to calling a C function". Is it
true though?

Actually, **ia64** has not one but two close but different syscall ABIs:

- **break** instruction based kernel entry (similar to interrupt trap)
- **epc** magic instruction based (also called **fsys** mode, "fast syscall")

**break** ABI is roughly the following:

- syscall number is placed into **r15** (non-rotating register)
- syscall arguments go into rotating registers for output (not input or local)
- **break 0x10000** is executed to switch into kernel mode via
  **break** interrupt vector **ENTRY(break_fault)** (at arch/ia64/kernel/ivt.S)

Here argument layout for syscall matches argument layour for a standard C function
on **i64**. Only syscall number is passed via unusual **r15**.

**glibc** syscall wrapper fully illustrates typical call:

.. code-block::

  ENTRY(syscall)
        /* We are called like so:
           {out0,out1,...,out6} registers -> {NR, arg1, ..., arg6}
           Shift the register window so that {out1...out6} are available
           in {out0...out5} like the kernel syscall handler expects.  */
        alloc r2=ar.pfs,1,0,8,0
        mov r15=r32             /* syscall number */
        break __IA64_BREAK_SYSCALL
        ;;
        cmp.ne p6,p0=-1,r10     /* r10 = -1 on error */
  (p6)  ret
        br.cond.spnt.few __syscall_error

Not to go into too many details **syscall's** proto is **syscall(NR, arg1, arg2, arg3,...,arg8)**
and kernel handles it as **sys_NR(arg1,arg2,arg3,...,arg8)**.

At this point I thought, "aha! off-by-one!". But tracing through all the paths
of **ia64** assembly I was not able to find any problems. I had to read on stack
handling in branch calls and interrupts in excellent software development intel's
manual to make sure I don't miss any special cases. Nothing stood out.

Adding more debugging I realized **break** mechanism was not used at all
in failing cases!

After a bit of debugging I discovered that most of glibc syscalls are
actually done via **epc** (not **break**) mechanism! Gah! I never had
a chance to have a closer look at it and always assumed it's an unimplemented
feature. So much for being a local expert in **ia64-linux** :D

**epc** ABI is unusual: **epc** instruction (Enter Privileged Code)
itself does only one thing: it changes privileges of current execution from
userspace to kernel level and executes next instruction right after it.
This means no context switch, no traps executed. It's almost like a **nop**.
next few instructions after **epc** need to manually perform all the necessary
context operations. Which might be none for simplest syscalls like **getpid()**!

If userspace could call **epc** from anywhere that would be a good way
to negate any kernel protection. Thus CPU has a few restrictions: code page
with **epc** needs to be marked as privileged for MMU so kernel could provide
safe code that is not easy to turn into an arbitrry privilege escalation

In practice kernel provides such a page as part of **vDSO**. **linux** calls it
**GATE** page. **glibc** finds the **vDSO** out and uses it as syscall
implementation. Linux calls the whole ABI an **fsys** mechanism:
https://www.kernel.org/doc/html/latest/ia64/fsys.html

The **__kernel_syscall_via_epc()** kernel entry in https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/ia64/kernel/gate.S#n300
looks roughly like that:

.. code-block::

  GLOBAL_ENTRY(__kernel_syscall_via_epc)
    .prologue
    .altrp b6
    .body
    /*
     * Note: the kernel cannot assume that the first two instructions in this
     * bundle get executed.  The remaining code must be safe even if
     * they do not get executed.
     */
    adds r17=-1024,r15       // A
    mov r10=0                // A default to successful syscall execution
    epc                      // B causes split-issue
    ;;
    RSM_PSR_BE_I(r20, r22)   // M2 (5 cyc to srlz.d)
    LOAD_FSYSCALL_TABLE(r14) // X
    ...

the argument layout bug
-----------------------

Digging through **break** vs. **epc** mechanism I found that **ptrace()**
gets the syscall register frame in slightly different states:

for **break** the layout is:

.. code-block::

  outputs:
  - arg8
  - arg7
  - arg6
  - ...
  - arg1

  locals:
  <none>

  inputs:
  - NR

**epc**:

.. code-block::

  outputs:
  - arg8
  - arg7
  - arg6
  - ...
  - arg1

  locals:
  - NR

  inputs:
  <none>

Both are perfectly valid states prepared to pass arguments to callee function.
And both require a **br.call** instruction to rotate **outputs** to the **inputs**
of callee target.

In normal syscall handling case (without **ptrace()** attached) **br.call**
is called for passing the control to syscall handler written in C. All arguments
are present in their **r32,r33,...** locations.

In tracing case **ptrace()** is executed right before **br.call**. There we
need to inspect syscall arguments (and possibly modify on user's request)
in **outputs regs** part of the active register set.

The bug was in the fact that **ia64**-specific **ptrace()** code assumed that
locals are never present (as in **break** **glibc**'s wrapper case).

Once we know the problem the fix is easy: skip locals and always use **output**
regs when inspecting syscall arguments:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=0ceb1ace4a2778e34a5414e5349712ae4dc41d85

Tl;DR of a patch: **ia64** has a **cr.ifs** register that tells us how many
inputs, local and output regusters are used in current function (register's
value is maintained by **alloc** and **br.call** / **br.ret** instructions).
Before the change we collected both **locals** + **outputs**, after the change - only **outputs**.

Are we done?

the error handling bug
----------------------

After the above patch applied I ran the **strace** test suite again.
It was a lot healtier with just a few failures. This time the error was in
syscall exit code (also new addition) of **ptrace(PTRACE_SYSCALL_INFO_EXIT)**.
This time sign of **errno** error was wrong.

Here we have a chance to see how **ia64** syscalls return status back to userspace:
two fixed registers are used for that:

- **r10** for fail-or-not status
- **r8** for status-or-error.

Here is the original code to set and get the status:

.. code-block:: c

    static inline long syscall_get_error(struct task_struct *task,
                                         struct pt_regs *regs)
    {
            return regs->r10 == -1 ? regs->r8:0;
    }
    
    static inline long syscall_get_return_value(struct task_struct *task,
                                                struct pt_regs *regs)
    {
            return regs->r8;
    }
    
    static inline void syscall_set_return_value(struct task_struct *task,
                                                struct pt_regs *regs,
                                                int error, long val)
    {
            if (error) {
                    /* error < 0, but ia64 uses > 0 return value */
                    regs->r8 = -error;
                    regs->r10 = -1;
            } else {
                    regs->r8 = val;
                    regs->r10 = 0;
            }
    }

6 lines of code. Simple, eh? Can you spot the error?

Note how **syscall_get_error()** does not remove negation
added in **syscall_set_return_value()**.

The fix was obvious:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=61bf318eac2c13356f7bd1c6a05421ef504ccc8a

Surely done now?

signal handling bug
-------------------

A few more test failures revealed another fun bug: signal handlers return
wrong signal masks.

In this case failed tests complained about status of blocked signals
in tracing **rt_sigreturn()** syscall.

When signal is delivered to userspace processes memory stack gets an extra
struct that describes delivered signal and a bit of context (on any arch AFAIU):

.. code-block:: c

    struct sigframe {
        unsigned long arg0; /* signum */
        unsigned long arg1; /* siginfo pointer */
        unsigned long arg2; /* sigcontext pointer */
    
        void __user *handler; /* pointer to the plabel of the signal handler */
        struct siginfo info;
        struct sigcontext sc;
    };
    ...
    struct sigcontext {
    ...
        sigset_t sc_mask; /* signal mask to restore after handler returns */
    };

Somehow **sc_mask** had unexpected value. Was it incorrectly populated by kernel?

**ia64** has two userspace stacks:

- usual C memory stack (tracked by **r12** register), userspace usually
  keps there function-local buffers, variables that don't fit in registers, etc.
- register backing store stack (**ar.bsp** and friends point to it), this area is
  managed by CPU to maintain rotating registers contents on **br.call** / **br.ret**.
  It's actually quite hard to interpret it's contents even in steady state due to
  fancy alignment restrictions, boundary framing with NaT marking. It's memory
  is also not synchronized with CPUs view of that memory to speed things up. You
  almost never want to mess with it for something like passing a struct around.

When it's in **rt_sigreturn()** syscall return that is the simplest way to look at
sigframe on stack. Where stack is present as one of **struct ptrace_syscall_info**
fields:

.. code-block:: c

      struct ptrace_syscall_info {
          __u8 op;        /* Type of system call stop */
          __u32 arch;     /* AUDIT_ARCH_* value; see seccomp(2) */
          __u64 instruction_pointer; /* CPU instruction pointer */
          __u64 stack_pointer;    /* CPU stack pointer */
          union {
              struct {    /* op == PTRACE_SYSCALL_INFO_ENTRY */
                  __u64 nr;       /* System call number */
                  __u64 args[6];  /* System call arguments */
              } entry;
              struct {    /* op == PTRACE_SYSCALL_INFO_EXIT */
                  __s64 rval;     /* System call return value */
                  __u8 is_error;  /* System call error flag;
                                     Boolean: does rval contain
                                     an error value (-ERRCODE) or
                                     a nonerror return value? */
              } exit;
              struct {    /* op == PTRACE_SYSCALL_INFO_SECCOMP */
                  __u64 nr;       /* System call number */
                  __u64 args[6];  /* System call arguments */
                  __u32 ret_data; /* SECCOMP_RET_DATA portion
                                     of SECCOMP_RET_TRACE
                                     return value */
              } seccomp;
          };
      };

So which of two stacks should be present in **stack_pointer** field? **r12**
or **ar.bspstore**? As a result
our frame is something like that:

.. code-block::

    +-------
    | ... <registers for outer functions, their cfm, predicates>
    +-------
    | ... <- ar.bspstore
    +-------
    | ... <- ar.bsp
    | vvv grows down vvv
    |
    |
    | ^^^ grows up ^^^
    ~~~~~~~~
    | ... <- r12
    | <signal frame>
    | <16-bytes of scratch area>
    | <rest of memory stack>
    +-------

Kernel had to pick one and picked wrong **ar.bspstore**. It contains no
valid data at all. It consists of leftover values for previous register
flushes and loads.

As a result **ptrace()** looked at a part of register backing store to look up **rt_sigreturn()**.

Once this mismatch became clear the fix was obvious:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=7ad1e366167837daeb93d0bacb57dee820b0b898

By then strace testsuite passed all tests \o/

Can we cautiosly declare the endeavour done?

plot twist
----------

Well, strace test suite now started hanging whole box on almost every
testsuite run. Looks like before it did not manage to get that far to destroy
kernel's internal state. Example crash:

.. code-block::

    Unable to handle kernel NULL pointer dereference (address 0000000000000338)
    sock_filter-v-X[6171]: Oops 11012296146944 [18]
    Modules linked in: usb_storage e1000 acpi_ipmi ipmi_si ipmi_devintf ipmi_msghandler rtc_efi
    
    CPU: 0 PID: 6171 Comm: sock_filter-v-X Tainted: G    B D W         5.12.0-rc2-00003-g97669c51470e-dirty #85
    Hardware name: hp server rx3600                   , BIOS 04.03                                                            04/08/2008
    psr : 0000121008026010 ifs : 800000000000040b ip  : [<a00000010008d1f1>]    Tainted: G    B D W         (5.12.0-rc2-00003-g97669c51470e-dirty)
    ip is at ptrace_stop+0x2b1/0x860
    unat: 0000000000000000 pfs : 000000000000040b rsc : 0000000000000003
    rnat: 0000000000000000 bsps: 0000000000000000 pr  : 000000255aa66a15
    ldrs: 0000000000000000 ccv : 00000000fffffa92 fpsr: 0009804c0270033f
    csd : 0000000000000000 ssd : 0000000000000000
    b0  : a00000010008d1b0 b6  : a0000001008b1b20 b7  : a00000010000d010
    f6  : 000000000000000000000 f7  : 1003e8208208208208209
    f8  : 1003effffffffffffffea f9  : 1003e0000000000000033
    f10 : 1003e8208208208208209 f11 : 1003effffffffffffffe6
    r1  : a000000101906440 r2  : 0000000000000010 r3  : 0000000000000000
    r8  : 00000000b3a0d9d1 r9  : 00000000000059d0 r10 : 00000000b3a08001
    r11 : 0000000000000001 r12 : e00000010f2d5880 r13 : e00000010f2d0000
    r14 : a0000001015c8304 r15 : 00000000deaf1eed r16 : e00000010f2d0000
    r17 : e00000010f2d100c r18 : a000000101706e70 r19 : e00000010f2d0018
    r20 : 0000000000010289 r21 : e00000010f2d0450 r22 : 0000000000000000
    r23 : 0000000000000338 r24 : 000000000000b3a2 r25 : 000000000000b3a2
    r26 : e00000010f2d048c r27 : 0000000000010013 r28 : fffffffffff7ffff
    r29 : 0000000000120000 r30 : 0000000000000000 r31 : e00000010f2d100c
    
    Call Trace:
     [<a000000100014d10>] show_stack+0x90/0xc0
                                    sp=e00000010f2d54b0 bsp=e00000010f2d3738
     [<a000000100015410>] show_regs+0x6d0/0xa40
                                    sp=e00000010f2d5680 bsp=e00000010f2d36c8
     [<a0000001000285e0>] die+0x1e0/0x3c0
                                    sp=e00000010f2d56a0 bsp=e00000010f2d3688
     [<a00000010005b160>] ia64_do_page_fault+0x820/0xb80
                                    sp=e00000010f2d56a0 bsp=e00000010f2d35e8
     [<a00000010000ca00>] ia64_leave_kernel+0x0/0x270
                                    sp=e00000010f2d56b0 bsp=e00000010f2d35e8
     [<a00000010008d1f0>] ptrace_stop+0x2b0/0x860
                                    sp=e00000010f2d5880 bsp=e00000010f2d3590
     [<a00000010008d8a0>] ptrace_do_notify+0x100/0x120
                                    sp=e00000010f2d5880 bsp=e00000010f2d3560
     [<a00000010008d950>] ptrace_notify+0x90/0x1a0
                                    sp=e00000010f2d58c0 bsp=e00000010f2d3540
     [<a000000100073700>] do_exit+0x1540/0x1700
                                    sp=e00000010f2d58c0 bsp=e00000010f2d34c8
     [<a0000001000287b0>] die+0x3b0/0x3c0
                                    sp=e00000010f2d58d0 bsp=e00000010f2d3488

It's a **NULL** pointer dereference. How hard could it be to nail down and fix
(or at least workaround)?

To make the box less unstable I sprintled a few **if (p == NULL) { WARN_ON(1); return; }**
around. That allowed surviving a few strace testsuite runs in a row. Woohoo!

**guppy** was able to survive a few days and then crashed with even more dire
and inscrutable panic.

Before digging into more details I first synced to latest kernel git to ease
upstreaming things bit by bit and using Latest and Greatest code.

Surprisingly latest **linux.git** did not even boot.

One of failures Jens quickly fixed right after successful bisection related
to task-level flag handling:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=f5f4fc4649ae542b1a25670b17aaf3cbb6187acc

Another failure was use of atomics against unaligned **bool** struct field
in a **hpsa** disk driver, was also easy to fix with help of Don and others:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=02ec144292bc424a5800d45d4cb472c66e97c520

I was "lucky" to find those failures before another kernel release (or another
few years of negligence).

Latest **linux.git** was still crashing the box.

more kernel debugging
---------------------

As a next step I enabled everything I could find related to memory corruption
debugging in linux kernel:

- **CONFIG_VM_DEBUG**
- **page_poison=on**
- **init_on_alloc=1** **init_on_free=1**
- **page_owner=on**
- **hardened_usercopy=1**
- **memblock=debug**
- various slab debugs

As a result I got kernel to unbootable state /o\\.

I hoped boot failures were related to underlying problem I observed.
One of the annoyances was that kernel silently crashed and did not
print errors to BMC's serial output. I disabled most of debugging
flags back and left **page_poison=on** **init_on_alloc=1** **init_on_free=1**.

This allowed catching and fixing some minor warnings like:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=f2a419cf495f95cac49ea289318b833477e1a0e2.

After a while (about a day) I started seeing the reports of arbitrarily corrupted
memory:

.. code-block::

    pagealloc: memory corruption
    000000004a763954: 05 00 00 00 00 00 00 00 f8 b5 b0 ff ff 0f 00 60  ...............`
    00000000b3626ed1: 60 b7 b0 ff ff 0f 00 60 50 68 1c 00 08 00 00 20  `......`Ph.....
    00000000f59604da: 00 00 00 00 00 00 00 00 00 70 00 00 00 00 00 00  .........p......
    00000000345d9313: e3 c2 9b 14 00 00 00 00 aa aa aa aa aa aa aa aa  ................
    00000000d092c8b5: aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa  ................
    ...
    0000000088df4d5c: aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa  ................
    00000000f6e761a6: aa aa aa aa aa aa aa aa 45 78 63 65 65 64 65 64  ........Exceeded
    0000000000d45288: 20 4d 61 78 53 74 61 72 74 75 70 73 0d 0a 00 aa   MaxStartups....
    00000000c40693de: aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa  ................
    00000000cf8ee6dc: aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa aa  ................
    ...
    000000005fa7b069: aa aa aa aa aa aa aa aa 10 00 00 00              ............
    CPU: 1 PID: 25234 Comm: sshd Not tainted 5.12.0-rc2-00010-gd6be88a244a9-dirty #125
    Hardware name: hp server rx3600                   , BIOS 04.03                                                            04/08/2008
    
    Call Trace:
     [<a000000100015210>] show_stack+0x90/0xc0
     [<a000000101161760>] dump_stack+0x150/0x1c0
     [<a0000001003f17b0>] __kernel_unpoison_pages+0x3f0/0x400
     [<a0000001003c1dc0>] get_page_from_freelist+0x1460/0x2ca0
     [<a0000001003c6540>] __alloc_pages_nodemask+0x3c0/0x660
     [<a0000001003ecfd0>] alloc_pages_vma+0xb0/0x500
     [<a000000100375580>] wp_page_copy+0xe0/0x15e0
     [<a0000001003799b0>] do_wp_page+0x170/0xa00
     [<a00000010037e0e0>] __handle_mm_fault+0x1960/0x1fe0
     [<a00000010037ea70>] handle_mm_fault+0x310/0x4e0
     [<a00000010005da50>] ia64_do_page_fault+0x1f0/0xb80
     [<a00000010000ca00>] ia64_leave_kernel+0x0/0x270

Yay! Maybe that's it?

The backtrace tells us it's a page fault handling code faulting in a page that
used to be in page freelist, but it's already in a corruptted state: it should
be full of **aa** values, but it clearly has some unrelated data like
**Exceeded MaxStartups**.

I prepared for a deep dive into virtual memory management in **linux*:

- got basic understanding of page fault handling on **ia64**, **TLB** population
  (**VHPT**, **TR**, and **TC** registers management)
- got basic understanding of memory management layout on **ia64**: where linear
  mapping starts (aka "identity" + base offset), where vmalloc() starts it's
  address, how and when it gets freed
- got basic understanding of linux 3-4-5 level page tables are maintained
  and synced back to architecture-specific **TLB**.

And after much debugging I found that this corruption is a bug in debugging
mechanism /o\\

**init_on_alloc=1**, **init_on_free=1** and **page_poison=on** are in direct
conflict (and a bit of redundancy) with each other:

- redundancy: if you have freed a page on **init_on_free=1** system then memory
  page can be allocated without **memset(0)** even on **init_on_alloc=1** system
  because **init_on_free=1** already guarantees it!
- conflict: **init_on_free=1** does **memset(0)** while **page_poison=1** does **memset(aa)**.
  Which one has more priority?

The problem was that **init_on_alloc=1** + **init_on_free=1** + **debug_pagealloc=1**
led to page freeing with **memset(aa)** and allocation without any **memset()** at all.
This caused two problems:

- reports on corruption where it should not be
- return pages with garbage data to the system even if system requested **alloc_page(__GFP_ZERO)**

In our case **alloc_page(__GFP_ZERO)** was used for page table (**PTE**) allocations
and instead of returning **PTE** of no pages it was full of bits that looked like
pointers to other pages.

Once this conflict was understood it was easy to report the bug and fix
it with help of **mm** folks:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=9df65f522536719682bccd24245ff94db956256c

There are many nuances when exactly the bug could happen. For example architecture must
not support **CONFIG_ARCH_SUPPORTS_DEBUG_PAGEALLOC** (**x86_64** was not
affected by the bug). Otherwise different mechanisms kick in.

On a positive side a few month later I managed to fix a mirror image bug
on **x86_64**:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=69e5d322a2fb86173fde8bad26e8eb38cad1b1e9

I'll spare you the details why there is a complication of handling static keys
and early parameters in a way that it could go out of sync like that.

another yak: page_owner
-----------------------

Linux kernel has a very cool facility enabled by both **CONFIG_PAGE_OWNER=y**
and **page_owner=on** boot option. It's idea is to keep the history of most recent
callers who freed and who allocated the page including full backtrace and
page flags. You can see full state for each page in **/sys/kernel/debug/page_owner**:

.. code-block::

    # cat /sys/kernel/debug/page_owner
      ...
      Page allocated via order 0, mask 0x12cc0(GFP_KERNEL|__GFP_NOWARN|__GFP_NORETRY),
       pid 1, ts 651931156 ns, free_ts 0 ns
      PFN 1049310 type Unmovable Block 1024 type Unmovable Flags 0x8000000000000200(sl
      ab|zone=2)
       get_page_from_freelist+0xa31/0xcd0
       __alloc_pages+0x161/0x2b0
       allocate_slab+0x382/0x420
       ___slab_alloc.constprop.0+0x512/0x730
       __slab_alloc.constprop.0+0x90/0xc0
       kmem_cache_alloc+0x3f2/0x430
       kmem_cache_create_usercopy+0x13e/0x2e0
       kmem_cache_create+0x18/0x20
       khugepaged_init+0x20/0x61
       hugepage_init+0x84/0x131
       do_one_initcall+0x41/0x200
       kernel_init_freeable+0x18e/0x1d6
       kernel_init+0x16/0x110
       ret_from_fork+0x1f/0x30
     ...

Initially I hoped to use **page_owner** to dump at corruption detection time
and at random times when I want to see past page history manually (say,
at NULL-corruption detection time).

The problem was that setting **page_owner=on** rendered **ia64**
unbootable. It happened because on **ia64** stack unwinder requires memory
allocation (and gets into infinite recursion) while on other arches it's not
required.

Fun fact: actually storing stack trace for page owner itself does require
memory allocation as well (on any architecture). It could have a potential
getting into recursion. **page_owner** code  tried to prevent it by scanning
current backtrace for duplicate address entries.

I sidestepped **page_owner=on** recursion by storing single bit in currently running
task: https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=8e9b16c47680f6e7d6e5864a37f313f905a91cf5

It should be slightly faster now.

While I have such a nice corruption reporter I attempted to explore and fix
a few tools available to debug it efficintly:

- added **page_owner** info reporting on detected page corruption: https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=f58bd538e6a2deb2bcdfe527d9ed45643348a4e6
- fixed **page_owner=on** to work the same as **page_owner=1** (for consistency): https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=608b5d668c8ea6734594a401c9adab4093ad9847
- fixed ia64-specific symbolizer crash on function descriptors: https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=99e729bd40fb3272fa4b0140839d5e957b58588a

back to strace killer
---------------------

strace testsuite was still able to kill the box. Example crash looked like:

.. code-block::

    Unable to handle kernel paging request at virtual address aaaaaaaaaaaaaab2
    swapper/0[0]: Oops 8813272891392 [1]
    Modules linked in: acpi_ipmi e1000 usb_storage ipmi_si ipmi_devintf ipmi_msghandler rtc_efi
    
    CPU: 0 PID: 0 Comm: swapper/0 Not tainted 5.12.0-rc2-00010-gd6be88a244a9-dirty #129
    Hardware name: hp server rx3600                   , BIOS 04.03                                                            04/08/2008
    psr : 0000101008026010 ifs : 8000000000000307 ip  : [<a000000100e8e670>]    Not tainted (5.12.0-rc2-00010-gd6be88a244a9-dirty)
    ip is at sk_filter_release_rcu+0x70/0x120
    unat: 0000000000000000 pfs : 0000000000000895 rsc : 0000000000000003
    rnat: 0000000000000468 bsps: 0000000000001000 pr  : 01606a5694556a55
    ldrs: 0000000000000000 ccv : 000000001f020f81 fpsr: 0009804c0270033f
    csd : 0000000000000000 ssd : 0000000000000000
    b0  : a00000010018fd00 b6  : a000000100e8e600 b7  : e00000003f81c740
    f6  : 1003e000000000003ae7e f7  : 1003e0000000002813e10
    f8  : 1003e0019aba91335cd31 f9  : 1003e0019b4e1e1313911
    f10 : 1003e0000038521a57b7f f11 : 1003e0000000000000000
    r1  : a0000001019465c0 r2  : a000000200034004 r3  : 00000000aaaaaaaa
    r8  : aaaaaaaaaaaaaab2 r9  : a00000010174d0c0 r10 : ffffffffffa3cd70
    r11 : a00000010148bec8 r12 : a000000101607ba0 r13 : a000000101600000
    r14 : e000000116287718 r15 : a000000200034040 r16 : a000000100e8e600
    r17 : e00000010e6dfe80 r18 : a000000101607bc0 r19 : e000000005808fc0
    r20 : e00000010e6dfe80 r21 : 0000000000000000 r22 : e00000010e6dfe80
    r23 : e00000010e6dfe80 r24 : e000000005808f78 r25 : 0000000000001f04
    r26 : 00000000000cf92c r27 : 0000000000000007 r28 : a00000010174dba8
    r29 : 0000000000000007 r30 : 0000000000000007 r31 : 000000000000000a
        
    Call Trace:
     [<a000000100015210>] show_stack+0x90/0xc0
     [<a000000100015910>] show_regs+0x6d0/0xa40
     [<a000000100029420>] die+0x1e0/0x3c0
     [<a00000010005e370>] ia64_do_page_fault+0xb10/0xb80
     [<a00000010000ca00>] ia64_leave_kernel+0x0/0x270
     [<a000000100e8e670>] sk_filter_release_rcu+0x70/0x120
     [<a00000010018fd00>] rcu_core+0x8c0/0x1440
     [<a0000001001908a0>] rcu_core_si+0x20/0x40
     [<a000000101182cb0>] __do_softirq+0x230/0x670
     [<a000000100079d60>] irq_exit+0x180/0x220
     [<a000000100013a70>] ia64_handle_irq+0x1b0/0x360
     [<a00000010000ca00>] ia64_leave_kernel+0x0/0x270
     [<a0000001000143f0>] ia64_pal_call_static+0x90/0xc0
     [<a0000001000150c0>] ia64_pal_halt_light.isra.0+0x40/0x80
     [<a000000100016200>] arch_cpu_idle+0x100/0x1c0
     [<a0000001011818a0>] default_idle_call+0xe0/0x140
     [<a0000001000eb530>] do_idle+0x330/0x4e0
     [<a0000001000ebe30>] cpu_startup_entry+0x50/0x80
     [<a00000010116ded0>] rest_init+0x230/0x250
     [<a000000101490e70>] arch_call_rest_init+0x20/0x40
     [<a000000101491ad0>] start_kernel+0xbf0/0xc20
     [<a00000010116dc60>] start_ap+0x760/0x780
    Disabling lock debugging due to kernel taint
    Kernel panic - not syncing: Fatal exception
    ---[ end Kernel panic - not syncing: Fatal exception ]---

This time the victim is a **swapper** thread that happens to execute
deferred **sk_filter_release_rcu** execution: strace test allocated **sk_filter**
and someone else failed to free it. Virtual address **aaaaaaaaaaaaaab2** says
that it's probably an use-after-free case.

Should be simple to debug, right?

As I already spent A Lot of time spelunking through memory management in
**ia64** I dropped a bit of dead code around **DISCONTIGMEM**:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=9187592b96385e5060dfb2b182aa9ec93d5c0332

From now on **ia64** is just a **SPARSEMEM** architecture (or **FLATMEM** if
you are lucky to get contiguous physical address layout, I am not: **rx3600**
has 1TB gap for me).

I minimized **strace** killer example down to:

.. code-block:: c

    #include <unistd.h>
    #include <netinet/in.h>
    #include <sys/socket.h>
    #include <linux/filter.h>

    int main(void)
    {
        struct sock_filter bpf_filter[] = {
            BPF_STMT(BPF_RET|BPF_K, 0)
        };
        struct sock_fprog prog = {
            .len = 1,
            .filter = bpf_filter,
        };
        int fd = socket(AF_INET, SOCK_DGRAM, 0);
        setsockopt(fd, SOL_SOCKET, SO_ATTACH_FILTER, &prog, sizeof(prog));
        return 0;
    }

This crashes guppy in a second (usually takes 8 runs):

.. code-block::

    $ gcc bug.c -o bug; while ./bug; do echo again; done

The sample program creates IPv4 socket and attackes BPF program to it.
Kernel crashes at the cleanup time.

Socket filters are special in kernel because they use slightly different
virtual memory freeing policy compared to rest of vmalloc()'ed regions:
it's called **VM_FLUSH_RESET_PERMS**. This flag should eagerly unmap memory
and eagerly flush **TLB**. From my understanding it's a security feature
that slightly pessimizes performance and does not affect correctness
(modulo bugs we probably observe here).

To get **ia64** box into a fully stable state I ignore any **VM_FLUSH_RESET_PERMS**
mappings with the following hack:

.. code-block:: diff

    --- a/mm/vmalloc.c
    +++ b/mm/vmalloc.c
    @@ -2214,6 +2214,9 @@ static void vm_remove_mappings(struct vm_struct *area, int deallocate_pages)
    
        remove_vm_area(area->addr);
    
    +   /* workaround mysterious double-free on vmalloc() for bpf. */
    +   return;
    +
        /* If this is not VM_FLUSH_RESET_PERMS memory, no need for the below. */
        if (!flush_reset)
            return;

Unfortunately I don't know yet why crash happens and can only speculate at
this point. I suspect that **mm** code lacks a barrier somewhere that allows
page reuse before **TLB** flush happens.

To be continued.

Parting Words
-------------

**strace** has a great test suite to detect all sorts of corner cases in linux kernel.

Random factoids:

- **ia64** linux syscalls start from **1024**.
- **ia64** got even better **ptrace()** support.
- **page_owner=on** is now usable on **ia64**!
- I still did not get to the bottom of it. But it feels I'm very close :)
- **VM_FLUSH_RESET_PERMS** is a thing.
- It took me about 2 months to get some progress on this problem.
- It took **guppy** about 350 reboots to recover from machine lockups. I suspect it's more
  than this machine ever saw in it's previous life.
- Debugging tools can corrupt your data sometimes even if original setup is not supposed to.

Have fun!
