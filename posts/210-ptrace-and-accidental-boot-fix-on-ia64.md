---
title: "ptrace() and accidental boot fix on ia64"
date: August 04, 2018
---

This story is another dive into `linux` kernel internals. It has started
as a `strace` hangup on `ia64` and ended up being an unusual case of
`gcc` generating garbage code for `linux` kernel (not perfectly valid `c`
either). I'll try to cover a few `ptrace()` system call corners on
`x86_64` and `ia64` for comparison.

## Intro

I updated
[`elilo`](/posts/207-linker-script-weird-tricks-or-EFI-on-ia64.html)
and `kernel` on `ia64` machine recently.
Kernel boot times shrunk from 10 minutes (kernel `3.14.14`) down to 2
minutes (kernel `4.9.72`). `3.14.14` kernel had large 8-minute pause
when early console was not accessible. Every time this pause happened I
thought I bricked the machine. And now delays are gone \\o/
One extra thing broke (so far): every time I ran `strace` it was hanging
without any output printed. Mike Frysinger pointed out `strace` hangup
likely related to `gdb` problems on `ia64` [reported
before](https://bugs.gentoo.org/518130) by Émeric Maschino.

And he was right!

## Reproducing

Using [`ski` image](/posts/199-ia64-machine-emulation.html) I booted fresh
kernel to make sure the bug was still there:

``` 
# strace ls
<no response, hangup>
```

Yay! `ski` was able reproduce it: no need to torture physical machine
while debugging. Next step was to find where `strace` got stuck. As
`strace` and `gdb` are broken I had to resort to `printf()`
debugging.
Before doing that I tried `strace`s `-d` option to enable debug
mode where it prints everything it expects from traced process:

``` 
root@ia64 / # strace -d ls
strace: ptrace_setoptions = 0x51
strace: new tcb for pid 52, active tcbs:1
strace: [wait(0x80137f) = 52] WIFSTOPPED,sig=SIGSTOP,EVENT_STOP (128)
strace: pid 52 has TCB_STARTUP, initializing it
strace: [wait(0x80057f) = 52] WIFSTOPPED,sig=SIGTRAP,EVENT_STOP (128)
strace: [wait(0x00127f) = 52] WIFSTOPPED,sig=SIGCONT
strace: [wait(0x00857f) = 52] WIFSTOPPED,sig=133
????
```

Cryptic output. I tried to compare this output against correctly working
`x86_64` system to understand what went wrong:

``` 
amd64 $ strace -d ls
strace: ptrace_setoptions = 0x51
strace: new tcb for pid 29343, active tcbs:1
strace: [wait(0x80137f) = 29343] WIFSTOPPED,sig=SIGSTOP,EVENT_STOP (128)
strace: pid 29343 has TCB_STARTUP, initializing it
strace: [wait(0x80057f) = 29343] WIFSTOPPED,sig=SIGTRAP,EVENT_STOP (128)
strace: [wait(0x00127f) = 29343] WIFSTOPPED,sig=SIGCONT
strace: [wait(0x00857f) = 29343] WIFSTOPPED,sig=133
execve("/bin/ls", ["ls"], 0x60000fffffa4f1f8 /* 36 vars */strace: [wait(0x04057f) = 29343] WIFSTOPPED,sig=SIGTRAP,EVENT_EXEC (4)
strace: [wait(0x00857f) = 29343] WIFSTOPPED,sig=133
...
```

Up to `execve` call both logs are identical. Still no clue.
I spent some time looking at `ptrace` state machine in kernel and gave up
trying to understand what was wrong. I then asked [`strace`
maintainer](https://github.com/strace/strace/issues/33) on what could be
wrong and got an almost immediate response from Dmitry V. Levin:
`strace` did not show actual error.

After a source code tweak he pointed at `ptrace()` syscall failure
returning `-EIO`:

``` 
$ ./strace -d /
./strace: ptrace_setoptions = 0x51
./strace: new tcb for pid 11080, active tcbs:1
./strace: [wait(0x80137f) = 11080] WIFSTOPPED,sig=SIGSTOP,EVENT_STOP (128)
./strace: pid 11080 has TCB_STARTUP, initializing it
./strace: [wait(0x80057f) = 11080] WIFSTOPPED,sig=SIGTRAP,EVENT_STOP (128)
./strace: [wait(0x00127f) = 11080] WIFSTOPPED,sig=SIGCONT
./strace: [wait(0x00857f) = 11080] WIFSTOPPED,sig=133
./strace: get_regs: get_regs_error: Input/output error
????
...
"Looks like ptrace(PTRACE_GETREGS) always fails with EIO on this new kernel."
```

Now I got a more specific signal: `ptrace(PTRACE_GETREGS, ...)`
syscall failed.

## Into the kernel

I felt I had finally found the smoking gun: getting registers of
`WIFSTOPPED` traced task should never fail. All registers must be
already stored somewhere in memory.
Otherwise how would kernel be able to resume executing traced task when
needed?
Before diving into `ia64` land let's look into `x86_64`
`ptrace(PTRACE_GETREGS, ...)` implementation.

### `x86_64` `ptrace(PTRACE_GETREGS)`

To find a `<foo>` syscall implementation in kernel we can search for
`sys_<foo>()` function definition. The lazy way to find a
definition is to interrogate built kernel with `gdb`:

``` 
$ gdb --quiet ./vmlinux
(gdb) list sys_ptrace
1105
1106    #ifndef arch_ptrace_attach
1107    #define arch_ptrace_attach(child)       do { } while (0)
1108    #endif
1109
1110    SYSCALL_DEFINE4(ptrace, long, request, long, pid, unsigned long, addr,
1111                    unsigned long, data)
1112    {
1113            struct task_struct *child;
1114            long ret;
```

`SYSCALL_DEFINE4(ptrace, ...)` macro defines actual
[`sys_ptrace()`](http://lxr.linux.no/#linux+v4.15.13/kernel/ptrace.c#L1121)
which does a few sanity checks and dispatches to **arch_ptrace()**:

``` c
SYSCALL_DEFINE4(ptrace, long, request, long, pid, unsigned long, addr,
                unsigned long, data)
{
    // simplified a bit
    struct task_struct *child;
    long ret;

    child = ptrace_get_task_struct(pid);
    ret = arch_ptrace(child, request, addr, data);
    return ret;
}
```

`x86_64` implementation [does `copy_regset_to_user()`
call](http://lxr.linux.no/#linux+v4.15.13/arch/x86/kernel/ptrace.c#L809)
and takes a few lines of code to fetch registers:

``` c
long arch_ptrace(struct task_struct *child, long request,
                 unsigned long addr, unsigned long data) {
   // ...
   case PTRACE_GETREGS:    /* Get all gp regs from the child. */
       return copy_regset_to_user(child,
                                  task_user_regset_view(current),
                                  REGSET_GENERAL,
                                  0, sizeof(struct user_regs_struct),
                                  datap);
```

Let's look at it in detail to get the idea where registers are normally
stored.

``` c
static inline int copy_regset_to_user(struct task_struct *target,
                                      const struct user_regset_view *view,
                                      unsigned int setno,
                                      unsigned int offset, unsigned int size,
                                      void __user *data)
{
    const struct user_regset *regset = &view->regsets[setno];

    if (!regset->get)
            return -EOPNOTSUPP;

    if (!access_ok(VERIFY_WRITE, data, size))
            return -EFAULT;

    return regset->get(target, regset, offset, size, NULL, data);
}
```

Here `copy_regset_to_user()` is just a dispatcher to `view`
argument. Moving on:

``` c
const struct user_regset_view *task_user_regset_view(struct task_struct *task)
{
    // simplified #ifdef-ery
    if (!user_64bit_mode(task_pt_regs(task)))
        return &user_x86_32_view;

    return &user_x86_64_view;
}
// ...
static const struct user_regset_view user_x86_64_view = {
    .name = "x86_64", .e_machine = EM_X86_64,
    .regsets = x86_64_regsets, .n = ARRAY_SIZE(x86_64_regsets)
};
// ...
static struct user_regset x86_64_regsets[] __ro_after_init = {
    [REGSET_GENERAL] = {
        .core_note_type = NT_PRSTATUS,
        .n = sizeof(struct user_regs_struct) / sizeof(long),
        .size = sizeof(long), .align = sizeof(long),
        .get = genregs_get, .set = genregs_set
    },
    // ...
```

A bit of boilerplate to tie `genregs_get()` and `genregs_set()` to
64-bit (or 32-bit) caller. Let's look at 64-bit variant of
`genregs_get()` as it's used in our `PTRACE_GETREGS` case:

``` c
static int genregs_get(struct task_struct *target,
                       const struct user_regset *regset,
                       unsigned int pos, unsigned int count,
                       void *kbuf, void __user *ubuf)
{
    if (kbuf) {
        unsigned long *k = kbuf;
        while (count >= sizeof(*k)) {
            *k++ = getreg(target, pos);
        count -= sizeof(*k);
        pos += sizeof(*k);
        }
    } else {
        unsigned long __user *u = ubuf;
        while (count >= sizeof(*u)) {
            if (__put_user(getreg(target, pos), u++))
                return -EFAULT;
            count -= sizeof(*u);
            pos += sizeof(*u);
        }
    }

    return 0;
}
// ...
static unsigned long getreg(struct task_struct *task, unsigned long offset)
{
    // ... simplified
    return *pt_regs_access(task_pt_regs(task), offset);
}
static unsigned long *pt_regs_access(struct pt_regs *regs, unsigned long regno)
{
    BUILD_BUG_ON(offsetof(struct pt_regs, bx) != 0);
    return &regs->bx + (regno >> 2);
}
// ..
#define task_pt_regs(task) \
({                                                                  \
    unsigned long __ptr = (unsigned long)task_stack_page(task);     \
    __ptr += THREAD_SIZE - TOP_OF_KERNEL_STACK_PADDING;             \
    ((struct pt_regs *)__ptr) - 1;                                  \
})
static inline void *task_stack_page(const struct task_struct *task)
{
    return task->stack;
}
```

From `task_pt_regs()` definition we see that actual register contents
is stored in task's kernel stack. And `genregs_get()` copies register
contents one by one in a `while()` loop.

How do task's registers get stored to task's kernel stack? There are a
few paths to get there. Most frequent is perhaps interrupt handling when
task is unscheduled from CPU and is moved to scheduler wait queue.

[`ENTRY(interrupt_entry)`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/x86/entry/entry_64.S?h=v4.18-rc7#n546):
is an entry point for interrupt handling.

``` asm
ENTRY(interrupt_entry)
    UNWIND_HINT_FUNC
    ASM_CLAC
    cld

    testb        $3, CS-ORIG_RAX+8(%rsp)
    jz        1f
    SWAPGS

    /*
     * Switch to the thread stack. The IRET frame and orig_ax are
     * on the stack, as well as the return address. RDI..R12 are
     * not (yet) on the stack and space has not (yet) been
     * allocated for them.
     */
    pushq        %rdi

    /* Need to switch before accessing the thread stack. */
    SWITCH_TO_KERNEL_CR3 scratch_reg=%rdi
    movq        %rsp, %rdi
    movq        PER_CPU_VAR(cpu_current_top_of_stack), %rsp

     /*
      * We have RDI, return address, and orig_ax on the stack on
      * top of the IRET frame. That means offset=24
      */
    UNWIND_HINT_IRET_REGS base=%rdi offset=24

    pushq        7*8(%rdi)                /* regs->ss */
    pushq        6*8(%rdi)                /* regs->rsp */
    pushq        5*8(%rdi)                /* regs->eflags */
    pushq        4*8(%rdi)                /* regs->cs */
    pushq        3*8(%rdi)                /* regs->ip */
    pushq        2*8(%rdi)                /* regs->orig_ax */
    pushq        8(%rdi)                        /* return address */
    UNWIND_HINT_FUNC

    movq        (%rdi), %rdi
1:

    PUSH_AND_CLEAR_REGS save_ret=1
    ENCODE_FRAME_POINTER 8

    testb        $3, CS+8(%rsp)
    jz        1f

    /*
     * IRQ from user mode.
     *
     * We need to tell lockdep that IRQs are off.  We can't do this until
     * we fix gsbase, and we should do it before enter_from_user_mode
     * (which can take locks).  Since TRACE_IRQS_OFF is idempotent,
     * the simplest way to handle it is to just call it twice if
     * we enter from user mode.  There's no reason to optimize this since
     * TRACE_IRQS_OFF is a no-op if lockdep is off.
     */
    TRACE_IRQS_OFF

    CALL_enter_from_user_mode

1:
    ENTER_IRQ_STACK old_rsp=%rdi save_ret=1
    /* We entered an interrupt context - irqs are off: */
    TRACE_IRQS_OFF

    ret
END(interrupt_entry)
; ...
.macro PUSH_AND_CLEAR_REGS rdx=%rdx rax=%rax save_ret=0
    /*
     * Push registers and sanitize registers of values that a
     * speculation attack might otherwise want to exploit. The
     * lower registers are likely clobbered well before they
     * could be put to use in a speculative execution gadget.
     * Interleave XOR with PUSH for better uop scheduling:
     */
    .if \save_ret
    pushq        %rsi                /* pt_regs->si */
    movq        8(%rsp), %rsi        /* temporarily store the return address in %rsi */
    movq        %rdi, 8(%rsp)        /* pt_regs->di (overwriting original return address) */
    .else
    pushq   %rdi                /* pt_regs->di */
    pushq   %rsi                /* pt_regs->si */
    .endif
    pushq        \rdx                /* pt_regs->dx */
    xorl        %edx, %edx        /* nospec   dx */
    pushq   %rcx                /* pt_regs->cx */
    xorl        %ecx, %ecx        /* nospec   cx */
    pushq   \rax                /* pt_regs->ax */
    pushq   %r8                /* pt_regs->r8 */
    xorl        %r8d, %r8d        /* nospec   r8 */
    pushq   %r9                /* pt_regs->r9 */
    xorl        %r9d, %r9d        /* nospec   r9 */
    pushq   %r10                /* pt_regs->r10 */
    xorl        %r10d, %r10d        /* nospec   r10 */
    pushq   %r11                /* pt_regs->r11 */
    xorl        %r11d, %r11d        /* nospec   r11*/
    pushq        %rbx                /* pt_regs->rbx */
    xorl    %ebx, %ebx        /* nospec   rbx*/
    pushq        %rbp                /* pt_regs->rbp */
    xorl    %ebp, %ebp        /* nospec   rbp*/
    pushq        %r12                /* pt_regs->r12 */
    xorl        %r12d, %r12d        /* nospec   r12*/
    pushq        %r13                /* pt_regs->r13 */
    xorl        %r13d, %r13d        /* nospec   r13*/
    pushq        %r14                /* pt_regs->r14 */
    xorl        %r14d, %r14d        /* nospec   r14*/
    pushq        %r15                /* pt_regs->r15 */
    xorl        %r15d, %r15d        /* nospec   r15*/
    UNWIND_HINT_REGS
    .if \save_ret
    pushq        %rsi                /* return address on top of stack */
    .endif
.endm
```

Interesting effects of the `interrupt_entry` are:

- registers are backed up by `PUSH_AND_CLEAR_REGS` macro
- memory area used for backup is
  `PER_CPU_VAR(cpu_current_top_of_stack)` (task's kernel stack)

To recap: `ptrace(PTRACE_GETREGS, ...)` does elementwise copy (using
`__put_user()`) for each general register located in a single
`struct pt_regs` in task's kernel stack to tracer's userspace.

Now let's look at how `ia64` does the same.

## `ia64` `ptrace(PTRACE_GETREGS)`

"Can't be much more complicated than on x86_64" was my thought. Haha.

I started searching for `-EIO` failure in kernel and sprinkling
`printk()` statements in `ptrace()` handling code.
`ia64` begins with the same call path as `x86_64`:

- `ptrace()` entry point:
  [`SYSCALL_DEFINE4(ptrace...`](http://lxr.linux.no/#linux+v4.15.13/kernel/ptrace.c#L1121)
- `ia64`-specific `arch_ptrace()` handler:
  [`arch_ptrace(...`](http://lxr.linux.no/#linux+v4.15.13/arch/ia64/kernel/ptrace.c#L1158)
- [`ptrace_getregs(...`](http://lxr.linux.no/#linux+v4.15.13/arch/ia64/kernel/ptrace.c#L828)

Again, `ptrace_getregs()` is supposed to copy in-memory context back
to caller's userspace. Where did it return `EIO`?

**Quiz**: while you are skimming through the `ptrace_getregs()` code
and comments right below, try to guess which `EIO` exit path is taken
in our case. I've marked the cases with `[N]` numbers.

``` c
static long
ptrace_getregs (struct task_struct *child, struct pt_all_user_regs __user *ppr)
{
    // ...
    // [1] check if we can write back to userspace
    if (!access_ok(VERIFY_WRITE, ppr, sizeof(struct pt_all_user_regs)))
            return -EIO;

    // [2] get pointer to register context (ok)
    pt = task_pt_regs(child);
    // [3] and tracee kernel stack (unexpected!)
    sw = (struct switch_stack *) (child->thread.ksp + 16);

    // [4] Try to unwind tracee's call chain (even more unexpected!)
    unw_init_from_blocked_task(&info, child);
    if (unw_unwind_to_user(&info) < 0) {
            return -EIO;
    }

    // [5] validate alignment of target userspace buffer
    if (((unsigned long) ppr & 0x7) != 0) {
            dprintk("ptrace:unaligned register address %p\n", ppr);
            return -EIO;
    }

    // [6] fetch special registers into local variables
    if (access_uarea(child, PT_CR_IPSR, &psr, 0) < 0
        || access_uarea(child, PT_AR_EC, &ec, 0) < 0
        || access_uarea(child, PT_AR_LC, &lc, 0) < 0
        || access_uarea(child, PT_AR_RNAT, &rnat, 0) < 0
        || access_uarea(child, PT_AR_BSP, &bsp, 0) < 0
        || access_uarea(child, PT_CFM, &cfm, 0)
        || access_uarea(child, PT_NAT_BITS, &nat_bits, 0))
            return -EIO;

    /* control regs */

    // [7] Finally start populating reguster contents into userspace:
    retval |= __put_user(pt->cr_iip, &ppr->cr_iip);
    retval |= __put_user(psr, &ppr->cr_ipsr);

    /* app regs */
    // [8] a few application registers
    retval |= __put_user(pt->ar_pfs, &ppr->ar[PT_AUR_PFS]);
    retval |= __put_user(pt->ar_rsc, &ppr->ar[PT_AUR_RSC]);
    retval |= __put_user(pt->ar_bspstore, &ppr->ar[PT_AUR_BSPSTORE]);
    retval |= __put_user(pt->ar_unat, &ppr->ar[PT_AUR_UNAT]);
    retval |= __put_user(pt->ar_ccv, &ppr->ar[PT_AUR_CCV]);
    retval |= __put_user(pt->ar_fpsr, &ppr->ar[PT_AUR_FPSR]);

    retval |= __put_user(ec, &ppr->ar[PT_AUR_EC]);
    retval |= __put_user(lc, &ppr->ar[PT_AUR_LC]);
    retval |= __put_user(rnat, &ppr->ar[PT_AUR_RNAT]);
    retval |= __put_user(bsp, &ppr->ar[PT_AUR_BSP]);
    retval |= __put_user(cfm, &ppr->cfm);

    /* gr1-gr3 */
    // [9] normal (general) registers
    retval |= __copy_to_user(&ppr->gr[1], &pt->r1, sizeof(long));
    retval |= __copy_to_user(&ppr->gr[2], &pt->r2, sizeof(long) *2);

    /* gr4-gr7 */
    // [10] more normal (general) registers!
    for (i = 4; i < 8; i++) {
            if (unw_access_gr(&info, i, &val, &nat, 0) < 0)
                    return -EIO;
            retval |= __put_user(val, &ppr->gr[i]);
    }

    /* gr8-gr11 */
    // [11] even more normal (general) registers!!
    retval |= __copy_to_user(&ppr->gr[8], &pt->r8, sizeof(long) * 4);

    /* gr12-gr15 */
    // [11] you've got the idea
    retval |= __copy_to_user(&ppr->gr[12], &pt->r12, sizeof(long) * 2);
    retval |= __copy_to_user(&ppr->gr[14], &pt->r14, sizeof(long));
    retval |= __copy_to_user(&ppr->gr[15], &pt->r15, sizeof(long));

    /* gr16-gr31 */
    // [12] even more of those
    retval |= __copy_to_user(&ppr->gr[16], &pt->r16, sizeof(long) * 16);

    /* b0 */
    // [13] branch register b0
    retval |= __put_user(pt->b0, &ppr->br[0]);

    /* b1-b5 */
    // [13] more branch registers
    for (i = 1; i < 6; i++) {
            if (unw_access_br(&info, i, &val, 0) < 0)
                    return -EIO;
            __put_user(val, &ppr->br[i]);
    }

    /* b6-b7 */
    // [14] even more branch registers
    retval |= __put_user(pt->b6, &ppr->br[6]);
    retval |= __put_user(pt->b7, &ppr->br[7]);

    /* fr2-fr5 */
    // [15] floating point registers
    for (i = 2; i < 6; i++) {
            if (unw_get_fr(&info, i, &fpval) < 0)
                    return -EIO;
            retval |= __copy_to_user(&ppr->fr[i], &fpval, sizeof (fpval));
    }

    /* fr6-fr11 */
    // [16] more floating point registers
    retval |= __copy_to_user(&ppr->fr[6], &pt->f6,
                             sizeof(struct ia64_fpreg) * 6);

    /* fp scratch regs(12-15) */
    // [17] more floating point registers
    retval |= __copy_to_user(&ppr->fr[12], &sw->f12,
                             sizeof(struct ia64_fpreg) * 4);

    /* fr16-fr31 */
    // [18] even more floating point registers
    for (i = 16; i < 32; i++) {
            if (unw_get_fr(&info, i, &fpval) < 0)
                    return -EIO;
            retval |= __copy_to_user(&ppr->fr[i], &fpval, sizeof (fpval));
    }

    /* fph */
    // [19] rest of floating point registers
    ia64_flush_fph(child);
    retval |= __copy_to_user(&ppr->fr[32], &child->thread.fph,
                             sizeof(ppr->fr[32]) * 96);

    /*  preds */
    // [20] predicate registers
    retval |= __put_user(pt->pr, &ppr->pr);

    /* nat bits */
    // [20] NaT status registers
    retval |= __put_user(nat_bits, &ppr->nat);

    ret = retval ? -EIO : 0;
    return ret;
}
```

It's a huge function. Be afraid not! It has two main parts:

- extraction of register values using `unw_unwind_to_user()`
- copying extracted values to caller's userspace using
  `__put_user()` and `__copy_to_user()` helpers.

Those two are analogous to `x86_64**` `copy_regset_to_user()`
implementation.

**Quiz answer**: surprisingly it's case `[4]`: `EIO` popped up
due to a failure in `unw_unwind_to_user()` call. Or not so
surprisingly given it's The Function to fetch register values from
somewhere.

Let's check where register contents are hiding on `ia64`. Here goes
[`unw_unwind_to_user()`
definition](http://lxr.linux.no/#linux+v4.15.13/arch/ia64/kernel/unwind.c#L1970):

``` c
int
unw_unwind_to_user (struct unw_frame_info *info)
{
        unsigned long ip, sp, pr = info->pr;

        do {
                unw_get_sp(info, &sp);
                if ((long)((unsigned long)info->task + IA64_STK_OFFSET - sp)
                    < IA64_PT_REGS_SIZE) {
                        UNW_DPRINT(0, "unwind.%s: ran off the top of the kernel stack\n",
                                   __func__);
                        break;
                }
                if (unw_is_intr_frame(info) &&
                    (pr & (1UL << PRED_USER_STACK)))
                        return 0;
                if (unw_get_pr (info, &pr) < 0) {
                        unw_get_rp(info, &ip);
                        UNW_DPRINT(0, "unwind.%s: failed to read "
                                   "predicate register (ip=0x%lx)\n",
                                __func__, ip);
                        return -1;
                }
        } while (unw_unwind(info) >= 0);
        unw_get_ip(info, &ip);
        UNW_DPRINT(0, "unwind.%s: failed to unwind to user-level (ip=0x%lx)\n",
                   __func__, ip);
        return -1;
}
EXPORT_SYMBOL(unw_unwind_to_user);
```

The code above is more complicated than on `x86_64`. How is it
supposed to work?
For efficiency reasons syscall interface (and even interrupt handling
interface) on `ia64` looks a lot more like normal function call. This
means that `linux` does not store all general registers to a separate
`struct pt_regs` backup area for each task switch.
Let's peek at interrupt handling entry for completeness.
`ia64` uses `interrupt` entry point to enter the kernel at
[`ENTRY(interrupt)`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/ia64/kernel/ivt.S?h=v4.18-rc7#n878):

``` asm
ENTRY(interrupt)
    /* interrupt handler has become too big to fit this area. */
    br.sptk.many __interrupt
END(interrupt)
// ...
ENTRY(__interrupt)
    DBG_FAULT(12)
    mov r31=pr                  // prepare to save predicates
    ;;
    SAVE_MIN_WITH_COVER         // uses r31; defines r2 and r3
    SSM_PSR_IC_AND_DEFAULT_BITS_AND_SRLZ_I(r3, r14)
                                // ensure everybody knows psr.ic is back on
    adds r3=8,r2                // set up second base pointer for SAVE_REST
    ;;
    SAVE_REST
    ;;
    MCA_RECOVER_RANGE(interrupt)
    alloc r14=ar.pfs,0,0,2,0 // must be first in an insn group
    MOV_FROM_IVR(out0, r8)        // pass cr.ivr as first arg
    add out1=16,sp                // pass pointer to pt_regs as second arg
    ;;
    srlz.d                        // make sure we see the effect of cr.ivr
    movl r14=ia64_leave_kernel
    ;;
    mov rp=r14
    br.call.sptk.many b6=ia64_handle_irq
END(__interrupt)
```

The code above handles interrupts as:

- `SAVE_MIN_WITH_COVER` sets kernel stack (`r12`), `gp` (`r1`) and
  so on
- `SAVE_REST` stores rest of registers `r2` to `r31` but leaves
  `r32` to `r127` be managed by `RSE` (register stack engine) like
  normal function call would.
- Hands off control to C code in `ia64_handle_irq`.

All the above means that in order to get register `r32` or similar we
would need to perform stack kernel unwinding down to the user space
boundary and read register values from `RSE` memory area (backing
store).

## Into the rabbit hole

Back to our unwinder failure.

Our case is not very complicated as tracee is stopped at system call
boundary and there is not too much to unwind. How one would know where
user boundary starts? `linux` looks at return instruction pointer in
every stack frame and checks if it's return address still points to
kernel address space.

Unwinding failure seemingly happens in depths of [`unw_unwind(info,
&ip)`](http://lxr.linux.no/#linux+v4.15.13/arch/ia64/kernel/unwind.c#L1883).
From there
[`find_save_locs(info);`](http://lxr.linux.no/#linux+v4.15.13/arch/ia64/kernel/unwind.c#L1834)
is called. `find_save_locs()` lazily builds or runs an unwind script.
The
[`run_script()`](http://lxr.linux.no/#linux+v4.15.13/arch/ia64/kernel/unwind.c#L1714)
is a small byte code interpreter of 11 instruction types.
If the above does not make sense to you it's fine. It did not make
sense to me either.
To get more information from unwinder I enabled debugging output for
unwinder by adding `#define UNW_DEBUG`:

``` diff
--- a/arch/ia64/kernel/unwind.c
+++ b/arch/ia64/kernel/unwind.c
    @@ -56,4 +56,6 @@
 #define UNW_STATS      0       /* WARNING: this disabled interrupts for long time-spans!! */

+#define UNW_DEBUG 1
+
 #ifdef UNW_DEBUG
   static unsigned int unw_debug_level = UNW_DEBUG;
```

I ran `strace` again:

``` 
ia64 # strace -v -d ls
strace: ptrace_setoptions = 0x51
unwind.build_script: no unwind info for ip=0xa00000010001c1a0 (prev ip=0x0)
unwind.run_script: no state->pt, dst=18, val=136
unwind.unw_unwind: failed to locate return link (ip=0xa00000010001c1a0)!
unwind.unw_unwind_to_user: failed to unwind to user-level (ip=0xa00000010001c1a0)
```

`build_script()` couldn't resolve current `ip=0xa00000010001c1a0`
address. Why? No idea! I added `printk()` around the place where I
expected a match:

``` diff
--- a/arch/ia64/kernel/unwind.c
+++ b/arch/ia64/kernel/unwind.c
@@ -1562,6 +1564,8 @@ build_script (struct unw_frame_info *info)

        prev = NULL;
        for (table = unw.tables; table; table = table->next) {
+               UNW_DPRINT(0, "unwind.%s: looking up ip=%#lx in [start=%#lx,end=%#lx)\n",
+                          __func__, ip, table->start, table->end);
                if (ip >= table->start && ip < table->end) {
                        /*
                         * Leave the kernel unwind table at the very front,
```

I ran `strace` again:

``` 
ia64 # strace -v -d ls
strace: ptrace_setoptions = 0x51
unwind.build_script: looking up ip=0xa00000010001c1a0 in [start=0xa000000100009240,end=0xa000000100000000)
unwind.build_script: looking up ip=0xa00000010001c1a0 in [start=0xa000000000040720,end=0xa000000000040ad0)
unwind.build_script: no unwind info for ip=0xa00000010001c1a0 (prev ip=0x0)
```

Can you spot the problem? Look at this range:
`[start=0xa000000100009240,end=0xa000000100000000)`. It's `end` is
less than `start`. This renders `table->start && ip < table->end`
condition to be always false. How could it happen?
It means the `ptrace()` itself is not at fault here but a victim of
already corrupted `table->end` value.

## Going deeper

To find `table->end` corruption I checked if `table` was populated
correctly. It is done by a simple function `init_unwind_table()`:

``` c
static void
init_unwind_table (struct unw_table *table, const char *name, unsigned long segment_base,
                   unsigned long gp, const void *table_start, const void *table_end)
{
    const struct unw_table_entry *start = table_start, *end = table_end;

    table->name = name;
    table->segment_base = segment_base;
    table->gp = gp;
    table->start = segment_base + start[0].start_offset;
    table->end = segment_base + end[-1].end_offset;
    table->array = start;
    table->length = end - start;
}
```

Table construction happens in only a few places:

``` c
void __init
unw_init (void)
{
    extern char __gp[];
    extern char __start_unwind[], __end_unwind[];
    ...
    // Kernel's own unwind table
    init_unwind_table(&unw.kernel_table, "kernel", KERNEL_START, (unsigned long) __gp,
        __start_unwind, __end_unwind);
}
// ...
void *
unw_add_unwind_table (const char *name, unsigned long segment_base, unsigned long gp,
                      const void *table_start, const void *table_end)
{
    // ...
    init_unwind_table(table, name, segment_base, gp, table_start, table_end);
}
// ...
static int __init
create_gate_table (void)
{
    // ...
    unw_add_unwind_table("linux-gate.so", segbase, 0, start, end);
}
// ...
static void
register_unwind_table (struct module *mod)
{
    // ...
    mod->arch.core_unw_table = unw_add_unwind_table(mod->name, 0, mod->arch.gp,
                                                    core, core + num_core);
    mod->arch.init_unw_table = unw_add_unwind_table(mod->name, 0, mod->arch.gp,
                                                    init, init + num_init);
}
```

Here we see unwind tables created for:

- one table for kernel itself
- one table `linux-gate.so` (equivalent of `linux-vdso.so.1` on
  `x86_64`)
- one table for each kernel module

## Arrays are hard

Nothing complicated, right? Actually `gcc` fails to generate correct
code for `end[-1].end_offset` expression! It happens to be a rare
corner case:
Both `__start_unwind` and `__end_unwind` are defined in linker
script as external symbols:

``` 
# somewhere in arch/ia64/kernel/vmlinux.lds.S
# ...
SECTIONS {
    # ...
    .IA_64.unwind : AT(ADDR(.IA_64.unwind) - LOAD_OFFSET) {
            __start_unwind = .;
            *(.IA_64.unwind*)
            __end_unwind = .;
    } :code :unwind
    # ...
```

Here is how C code defines `__end_unwind`:

``` c
extern char __end_unwind[];
```

If we manually inline all the above into `unw_init` we will get the
following:

``` c
void __init
unw_init (void)
{
    extern char __end_unwind[];
    ...
    table->end = segment_base + ((unw_table_entry *)__end_unwind)[-1].end_offset;
}
```

If `__end_unwind[]` would be an array defined in `c` then
negative index `-1` would cause undefined behavior.
On the practical side it's just pointer arithmetic. Is there anything
special about subtracting a few bytes from an arbitrary address and then
dereference it?
Let's check what kind of assembly `gcc` actually generates.

## Compiler mysteries

Still reading? Great! You got to most exciting part of this article!
Let's look at simpler code first. And then we will grow it to be closer
to our initial example.
Let's start from global array with a negative index:

``` c
extern long __some_table[];
long end(void) { return __some_table[-1]; }
```

Compilation result (I'll strip irrelevant bits and annotations):

``` asm
; ia64-unknown-linux-gnu-gcc-8.2.0 -O2 -S a.c
    .text
    .global end#
    .proc end#
end:
    addl r14 = @ltoffx(__some_table#), r1
    ;;
    ld8.mov r14 = [r14], __some_table#
    ;;
    adds r14 = -8, r14
    ;;
    ld8 r8 = [r14]
    br.ret.sptk.many b0
    .endp end#
```

Here two things happen:

- `__some_table` address is read from `GOT` (`r1` is roughly
  `GOT` register) by performing an `ld8.mov` (a form of 8-byte load)
  into `r14`.
- final value is loaded from address `r14 - 8` using `ld8` (also a
  8-byte load).

Simple!
We can simplify the example by avoiding `GOT` indirection. The typical
way to do it is to use `__attribute__((visibility("hidden")))`
hint:

``` c
extern long __some_table[] __attribute__((visibility("hidden")));
long end(void) { return __some_table[-1]; }
```

Assembly code:

``` asm
; ia64-unknown-linux-gnu-gcc-8.2.0 -O2 -S a.c
    .text
    .global end#
    .proc end#
end:
    movl r14 = @gprel(__some_table#)
    ;;
    add r14 = r1, r14
    ;;
    adds r14 = -8, r14
    ;;
    ld8 r8 = [r14]
    br.ret.sptk.many b0
```

Here `movl r14 = @gprel(__some_table#)` is a link-time 64-bit
constant: an offset of `__some_table` array from `r1` value. Only
a single 8-byte load happens at address `@gprel(__some_table#) +
r1 - 8`.
Also straightforward.
Now let's change the alignment of our table from `long` (8 bytes on
`ia64`) to `char` (1 byte):

``` c
extern char __some_table[] __attribute__((visibility("hidden")));
long end(void) { return ((long*)__some_table)[-1]; }
```

``` asm
; ia64-unknown-linux-gnu-gcc-8.2.0 -O2 -S a.c
    .text
    .global end#
    .proc end#
end:
    movl r14 = @gprel(__some_table#)
    ;;
    add r14 = r1, r14
    ;;
    adds r19 = -7, r14
    adds r16 = -8, r14
    adds r18 = -6, r14
    adds r17 = -5, r14
    adds r21 = -4, r14
    adds r15 = -3, r14
    ;;
    ld1 r19 = [r19]
    adds r20 = -2, r14
    adds r14 = -1, r14
    ld1 r16 = [r16]
    ;;
    ld1 r18 = [r18]
    shl r19 = r19, 8
    ld1 r17 = [r17]
    ;;
    or r19 = r16, r19
    shl r18 = r18, 16
    ld1 r16 = [r21]
    ld1 r15 = [r15]
    shl r17 = r17, 24
    ;;
    or r18 = r19, r18
    shl r16 = r16, 32
    ld1 r8 = [r20]
    ld1 r19 = [r14]
    shl r15 = r15, 40
    ;;
    or r17 = r18, r17
    shl r14 = r8, 48
    shl r8 = r19, 56
    ;;
    or r16 = r17, r16
    ;;
    or r15 = r16, r15
    ;;
    .mmi
    or r14 = r15, r14
    ;;
    or r8 = r14, r8
    br.ret.sptk.many b0
    .endp end#
```

This is quite a blowup in code size! Here instead of one 8-byte `ld8`
load compiler generated 8 1-byte `ld1` loads to assemble valid value
with the help of arithmetic shifts and `or`s.
Note how each individual byte gets it's personal register to keep an
address and result of the load.
Here is the subset of above instructions to handle byte offset `-5`:

``` asm
; point r14 at __some_table:
movl r14 = @gprel(__some_table#)
add r14 = r1, r14
;
; read one byte and shift it
; into destination byte position:
;
adds r17 = -5, r14
ld1 r17 = [r17]
shl r17 = r17, 24
or r16 = r17, r16
```

This code, while ugly and inefficient, is still correct.
Now let's wrap our 8-byte value in a `struct` to make example closer
to original unwinder's table registration code:

``` c
extern char __some_table[] __attribute__((visibility("hidden")));
struct s { long v; };
long end(void) { return ((struct s *)__some_table)[-1].v; }
```

**Quiz time**: do you think generated code will be exactly the same as
in previous example or somehow different?

``` asm
; ia64-unknown-linux-gnu-gcc-8.2.0 -O2 -S a.c
    .text
    .global end#
    .proc end#
end:
    movl r14 = @gprel(__some_table#)
    movl r16 = 0x1ffffffffffffff9
    ;;
    add r14 = r1, r14
    movl r15 = 0x1ffffffffffffff8
    movl r17 = 0x1ffffffffffffffa
    ;;
    add r15 = r14, r15
    add r17 = r14, r17
    add r16 = r14, r16
    ;;
    ld1 r8 = [r15]
    ld1 r16 = [r16]
    ;;
    ld1 r15 = [r17]
    movl r17 = 0x1ffffffffffffffb
    shl r16 = r16, 8
    ;;
    add r17 = r14, r17
    or r16 = r8, r16
    shl r15 = r15, 16
    ;;
    ld1 r8 = [r17]
    movl r17 = 0x1ffffffffffffffc
    or r15 = r16, r15
    ;;
    add r17 = r14, r17
    shl r8 = r8, 24
    ;;
    ld1 r16 = [r17]
    movl r17 = 0x1ffffffffffffffd
    or r8 = r15, r8
    ;;
    add r17 = r14, r17
    shl r16 = r16, 32
    ;;
    ld1 r15 = [r17]
    movl r17 = 0x1ffffffffffffffe
    or r16 = r8, r16
    ;;
    add r17 = r14, r17
    shl r15 = r15, 40
    ;;
    ld1 r8 = [r17]
    movl r17 = 0x1fffffffffffffff
    or r15 = r16, r15
    ;;
    add r14 = r14, r17
    shl r8 = r8, 48
    ;;
    ld1 r16 = [r14]
    or r15 = r15, r8
    ;;
    shl r8 = r16, 56
    ;;
    or r8 = r15, r8
    br.ret.sptk.many b0
    .endp end#
```

The code is different from the previous one! Seemingly not too much but
there one suspicious detail: offsets now are very large. Let's look at
our `-5` example again:

``` asm
; point r14 at __some_table:
movl r14 = @gprel(__some_table#)
add r14 = r1, r14
;
; read one byte and shift it
; into destination byte position:
;
movl r17 = 0x1ffffffffffffffb
add r17 = r14, r17
ld1 r8 = [r17]
shl r8 = r8, 24
or r8 = r15, r8
; ...
```

The offset `0x1ffffffffffffffb` (2305843009213693947) used here is
incorrect. It should have been `0xfffffffffffffffb` (-5).
We encounter (arguably) a compiler bug known as
[PR84184](https://gcc.gnu.org/PR84184). Upstream says struct handling is
different enough from direct array dereferences to trick `gcc` into
generating incorrect byte offsets.
One day I'll take a closer look at it to understand mechanics.

Let's explore one more example: what if we add bigger alignment to
`__some_table` without changing it's type?

``` c
extern char __some_table[] __attribute__((visibility("hidden"))) __attribute((aligned(8)));
struct s { long v; };
long end(void) { return ((struct s *)__some_table)[-1].v; }
```

``` asm
; ia64-unknown-linux-gnu-gcc-8.2.0 -O2 -S a.c
    .text
    .global end#
    .proc end#
end:
    movl r14 = @gprel(__some_table#)
    ;;
    add r14 = r1, r14
    ;;
    adds r14 = -8, r14
    ;;
    ld8 r8 = [r14]
    br.ret.sptk.many b0
```

Exactly as our original clean and fast example: single aligned load at
offset `-8`.
Now we have a simple workaround!
What if we pass our array in a register instead of using a global
reference? (effectively uninlining array address)

``` c
struct s { long v; };
long end(char * __some_table) { return ((struct s *)__some_table)[-1].v; }
```

``` asm
; ia64-unknown-linux-gnu-gcc-8.2.0 -O2 -S a.c
    .text
    .global end#
    .proc end#
end:
    adds r32 = -8, r32
    ;;
    ld8 r8 = [r32]
    br.ret.sptk.many b0
```

Also works! Note how compiler promotes alignment after a type cast from
1 to 8.
In this case a few things happen at the same time to trigger bad code
generation:

- `gcc` infers that `char __end_unwind[]` is an array literal with
  alignment 1
- `gcc` inlines `__end_unwind` into `init_unwind_table` and demotes
  alignment from 8 (`const struct unw_table_entry`) to 1 (`extern
  char []`)
- `gcc` assumes that `__end_unwind` can't have negative subscript and
  generates invalid (and inefficient) code

## Workarounds (aka hacks) time!

We can workaround corner-case conditions above in a few different ways:

- `[hack]` forbid inlining of `init_unwind_table()`: [lkml patch
  v1](https://lkml.org/lkml/2018/3/9/976)
- `[better fix]` expose real alignment of `__end_unwind`: [lkml
  patch v2](https://lkml.org/lkml/2018/2/2/914)

Fix is still not perfect as negative subscript it used. But at least the
load is aligned.
Note that `void __init unw_init()` is called early in kernel startup
sequence even before console is initialized.
This code generation bug causes either garbage read from some memory
location or kernel crash trying to access unmapped memory.
That is the `strace` breakage mechanics.

## Parting words

- Task switch on `x86_64` and on `ia64` is fun :)
- On `x86_64` implementation of `ptrace(PTRACE_GETREGS, ...)` is
  very straightforward: almost a `memcpy()` from predefined location.
- On `ia64` `ptrace(PTRACE_GETREGS, ...)` requires many moving
  parts:
  - call stack unwinder for kernel (involving linker scripts to define
    `__end_unwind` and `__start_unwind`)
  - byte code generator and byte code interpreter to speed up unwinding for
    every `ptrace()` call
- Unaligned load of register-sized value is a tricky and fragile
  business

Have fun!
