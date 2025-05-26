---
title: "Stack protection on mips64"
date: December 16, 2017
---

## Bug

On `#gentoo-toolchain` Matt Turner reported an obscure `SIGSEGV` of
`top` program on one of `mips` boxes:

``` 
07:01 <@mattst88> since upgrading to >=glibc-2.25 on mips:
07:01 <@mattst88> bcm91250a-be ~ # top
07:01 <@mattst88> *** stack smashing detected ***: <unknown> terminated
07:01 <@mattst88> Aborted
```

A simple (but widely used) tool seemingly managed to overwrite it's
stack. Looks straightforward, right? Let's see!

## Reproducing

As I don't have any `mips` hardware I will fake some:

``` 
# crossdev -t mips64-unknown-linux-gnu
# <set a few profile variables>
# mips64-unknown-linux-gnu-emerge -1 procps
```

We've built a toolchain that targets `mips64` (`N32` ABI). Checking
if it crashes:

``` 
$ LD_LIBRARY_PATH=/lib64 qemu-mipsn32 -L /usr/mips64-unknown-linux-gnu /usr/mips64-unknown-linux-gnu/usr/bin/top
qemu: uncaught target signal 11 (Segmentation fault) - core dumped
Segmentation fault
```

This is not exactly stack smash. But who knows, maybe my set of tools is
slightly different from Matt's and the same bug manifests in a slightly
different way.

## First workaround

Gentoo recently enabled stack protection and already [exposed a
bug](https://gcc.gnu.org/PR81996) at least on `powerpc32`.
First, let's check if it's related to stack protector option:

``` 
# EXTRA_ECONF=--enable-stack-protector=no emerge -v1 cross-mips64-unknown-linux-gnu/glibc
$ LD_LIBRARY_PATH=/lib64 qemu-mipsn32 -L /usr/mips64-unknown-linux-gnu /usr/mips64-unknown-linux-gnu/usr/bin/top
<running top>
```

That worked! To unbreak `mips` users I disabled `glibc` stack
self-protection by passing
[`--enable-stack-protector=no`](https://gitweb.gentoo.org/repo/gentoo.git/commit/sys-libs/glibc?id=b14c692fa08dc7bc53a81d32d36ddb1231769040).

## Into the rabbit hole

Now let's try to find out what is at fault here. In case of `powerpc`
it was bad `gcc` code generation. Could it be something similar here?
First, we need a backtrace from `qemu`. Unfortunately
`qemu`-generated `.core` files are truncated and are not directly
readable by `gdb`:

``` 
$ gdb --quiet /usr/mips64-unknown-linux-gnu/usr/bin/top qemu_top_20171216-220221_14697.core
Reading symbols from /usr/mips64-unknown-linux-gnu/usr/bin/top...done.
BFD: Warning: /home/slyfox/qemu_top_20171216-220221_14697.core is truncated: expected core file size >= 8867840, found: 1504.
```

Let's try `gdbserver` mode instead. Running `qemu` with `-g
12345` to wait for `gdb` session:

``` 
$ LD_LIBRARY_PATH=/lib64 qemu-mipsn32 -g 12345 -L /usr/mips64-unknown-linux-gnu /usr/mips64-unknown-linux-gnu/usr/bin/top
```

And in second terminal fire up `gdb`:

``` 
$ gdb --quiet /usr/mips64-unknown-linux-gnu/usr/bin/top
Reading symbols from /usr/mips64-unknown-linux-gnu/usr/bin/top...done.
(gdb) set sysroot /usr/mips64-unknown-linux-gnu
(gdb) target remote :12345
Remote debugging using :12345
Reading symbols from /usr/mips64-unknown-linux-gnu/lib32/ld.so.1...
Reading symbols from /usr/lib64/debug//usr/mips64-unknown-linux-gnu/lib32/ld-2.26.so.debug...done.
done.
0x40801d10 in __start () from /usr/mips64-unknown-linux-gnu/lib32/ld.so.1
(gdb) continue 
Continuing.

Program received signal SIGSEGV, Segmentation fault.
0x408cb908 in _dlerror_run (operate=operate@entry=0x408cadf0 <dlopen_doit>, args=args@entry=0x407feb58)
    at dlerror.c:163
163       result->errcode = _dl_catch_error (&result->objname, &result->errstring,
(gdb) bt
#0  0x408cb908 in _dlerror_run (operate=operate@entry=0x408cadf0 <dlopen_doit>, args=args@entry=0x407feb58)
    at dlerror.c:163
#1  0x408caf4c in __dlopen (file=file@entry=0x10012d58 "libnuma.so", mode=mode@entry=1) at dlopen.c:87
#2  0x1000306c in before (me=0x407ff3b6 "/usr/mips64-unknown-linux-gnu/usr/bin/top") at top/top.c:3308
#3  0x10001a10 in main (dont_care_argc=<optimized out>, argv=0x407ff1d4) at top/top.c:5721
```

Woohoo! Nice backtrace! Matt confirmed he sees the same backtrace.

Curious fact: `top` tries to load `libnuma.so` opportunistically
(`top/top.c`):

``` c
// ...
#ifndef NUMA_DISABLE
#if defined(PRETEND_NUMA) || defined(PRETEND8CPUS)
   Numa_node_tot = Numa_max_node() + 1;
#else
   // we'll try for the most recent version, then a version we know works...
   if ((Libnuma_handle = dlopen("libnuma.so", RTLD_LAZY))
    || (Libnuma_handle = dlopen("libnuma.so.1", RTLD_LAZY))) {
      Numa_max_node = dlsym(Libnuma_handle, "numa_max_node");
      Numa_node_of_cpu = dlsym(Libnuma_handle, "numa_node_of_cpu");
      if (Numa_max_node && Numa_node_of_cpu)
         Numa_node_tot = Numa_max_node() + 1;
      else {
         dlclose(Libnuma_handle);
         Libnuma_handle = NULL;
      }
   }
#endif
#endif
// ...
```

As I did not have `libnuma` installed it should not matter which
library we try to load. I tried to write a minimal reproducer that only
calls `dlopen()`:

``` c
#include <dlfcn.h>

// mips64-unknown-linux-gnu-gcc dlopen-bug.c -o dlopen-bug -ldl

int main() {
    void * h = dlopen("libdoes-not-exist.so", RTLD_LAZY);
}
```

``` 
$ mips64-unknown-linux-gnu-gcc dlopen-bug.c -o dlopen-bug -ldl
$ qemu-mipsn32 -L /usr/mips64-unknown-linux-gnu ./dlopen-bug
qemu: uncaught target signal 11 (Segmentation fault) - core dumped
Segmentation fault
```

The backtrace is the same. OK, that's bad. If it's a stack overflow it
definitely happens somewhere in `glibc` internals and not in the
client.

## Reproducing on master

Chances are we will need to fix `glibc` to get our stack protection
back. I tried to reproduce the same failure on `master` branch:

``` 
$ ../glibc/configure \
      --enable-stack-protector=all \
      --enable-stackguard-randomization \
      --enable-kernel=3.2.0 \
      --enable-add-ons=libidn \
      --without-selinux \
      --without-cvs \
      --disable-werror \
      --enable-bind-now \
      --build=x86_64-pc-linux-gnu \
      --host=mips64-unknown-linux-gnu \
      --disable-profile \
      --without-gd \
      --with-headers=/usr/mips64-unknown-linux-gnu/usr/include \
      --prefix=/usr \
      --sysconfdir=/etc \
      --localstatedir=/var \
      --libdir='$(prefix)/lib32' \
      --mandir='$(prefix)/share/man' \
      --infodir='$(prefix)/share/info' \
      --libexecdir='$(libdir)/misc/glibc' \
      --disable-multi-arch \
      --disable-systemtap \
      --disable-nscd \
      --disable-timezone-tools \
      CFLAGS="-O2 -ggdb3"
$ make
$ qemu-mipsn32 ./elf/ld.so --library-path .:dlfcn ~/tmp/dlopen-bug
```

No failure. It could mean the bug was fixed in `master` or something
else introduces the error. I checked out `glibc-2.26` and retried above:

``` 
$ qemu-mipsn32 ./elf/ld.so --library-path .:dlfcn ~/tmp/dlopen-bug
qemu: uncaught target signal 11 (Segmentation fault) - core dumped
Segmentation fault
```

Aha, the problem is still there in latest release.
I bisected `glibc` from `2.26` to `master` to find the commit that
fixes `SIGSEGV`. My bisection stopped at [commit
2449ae7b](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=2449ae7b2da24c9940962304a3e44bc80e389265)

``` 
commit 2449ae7b2da24c9940962304a3e44bc80e389265
Author: Florian Weimer <fweimer@redhat.com>
Date:   Thu Aug 10 13:40:22 2017 +0200

    ld.so: Introduce struct dl_exception

    This commit separates allocating and raising exceptions.  This
    simplifies catching and re-raising them because it is no longer
    necessary to make a temporary, on-stack copy of the exception message.
```

Looking hard at that commit I have found nothing that could fix a bug.
The change shuffled a few things around but did not change behavior too
much. I decided to fetch [parent commit
f87cc2bfb](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=f87cc2bfba9b844da48a63441c6099342b1551c7)
and spend some time to understand the failure mode.
First, the crash happens in
[`_dlerror_run()`](https://sourceware.org/git/?p=glibc.git;a=blob;f=dlfcn/dlerror.c;h=c0ead7dcb64e782ac4f2bee67d6117dc68a8314f;hb=f87cc2bfba9b844da48a63441c6099342b1551c7#l124)
code:

``` 
0x40801c70 in __start () from /home/slyfox/tmp/lib32/ld.so.1
(gdb) continue 
Continuing.

Program received signal SIGSEGV, Segmentation fault.
_dlerror_run (operate=operate@entry=0x40838df0 <dlopen_doit>, args=args@entry=0x407ff058) at dlerror.c:163
163       result->errcode = _dl_catch_error (&result->objname, &result->errstring,
(gdb) bt
#0  _dlerror_run (operate=operate@entry=0x40838df0 <dlopen_doit>, args=args@entry=0x407ff058) at dlerror.c:163
#1  0x40838f4c in __dlopen (file=<optimized out>, mode=<optimized out>) at dlopen.c:87
#2  0x1000076c in main ()
(gdb) list
158           if (result->malloced)
159             free ((char *) result->errstring);
160           result->errstring = NULL;
161         }
162
163       result->errcode = _dl_catch_error (&result->objname, &result->errstring,
164                                          &result->malloced, operate, args);
165
166       /* If no error we mark that no error string is available.  */
167       result->returned = result->errstring == NULL;
```

Simplified version of `_dlerror_run()` looks like this:

``` c
static struct dl_action_result last_result;
static struct dl_action_result *static_buf = &last_result

// ...

int
internal_function
_dlerror_run (void (*operate) (void *), void *args)
{
  struct dl_action_result *result;
  result = static_buf;
  if (result->errstring != NULL)
  {
     if (result->malloced)
       free ((char *) result->errstring);
     result->errstring = NULL;
  }
  result->errcode = _dl_catch_error (&result->objname, &result->errstring,
                                     &result->malloced, operate, args);
  /* If no error we mark that no error string is available.  */
  result->returned = result->errstring == NULL;
  return result->errstring != NULL;
}
```

The `SIGSEGV` happens when we try to store result of
`_dl_catch_error()` into `result->errcode`.

I poked in `gdb` at the values of `result` right before
`\_dl_catch_error()` call and after it. And values are different! Time
to look at where `result` is physically stored:

``` 
(gdb) disassemble /s _dlerror_run
163       result->errcode = _dl_catch_error (&result->objname, &result->errstring,
=> 0x40839918 <+184>:   sw      v0,0(s0)
(gdb) print (void*)$s0
$1 = (void *) 0x40834c44 <__stack_chk_guard>
```

The instruction stores single word(32 bits) at address of `s0`
register. But `s0` points not to `last_result` (it did right before
the call) but at `__stack_chk_guard`.

## How does stack checks work on `mips`

What is `__stack_chk_guard`? Has to do something about stack checks.
Short answer: it's a read-only variable that holds stack canary value.
`glibc` is not supposed to write to it after it is initialized.
Something leaked out address of that variable into `s0` (callee-save
register).
Let's familiarize ourselves with `mips` ABI a bit and look at how does
stack checks look like in generated code in a simple example:

``` c
void g(void) {}
```

``` asm
; mips64-unknown-linux-gnu-gcc -S b.c -fno-stack-protector -O1
.file   1 "b.c"
.section .mdebug.abiN32
.previous
.nan    legacy
.module fp=64
.module oddspreg
.abicalls
.text
.align  2
.globl  g
.set    nomips16
.set    nomicromips
.ent    g
.type   g, @function
g:
    .frame  $sp,0,$31               # vars= 0, regs= 0/0, args= 0, gp= 0
    .mask   0x00000000,0
    .fmask  0x00000000,0
    .set    noreorder
    .set    nomacro
    jr      $31
     nop
.set    macro
.set    reorder
.end    g
.size   g, .-g
.ident  "GCC: (Gentoo 7.2.0 p1.1) 7.2.0"
```

`31` register is also known as `ra`, return address. The code has a
lot of pragmas but they are needed only for debugging. The real code is
two instructions: `jr \$31; nop`.
Let's check what `-fstack-protector-all` does with our code:

``` asm
; mips64-unknown-linux-gnu-gcc -S b.c -fstack-protector-all -O1
.file   1 "b.c"
.section .mdebug.abiN32
.previous
.nan    legacy
.module fp=64
.module oddspreg
.abicalls
.text
.align  2
.globl  g
.set    nomips16
.set    nomicromips
.ent    g
.type   g, @function
g:
    .frame  $sp,32,$31              # vars= 16, regs= 2/0, args= 0, gp= 0
    .mask   0x90000000,-8
    .fmask  0x00000000,0
    .set    noreorder
    .set    nomacro
    addiu   $sp,$sp,-32                 ; allocate 32 bytes on stack
    sd      $31,24($sp)                 ; backup $31 (ra)
    sd      $28,16($sp)                 ; backup $28 (gp)
    lui     $28,%hi(__gnu_local_gp)     ; compute address of GOT
    addiu   $28,$28,%lo(__gnu_local_gp) ; (requires two instructions
    lw      $2,%got_disp(__stack_chk_guard)($28) ; read offset of __stack_chk_guard in GOT
    lw      $3,0($2)                    ; read canary value of __stack_chk_guard
    sw      $3,12($sp)                  ; store canary on stack
                                        ; ... time to check our canary!
    lw      $3,12($sp)                  ; load canary from stack
    lw      $2,0($2)                    ; load canary from __stack_chk_guard
    bne     $3,$2,.L4                   ; check canary value and crash the program
    ld      $31,24($sp)                 ; restore return address
    ld      $28,16($sp)                 ; restore gp
    jr      $31                         ; (restore stack pointer and) return
     addiu   $sp,$sp,32

.L4:
    lw      $25,%call16(__stack_chk_fail)($28)
    .reloc  1f,R_MIPS_JALR,__stack_chk_fail
1:      jalr    $25
    nop
.set    macro
.set    reorder
.end    g
.size   g, .-g
.ident  "GCC: (Gentoo 7.2.0 p1.1) 7.2.0"
```

Here is a quick
[table](https://www.cs.umd.edu/class/sum2003/cmsc311/Notes/Mips/altReg.html)
of `mips` registers.

15 instructions are doing the following: intermediate registers to hold
canary value `2`(`v0`) and `3`(`v1`) are written on stack
(`sp` register), read back and checked against value stored in
`__stack_chk_guard`. Quite straightforward.

# Who broke `s0`?

Back to our `_dl_catch_error()` why did `s0` change? `mips`
`ABI` says `s0` is callee-save. It means `s0` should not be
changed by callee functions.

To get more clues we need to dive into
[`_dl_catch_error()`](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/dl-error-skeleton.c;h=8e5888d4bdf7e325358bc737000b31573f000736;hb=f87cc2bfba9b844da48a63441c6099342b1551c7#l171)

``` c
struct catch
{
  const char **objname;       /* Object/File name.  */
  const char **errstring;     /* Error detail filled in here.  */
  bool *malloced;             /* Nonzero if the string is malloced
                                 by the libc malloc.  */
  volatile int *errcode;      /* Return value of _dl_signal_error.  */
  jmp_buf env;                /* longjmp here on error.  */
};
// ...
int
internal_function
_dl_catch_error (const char **objname, const char **errstring,
                 bool *mallocedp, void (*operate) (void *), void *args)
{
  /* We need not handle `receiver' since setting a `catch' is handled
     before it.  */

  /* Only this needs to be marked volatile, because it is the only local
     variable that gets changed between the setjmp invocation and the
     longjmp call.  All others are just set here (before setjmp) and read
     in _dl_signal_error (before longjmp).  */
  volatile int errcode;

  struct catch c;
  /* Don't use an initializer since we don't need to clear C.env.  */
  c.objname = objname;
  c.errstring = errstring;
  c.malloced = mallocedp;
  c.errcode = &errcode;

  struct catch *const old = catch_hook;
  catch_hook = &c;

  /* Do not save the signal mask.  */
  if (__builtin_expect (__sigsetjmp (c.env, 0), 0) == 0)
    {
      (*operate) (args);
      catch_hook = old;
      *objname = NULL;
      *errstring = NULL;
      *mallocedp = false;
      return 0;
    }

  /* We get here only if we longjmp'd out of OPERATE.  _dl_signal_error has
     already stored values into *OBJNAME, *ERRSTRING, and *MALLOCEDP.  */
  catch_hook = old;
  return errcode;
}
```

This code is straightforward (but very scary): it wraps call of
`operate` callback into `__sigsetjmp()` (really just a `setjmp()`).
`setjmp()` is a simple-ish function: it stores most of current
registers into `c.env()` and later `longjmp()` restores them.
Caller-saves are not saved because `longjmp()` looks like a normal `c`
function call.
Normally `longjmp()` is called only when error condition happens. In
our case it's called when `dlopen()` fails (we are opening
non-existent file). `longjmp()` restores all registers stored by
`setjmp()` including instruction pointer `pc`, stack pointer `sp`,
caller-saves `s0..s7` and others.

The question arises: why and where do we lose `s0` register? At save
(`setjmp()`) or at restore (`longjmp()`)?

As we can see `setjmp()` and `longjmp()` are functions very
sensitive to `ABI`. Let's check how `__sigsetjmp()` is
implemented at
[`sysdeps/mips/mips64/setjmp.S`](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/mips/mips64/setjmp.S;h=3e4120e1ea8db9e41f48d7efe17bd2f2ea8c38c5;hb=f87cc2bfba9b844da48a63441c6099342b1551c7#l22)

``` asm
ENTRY (__sigsetjmp)
    SETUP_GP
    SETUP_GP64_REG (v0, C_SYMBOL_NAME (__sigsetjmp))
    move a2, sp
    move a3, fp
    PTR_LA t9, __sigsetjmp_aux
    RESTORE_GP64_REG
    move a4, gp
    jr t9
END (__sigsetjmp)
```

Note how `__sigsetjmp` does almost nothing here: only saves `gp`,
`sp` and `fp` and defers everything to `__sigsetjmp_aux`. Let's
peek at that in
[`sysdeps/mips/mips64/setjmp_aux.c`](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/mips/mips64/setjmp_aux.c;h=b43c36a7d50d82da6d1e47c31a052e03e94f6aff;hb=f87cc2bfba9b844da48a63441c6099342b1551c7#l27)

Suddenly, its implementation is in `c`:

``` c
int
__sigsetjmp_aux (jmp_buf env, int savemask, long long sp, long long fp,
                 long long gp)
{
  /* Store the floating point callee-saved registers...  */
  asm volatile ("s.d $f20, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[0]));
  asm volatile ("s.d $f22, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[1]));
  asm volatile ("s.d $f24, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[2]));
  asm volatile ("s.d $f26, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[3]));
  asm volatile ("s.d $f28, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[4]));
  asm volatile ("s.d $f30, %0" : : "m" (env[0].__jmpbuf[0].__fpregs[5]));

  /* .. and the PC;  */
  asm volatile ("sd $31, %0" : : "m" (env[0].__jmpbuf[0].__pc));

  /* .. and the stack pointer;  */
  env[0].__jmpbuf[0].__sp = sp;

  /* .. and the FP; it'll be in s8. */
  env[0].__jmpbuf[0].__fp = fp;

  /* .. and the GP; */
  env[0].__jmpbuf[0].__gp = gp;

  /* .. and the callee-saved registers; */
  asm volatile ("sd $16, %0" : : "m" (env[0].__jmpbuf[0].__regs[0]));
  asm volatile ("sd $17, %0" : : "m" (env[0].__jmpbuf[0].__regs[1]));
  asm volatile ("sd $18, %0" : : "m" (env[0].__jmpbuf[0].__regs[2]));
  asm volatile ("sd $19, %0" : : "m" (env[0].__jmpbuf[0].__regs[3]));
  asm volatile ("sd $20, %0" : : "m" (env[0].__jmpbuf[0].__regs[4]));
  asm volatile ("sd $21, %0" : : "m" (env[0].__jmpbuf[0].__regs[5]));
  asm volatile ("sd $22, %0" : : "m" (env[0].__jmpbuf[0].__regs[6]));
  asm volatile ("sd $23, %0" : : "m" (env[0].__jmpbuf[0].__regs[7]));

  /* Save the signal mask if requested.  */
  return __sigjmp_save (env, savemask);
}
```

The function duly stores every caller-save (including `16` aka `s0`)
and more into `c.env`. But what happens when that function is being
compiled with `-fstack-protector-all`? How does it preserve original
registers? Unfortunately the answer is: it does not.
Let's compare assembly output with and without `-fstack-protector-all`:

``` asm
; **-fno-stack-protector**:
Dump of assembler code for function __sigsetjmp_aux:
addiu   sp,sp,-16
sd      gp,0(sp)
lui     gp,0x16
addu    gp,gp,t9
sd      ra,8(sp)
addiu   gp,gp,7248
sdc1    $f20,104(a0)
sdc1    $f22,112(a0)
sdc1    $f24,120(a0)
sdc1    $f26,128(a0)
sdc1    $f28,136(a0)
sdc1    $f30,144(a0)
sd      ra,0(a0)
sd      a2,8(a0)
sd      a3,80(a0)
sd      a4,88(a0)
sd      s0,16(a0)
sd      s1,24(a0)
sd      s2,32(a0)
sd      s3,40(a0)
sd      s4,48(a0)
sd      s5,56(a0)
sd      s6,64(a0)
sd      s7,72(a0)
lw      t9,-32236(gp)
bal     0x30000 <__sigjmp_save>
nop
ld      ra,8(sp)
ld      gp,0(sp)
jr      ra
addiu   sp,sp,16
```

``` asm
; **-fstack-protector-all**:
Dump of assembler code for function __sigsetjmp_aux:
addiu   sp,sp,-48
sd      gp,32(sp)
lui     gp,0x18
addu    gp,gp,t9
addiu   gp,gp,-23968
sd      s0,24(sp)     ; here we backup s0
lw      s0,-27824(gp) ; and load into s0 stack canary address
sd      ra,40(sp)
lw      v1,0(s0)
sw      v1,12(sp)
sdc1    $f20,104(a0)
sdc1    $f22,112(a0)
sdc1    $f24,120(a0)
sdc1    $f26,128(a0)
sdc1    $f28,136(a0)
sdc1    $f30,144(a0)
sd      ra,0(a0)
sd      a2,8(a0)
sd      a3,80(a0)
sd      a4,88(a0)
sd      s0,16(a0)
sd      s1,24(a0)
sd      s2,32(a0)
sd      s3,40(a0)
sd      s4,48(a0)
sd      s5,56(a0)
sd      s6,64(a0)
sd      s7,72(a0)
lw      t9,-32100(gp)
bal     0x31940 <__sigjmp_save>
nop
lw      a0,12(sp)
lw      v1,0(s0)
bne     a0,v1,0x31c7c <__sigsetjmp_aux+156>
ld      ra,40(sp)
ld      gp,32(sp)
ld      s0,24(sp)
jr      ra
addiu   sp,sp,48
lw      t9,-32644(gp)
jalr    t9
nop
```

Or in diff form:

```diff
--- no-sp       2017-12-16 23:53:51.591627849 +0000
+++ spa 2017-12-16 23:53:37.952647838 +0000
@@ -1 +1 @@
-    ; **-fno-stack-protector**:
+    ; **-fstack-protector-all**:
@@ -3,3 +3,3 @@
-    addiu   sp,sp,-16
-    sd      gp,0(sp)
-    lui     gp,0x16
+    addiu   sp,sp,-48
+    sd      gp,32(sp)
+    lui     gp,0x18
@@ -7,2 +7,6 @@
-    sd      ra,8(sp)
-    addiu   gp,gp,7248
+    addiu   gp,gp,-23968
+    sd      s0,24(sp)     ; here we backup s0
+    lw      s0,-27824(gp) ; and load into s0 stack canary address
+    sd      ra,40(sp)
+    lw      v1,0(s0)
+    sw      v1,12(sp)
@@ -27,2 +31,2 @@
-    lw      t9,-32236(gp)
-    bal     0x30000 <__sigjmp_save>
+    lw      t9,-32100(gp)
+    bal     0x31940 <__sigjmp_save>
@@ -30,2 +34,6 @@
-    ld      ra,8(sp)
-    ld      gp,0(sp)
+    lw      a0,12(sp)
+    lw      v1,0(s0)
+    bne     a0,v1,0x31c7c <__sigsetjmp_aux+156>
+    ld      ra,40(sp)
+    ld      gp,32(sp)
+    ld      s0,24(sp)
@@ -33 +41,4 @@
-    addiu   sp,sp,16
+    addiu   sp,sp,48
+    lw      t9,-32644(gp)
+    jalr    t9
+    nop
```

## The minimal reproducer and fix

To fully nail down the problem I'd like to have something nice for
upstream. Here is the minimal reproducer:

``` c
#include <setjmp.h>
#include <stdio.h>

int main() {
    jmp_buf jb;
    volatile register long s0 asm ("$s0");
    s0 = 1234;
    if (setjmp(jb) == 0)
        longjmp(jb, 1);
    printf ("$s0 = %lu\n", s0);
}
```

``` 
$ qemu-mipsn32 -L ~/bad-libc ./mips-longjmp-bug
$s0 = 1082346564
$ qemu-mipsn32 -L ~/fixed-libc ./mips-longjmp-bug
$s0 = 1234
```

And the fix is to disable stack protection of `__sigsetjmp_aux()` in
all build modes of `glibc`. This does work:

``` diff
--- a/sysdeps/mips/mips64/setjmp_aux.c
+++ b/sysdeps/mips/mips64/setjmp_aux.c
@@ -25,6 +25,7 @@
    access them in C.  */

 int
+inhibit_stack_protector
 __sigsetjmp_aux (jmp_buf env, int savemask, long long sp, long long fp,
                 long long gp)
 {
```

## Parting words

So it was not a stack corruption after all. But the register corruption
triggered by a security feature.

What more interesting is why Matt got `stack smashing detected` and
not `SIGSEGV`. It means that write into `__stack_chk_guard`
actually succeeded and caused canary check failure not because on-stack
canary copy changed but because global canary changed.
`__stack_chk_guard` sits in `.data.rel.ro` section. `qemu` maps
it as read-only and crashes my process. How it behaves on real target is
an exercise to the reader with `mips` device :)

Fun facts:

- It took me 12 days to find out the cause of failure. Working `gdb`
  and `qemu-user` made the fix happen.
- `setjmp()` / `longjmp()` are not so opaque for me and hopefully
  for you. But make sure you have read [all the volatility
  gotchas](https://linux.die.net/man/3/longjmp) (Notes section)
- `glibc` uses `setjmp()` / `longjmp()` for error handling
- `mips` ABI is very pleasant to work with and not as complicated as I
  thought :)
- `qemu-mipsn32` needs a fix to generate readable `.core` files

Have fun!
