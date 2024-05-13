---
title: "the sagemath saga"
date: May 12, 2024
---

It's a story of me was
[nerd sniped](https://fosstodon.org/@sheerluck@misskey.io/112372289317724185)
by `@sheerluck@misskey.io` with [`PR114872`](https://gcc.gnu.org/PR114872).
I spent this week having fun with a rare kind of bug: `gcc` was suspected
to have a bug that causes `sagemath` to crash with `SIGSEGV` on certain
inputs.

Ideally `sage` should not `SIGSEGV` on problematic inputs and should
instead print reasonable backtraces. At least printing a nicer error
message should not be hard, should it?

## The symptom

The report said that a simple session caused `sage` tool from `sagemath`
package to `SIGSEGV`:

```
$ sage
sage: libgap.AbelianGroup(0,0,0)

...
Segmentation fault (core dumped)
```

`sage` is an `ipython` `REPL` with a bunch of bindings for math
libraries like [`GAP`](https://www.gap-system.org/).

It is said that the bug happens only when `sagemath` is built against
`python-3.12` while `python-3.11` would work without the problems.

Normally it would be a strong hint of a `sagemath` bug. But the reporter
suspected it's a `gcc` problem as building `sage` with `-O1` made the
bug go away. Thus the bug is at least dependent on generated code and is
likely not just a logic bug.

**Quick quiz: where do you think the bug lies?** In `gcc`, `sagemath` or
somewhere else? And while at it: what is the cause of the bug?
Use-after-free, use of uninitialized data, logic bug or maybe something
else?

## The first reproducer attempt: `nixpkgs` package

I tried to reproduce the bug by building `sage` from `nxipkgs`. As
expected `sage` built against `python-3.11` worked just fine.
`python-3.11` is a `nixpkgs` default. I tried to flip the default to
`python-3.12` with this local change:

```diff
--- a/pkgs/top-level/all-packages.nix
+++ b/pkgs/top-level/all-packages.nix
@@ -17505 +17505 @@ with pkgs;
-  python3 = python311;
+  python3 = python312;
@@ -17509 +17509 @@ with pkgs;
-  python3Packages = dontRecurseIntoAttrs python311Packages;
+  python3Packages = dontRecurseIntoAttrs python312Packages;
```

Building it did not work as is:

```
$ nix build -f. sage
...


error: ipython-genutils-0.2.0 not supported for interpreter python3.12


error: nose-1.3.7 not supported for interpreter python3.12


> src/gmpy2_convert_gmp.c:464:76: error: ‘PyLongObject’ {aka ‘struct _longobject’} has no member named ‘ob_digit’
>   464 |                    sizeof(templong->ob_digit[0])*8 - PyLong_SHIFT, templong->ob_digit);
>       |                                                                            ^~
> error: command '/nix/store/8mjb3ziimfi3rki71q4s0916xkm4cm5p-gcc-wrapper-13.2.0/bin/gcc' failed with exit code 1
> /nix/store/558iw5j1bk7z6wrg8cp96q2rx03jqj1v-stdenv-linux/setup: line 1579: pop_var_context: head of shell_variables not a function context
For full logs, run 'nix log /nix/store/g7mf3p2cylf74j3ypq2ifcspx61isb36-python3.12-gmpy2-2.1.2.drv'.


> ModuleNotFoundError: No module named 'distutils'
> configure: error: Python explicitly requested and python headers were not found
For full logs, run 'nix log /nix/store/xyd63v16k1krblcfypfn5bs6jqbj9lwd-audit-3.1.2.drv'.
```

The above told me that some dependencies (at least in `nixpkgs`) are not
ready for `python-3.12`:

- `nose` and `ipython-genutils` are explicitly disabled for `python-3.12`
  in `nixpkgs`
- `gmpy2` and `audit` just fail to build for API changes in `python`
  itself

`gmpy2` specifically has an open
[upstream report](https://github.com/aleaxit/gmpy/issues/446)
to add support for `python-3.12`. This means distributions have to apply
not yet upstreamed changes to get earlier `python-3.12` support.

All the above means that porting fixes are sometimes not trivial and
might vary from a distribution to distribution if they want to get
`python-3.12` tested earlier.

I did not feel confident to patch at least 4 `python` libraries to get
`sagemath` to build. I switched the tactic to reproduce the bug on the
system reporters were using and to explore it there.

Original reporter used Arch Linux to reproduce the failure. Another user
[reported](https://github.com/sagemath/sage/pull/36407#issuecomment-2093792864)
that `Gentoo` users also seen the similar problem.

## The second reproducer attempt: `Gentoo` package from `::sage-on-gentoo`

I had a `Gentoo` chroot lying around for `nix` packaging testing. I
tried to reproduce `sagemath` failure there by using
[`::sage-on-gentoo`](https://github.com/cschwan/sage-on-gentoo) overlay.
Unfortunately neither latest release of `sagemath-standard-10.3` nor
`sagemath-standard-9999` `git` versions did build for me as is. I filed
2 bugs:

- [cschwan/sage-on-gentoo#783](https://github.com/cschwan/sage-on-gentoo/issues/783): `=sci-mathematics/sagemath-standard-10.3` fails as: `AttributeError: Can't pickle local object '_prepare_extension_detection.<locals>.<lambda>'`
- [cschwan/sage-on-gentoo#784](https://github.com/cschwan/sage-on-gentoo/issues/784): `=sci-mathematics/sage_setup-9999` fails as: `tar (child): sage-setup-*.tar.gz: Cannot open: No such file or directory`

I hoped that `pickle` failure was fixed in latest `git` and I could avoid
the second pickle bug by using it.

At least the `tar` failure looked like a packaging issue:

```
# ACCEPT_KEYWORDS='**' USE=text emerge -av1 sagemath-standard
...
tar (child): sage-setup-*.tar.gz: Cannot open: No such file or directory
tar (child): Error is not recoverable: exiting now
tar: Child returned status 2
tar: Error is not recoverable: exiting now
 * ERROR: sci-mathematics/sage_setup-9999::sage-on-gentoo failed (unpack phase):
...
```

François Bissey promptly
[fixed](https://github.com/cschwan/sage-on-gentoo/commit/cc57aef4021bf673d02d20bd483b2708f9336f63)
`tar` failure! Unfortunately that did not fix the `pickle` bug in
`sagemath-standard-9999` for me and it started failing just like `sagemath-standard-10.3`:

```
# emerge -av1 sagemath-standard
...
AttributeError: Can't pickle local object '_prepare_extension_detection.<locals>.<lambda>'
```

Surprisingly not everyone saw that problem and some people were able to
build the packages just fine. Day later I explored where
`_prepare_extension_detection` comes from and I was able to find a
[surprising workaround](https://github.com/cschwan/sage-on-gentoo/issues/783#issuecomment-2095500118):
I needed to uninstall completely unrelated `scikit-build-core` `python`
package that happened to be present in my system. `scikit-build-core` is
not used by `sagemath-standard` neither directly nor indirectly. But
somehow [it's code](https://github.com/scikit-build/scikit-build-core/blob/f6ed5a28fc85e621b03d984011d17def888ee0db/src/scikit_build_core/setuptools/build_cmake.py#L183) injected
the extra attributes to `cmake`-based package builds and failed the build.

At least I finally got `sage` tool in my `$PATH`!

I took me two evenings to get `sagemath` to build. At last I could look
at the crash now.

## Exploring the crash

`sage` is a python program. It has a default handler that executes `gdb`
at crash time. Unfortunately it does not work on `Gentoo`:

```
Attaching gdb to process id 1023.
Traceback (most recent call last):
  File "/usr/lib/python-exec/python3.12/cysignals-CSI", line 225, in <module>
    main(args)
  File "/usr/lib/python-exec/python3.12/cysignals-CSI", line 174, in main
    trace = run_gdb(args.pid, not args.nocolor)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/lib/python-exec/python3.12/cysignals-CSI", line 98, in run_gdb
    stdout, stderr = cmd.communicate(gdb_commands(pid, color))
                                     ^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/lib/python-exec/python3.12/cysignals-CSI", line 71, in gdb_commands
    with open(script, 'rb') as f:
         ^^^^^^^^^^^^^^^^^^
FileNotFoundError: [Errno 2] No such file or directory: '/usr/lib/python-exec/python3.12/../share/cysignals/cysignals-CSI-helper.py'
```

Missing `cysignals-CSI-helper.py` file at the expected location is a
case of [packaging error](https://bugs.gentoo.org/927767). `Gentoo` uses
very unusual path to python launcher and breaks too simplistic
`argv[0]+"/../share"` path construction used in `cysignals`. Adding a
few more `"../../../"` should do as a workaround.

I was able to workaround `gdb` launch failure by attaching `gdb` to
already running process before pasting the problematic command into the
`REPL`:

```
# gdb --quiet -p `pgrep sage-ipython`
Attaching to process 1060
[New LWP 1082]
[New LWP 1083]
[Thread debugging using libthread_db enabled]
Using host libthread_db library "/lib64/libthread_db.so.1".
0x00007fedba181256 in epoll_wait (epfd=5, events=events@entry=0x7fed57127590, maxevents=maxevents@entry=2,
    timeout=timeout@entry=500) at ../sysdeps/unix/sysv/linux/epoll_wait.c:30
30        return SYSCALL_CANCEL (epoll_wait, epfd, events, maxevents, timeout);
```

To get more details from the crash site I ran `continue` in `gdb` and
typed the trigger expression: `libgap.AbelianGroup(0,0,0)`.

To my surprise I got not a `SIGSEGV` but a `SIGABRT`:

```
(gdb) continue
Continuing.
[Thread 0x7fed56e006c0 (LWP 1083) exited]
[Detaching after vfork from child process 1105]

Thread 1 "sage-ipython" received signal SIGABRT, Aborted.
0x00007fedba0b97a7 in __GI_kill () at ../sysdeps/unix/syscall-template.S:120
120     T_PSEUDO (SYSCALL_SYMBOL, SYSCALL_NAME, SYSCALL_NARGS)
```

The default signal handler for `SIGABRT` normally crashes the process
and generates a core dump. `sagemath` installs `SIGABRT` handler (via
`cysignals` library) to report and recover from some errors like argument
type errors in the interpreter session.

`gdb` always intercepts `SIGABRT` before executing the handler. Thus I
needed to explicitly continue execution in `gdb` session:

```
(gdb) continue
Continuing.

Thread 1 "sage-ipython" received signal SIGSEGV, Segmentation fault.
0x00007fed5d13956f in _Py_IsImmortal (op=0x0) at /usr/include/python3.12/object.h:242
242         return _Py_CAST(PY_INT32_T, op->ob_refcnt) < 0;
```

Yay! I got the `SIGSEGV`!

A simple `NULL` dereference. What could be easier to debug? Just check
where it was set to `NULL` and do something about it, right?

First thing I wondered about is how does `SIGABRT` handler look like? It
was an idle curiosity. I expected to see some simple global variable tweak.
Alas what I found was [`longjmp()`](https://github.com/sagemath/cysignals/blob/035ed1605a8741a6f265a55cc682b26ea6e5d1c2/src/cysignals/implementation.c#L279):

```c
static void cysigs_interrupt_handler(int sig)
{
...
    if (cysigs.sig_on_count > 0)
    {
        if (!cysigs.block_sigint && !PARI_SIGINT_block && !custom_signal_is_blocked())
        {
            /* Raise an exception so Python can see it */
            do_raise_exception(sig);

            /* Jump back to sig_on() (the first one if there is a stack) */
            siglongjmp(trampoline, sig);
        }
    }
...
}
```

One has to be
[very](https://trofi.github.io/posts/188-grub-0.97-and-gcc-4.9.html)
[careful](https://trofi.github.io/posts/205-stack-protection-on-mips64.html)
with `setjmp()` / `longjmp()`.

## The example `setjmp()` / `longjmp()` failure mode

The `C` standard has a few constructs that don't mix well with `C`
abstract machine. `setjmp()` / `longjmp()` is one of those. It's a
frequent source of subtle and latent bugs.

In theory `setjmp()` / `longjmp()` is a portable way to save and restore
the execution at a certain point in the program. It's a non-local
`goto`. It is very powerful: you can jump from a few nested calls with
a `longjmp()` back to where you called `setjmp()`. In practice one has
to be very careful to get something that works most of the time.

Let's look at a contrived and **intentionally buggy** example:

```c
// $ cat a.c
#include <assert.h>
#include <setjmp.h>
#include <stdio.h>

static jmp_buf jb;

__attribute__((noipa)) static int foo(void) {
    int a = 0;
    int r = setjmp(jb); // r = 0 initially, r = 1 after longjmp()

    a += 1; // gets executed twice, but the compiler does not know it!

    if (!r) longjmp(jb, 1); // execute it just once

    return a;
}

int main(void) {
    int r = foo();
    printf("foo() = %d (expect 2)\n", r);
    assert(r == 2);
    return 0;
}
```

Here we execute `foo()` function once and use `longjmp()` to return to
`setjmp()` (also just once). As a result we should execute `a += 1;`
statement twice:

- once during natural `foo()` execution
- once via "hidden" jump via `longjmp()` call to `setjmp()`
  location.

I'm using `__attribute__((noipa))` to keep `foo()` from being inlined
into `main()` to ease `foo()`'s code exploration.

The test against the unoptimized build confirms that `a += 1` gets
executed twice:

```
$ gcc a.c -o a -O0 && ./a
foo() = 2 (expect 2)
```

Assembly output shows the expected code:

```asm
; $ gdb ./a
; (gdb) disassemble foo
Dump of assembler code for function foo:
   <+0>:     push   %rbp
   <+1>:     mov    %rsp,%rbp
   <+4>:     sub    $0x10,%rsp
   <+8>:     movl   $0x0,-0x8(%rbp)
   <+15>:    lea    0x2ed4(%rip),%rax        # 0x404040 <jb>
   <+22>:    mov    %rax,%rdi
   <+25>:    call   0x401050 <_setjmp@plt> // setjmp();
   <+30>:    mov    %eax,-0x4(%rbp)
   <+33>:    addl   $0x1,-0x8(%rbp)        // a += 1;
   <+37>:    cmpl   $0x0,-0x4(%rbp)        // if (!r) ...
   <+41>:    jne    0x401195 <foo+63>
   <+43>:    mov    $0x1,%esi
   <+48>:    lea    0x2eb3(%rip),%rax        # 0x404040 <jb>
   <+55>:    mov    %rax,%rdi
   <+58>:    call   0x401060 <longjmp@plt> // longjmp();
   <+63>:    mov    -0x8(%rbp),%eax
   <+66>:    leave
   <+67>:    ret                           // return a;
```

It's a linear code with a single explicit branch to skip `longjmp()` call.
Looks easy?

Now let's optimize it:

```
$ gcc a.c -o a -O2 && ./a
foo() = 1 (expect 2)
a: a.c:21: main: Assertion `r == 2' failed.
Aborted (core dumped)
```

Whoops. `foo() = 1` output suggests that `a` was incremented only once.
Let's look at the generated code to get the idea of what happened:

```asm
; $ gdb ./a
; (gdb) disassemble foo
Dump of assembler code for function foo:
   <+0>:     sub    $0x8,%rsp
   <+4>:     lea    0x2e85(%rip),%rdi        # 0x404040 <jb>
   <+11>:    call   0x401040 <_setjmp@plt>
   <+16>:    test   %eax,%eax              // if (!r) ...
   <+18>:    je     0x4011ce <foo+30>
   <+20>:    mov    $0x1,%eax              // a = 1;
   <+25>:    add    $0x8,%rsp
   <+29>:    ret                           // return a
   <+30>:    mov    $0x1,%esi
   <+35>:    lea    0x2e66(%rip),%rdi        # 0x404040 <jb>
   <+42>:    call   0x401060 <__longjmp_chk@plt>
```

Nothing prevented `gcc` from transforming the original `foo()` into this
simpler form:

```c
// ...
__attribute__((noipa)) static int foo(void) {
    int r = setjmp(jb); // r = 0 initially, r = 1 after longjmp()
    if (!r) longjmp(jb, 1);
    int a = 1;          // moved `int a = 0;` here and merged into `a += 1;`.
    return a;
}
// ...
```

Here `gcc` did quite a bit of code motion:

- `gcc` moved `int a = 0;` assignment and an `a += 1;` increment after
  both `setjmp()` and `longjmp()`.
- `gcc` merged `int a = 0;`, `a += 1;` and `return a;` into a single `return 1;`.

**Note that `gcc` moved all `a` manipulation across `setjmp()` and even
`longjmp()` boundaries**. `setjmp()` is expected to be
just a C function for `gcc`. `gcc` does not have to have any special
knowledge about control flow semantics of those functions. Thus this
optimization transformation while completely breaking the original
code's intent is legitimate and expected.

`man 3 setjmp` covers exactly this case in `CAVEATS` section as:

```
CAVEATS
  The compiler may optimize variables into registers, and longjmp() may
  restore the values of other registers in addition to the stack pointer
  and program counter. Consequently, the values of automatic variables
  are unspecified after a call to longjmp() if they meet all the
  following criteria:
  •  they are local to the function that made the corresponding setjmp()
     call;
  •  their values are changed between the calls to setjmp() and
     longjmp(); and
  •  they are not declared as volatile.
```

It's up to code author to adhere to these `CAVEATS`. Would be nice if
the compiler would be able to warn about the violations in simpler cases
though: `-flto` could expose large chunk of the control flow graph to
the analyzer.

Back to our broken example. One of the possible fixes here is to declare
`a` as `volatile`:

```diff
--- a.c.buggy   2024-05-09 23:14:54.383811692 +0100
+++ a.c 2024-05-10 00:03:53.694219636 +0100
@@ -7,3 +7,3 @@
 __attribute__((noipa)) static int foo(void) {
-    int a = 0;
+    volatile int a = 0;
     int r = setjmp(jb); // r = 0 initially, r = 1 after longjmp()
```

That would give us the following (correct) example:

```c
#include <assert.h>
#include <setjmp.h>
#include <stdio.h>

static jmp_buf jb;

__attribute__((noipa)) static int foo(void) {
    volatile int a = 0;
    int r = setjmp(jb); // r = 0 initially, r = 1 after longjmp()

    a += 1; // gets executed twice

    if (!r) longjmp(jb, 1);

    return a;
}

int main(void) {
    int r = foo();
    printf("foo() = %d (expect 2)\n", r);
    assert(r == 2);
}
```

Running:

```
$ gcc a.c -o a -O0 && ./a
foo() = 2 (expect 2)

$ gcc a.c -o a -O2 && ./a
foo() = 2 (expect 2)
```

The execution confirms that both optimizations run the increment twice
    as intended. This is the generated code for completeness:

```asm
; gdb ./a
; (gdb) disassemble foo
Dump of assembler code for function foo:
   <+0>:     sub    $0x18,%rsp
   <+4>:     lea    0x2e85(%rip),%rdi        # 0x404040 <jb>
   <+11>:    movl   $0x0,0xc(%rsp)           // int a = 0;
   <+19>:    call   0x401040 <_setjmp@plt>
   <+24>:    mov    %eax,%edx
   <+26>:    mov    0xc(%rsp),%eax           // load a from stack
   <+30>:    add    $0x1,%eax                // a += 1;
   <+33>:    mov    %eax,0xc(%rsp)           // store a on stack
   <+37>:    test   %edx,%edx
   <+39>:    je     0x4011e2 <foo+50>
   <+41>:    mov    0xc(%rsp),%eax           // load a from stack
   <+45>:    add    $0x18,%rsp
   <+49>:    ret                             // return
   <+50>:    mov    $0x1,%esi
   <+55>:    lea    0x2e52(%rip),%rdi        # 0x404040 <jb>
   <+62>:    call   0x401060 <__longjmp_chk@plt>
```

Note that use of `volatile` in `setjmp()` / `longjmp()` effectively
inhibits completely reasonable compiler optimizations just to make loads
and stores in generated code to match the order written in `C` source
code for a given function.

I wonder if adding `volatile` is enough for more heavyweight
optimizations like `-flto` that enable even more code movement, constant
propagation and stack reuse. The time will tell.

## `sagemath` use of `setjmp()` / `longjmp()`

After I noticed `setjmp()` / `longjmp()` use in `sagemath` I wondered how
many `volatile` keywords it has in the code where those were used.

To my surprise it had none. That was a good hint in a sense that it
looked like the problem we are dealing with. From that point I was
reasonably sure it's an application bug and not a `gcc` bug.

Still, having the concrete corruption evidence would help to clear any
doubts and would help the reporter to craft a fix for `sagemath`.

The backtrace claimed that the crash happens in
`__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__`
function:

```
(gdb) continue
Continuing.

Thread 1 "sage-ipython" received signal SIGSEGV, Segmentation fault.
0x00007feb544b756f in _Py_IsImmortal (op=0x0) at /usr/include/python3.12/object.h:242
242         return _Py_CAST(PY_INT32_T, op->ob_refcnt) < 0;
(gdb) bt
#0  0x00007feb544b756f in _Py_IsImmortal (op=0x0) at /usr/include/python3.12/object.h:242
#1  Py_DECREF (op=0x0) at /usr/include/python3.12/object.h:700
#2  Py_XDECREF (op=0x0) at /usr/include/python3.12/object.h:798
#3  __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__ (
    __pyx_v_self=__pyx_v_self@entry=0x7feb4e274f40,
    __pyx_v_args=__pyx_v_args@entry=(<sage.rings.integer.Integer at remote 0x7feb533456e0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd8c0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd620>))
    at /usr/src/debug/sci-mathematics/sagemath-standard-10.3/sagemath-standard-10.3-python3_12/build/cythonized/sage/libs/gap/element.c:26535
...
```

`"__pyx_"` prefix suggests it's a function generated by
[`cython`](https://cython.org/), a python-style DSL for writing `C` part
of code to create bindings for `C` libraries to be used in `python`
libraries. In this case `sagemath` created bindings for `libgap`
discrete algebra library.

The `cython` code for a function we a `SIGSEGV` in looked
[this way](https://github.com/sagemath/sage/blob/744939e037a67193e730d7205e612e2d58197fca/src/sage/libs/gap/element.pyx#L2504):

```python
cdef class GapElement_Function(GapElement):
    # ...
    def __call__(self, *args):
    # ...
        cdef Obj result = NULL
        cdef Obj arg_list
        cdef int n = len(args)

        if n > 0 and n <= 3:
            libgap = self.parent()
            a = [x if isinstance(x, GapElement) else libgap(x) for x in args]

        try:
            sig_GAP_Enter()
            sig_on()
            if n == 0:
                result = GAP_CallFunc0Args(self.value)
            elif n == 1:
                result = GAP_CallFunc1Args(self.value,
                                           (<GapElement>a[0]).value)
            elif n == 2:
                result = GAP_CallFunc2Args(self.value,
                                           (<GapElement>a[0]).value,
                                           (<GapElement>a[1]).value)
            elif n == 3:
                result = GAP_CallFunc3Args(self.value,
                                           (<GapElement>a[0]).value,
                                           (<GapElement>a[1]).value,
                                           (<GapElement>a[2]).value)
            else:
                arg_list = make_gap_list(args)
                result = GAP_CallFuncList(self.value, arg_list)
            sig_off()
        finally:
            GAP_Leave()
        if result == NULL:
            # We called a procedure that does not return anything
            return None
        return make_any_gap_element(self.parent(), result)
```

Looks very straightforward. No `setjmp()` in sight yet, or is there?.

WARNING: you are about to scroll through A Lot Of Code. You might want
to skip the bulk it and return later The build system produces
`element.c` with this contents:

```c
#define sig_GAP_Enter() {int t = GAP_Enter(); if (!t) sig_error();}
#define GAP_Enter() GAP_Error_Setjmp(); GAP_EnterStack()
#define GAP_Error_Setjmp() \
    (GAP_unlikely(GAP_Error_Prejmp_(__FILE__, __LINE__)) \
     || GAP_Error_Postjmp_(_setjmp(*GAP_GetReadJmpError())))
// ...
static PyObject *__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__(struct __pyx_obj_4sage_4libs_3gap_7element_GapElement_Function *__pyx_v_self, PyObject *__pyx_v_args) {
  Obj __pyx_v_result;
  Obj __pyx_v_arg_list;
  int __pyx_v_n;
  PyObject *__pyx_v_libgap = NULL;
  PyObject *__pyx_v_a = NULL;
  PyObject *__pyx_8genexpr3__pyx_v_x = NULL;
  PyObject *__pyx_r = NULL;
  __Pyx_RefNannyDeclarations
  Py_ssize_t __pyx_t_1;
  int __pyx_t_2;
  int __pyx_t_3;
  PyObject *__pyx_t_4 = NULL;
  PyObject *__pyx_t_5 = NULL;
  PyObject *__pyx_t_6 = NULL;
  int __pyx_t_7;
  PyObject *__pyx_t_8 = NULL;
  PyObject *__pyx_t_9 = NULL;
  PyObject *__pyx_t_10 = NULL;
  Obj __pyx_t_11;
  int __pyx_t_12;
  char const *__pyx_t_13;
  PyObject *__pyx_t_14 = NULL;
  PyObject *__pyx_t_15 = NULL;
  PyObject *__pyx_t_16 = NULL;
  PyObject *__pyx_t_17 = NULL;
  PyObject *__pyx_t_18 = NULL;
  PyObject *__pyx_t_19 = NULL;
  int __pyx_lineno = 0;
  const char *__pyx_filename = NULL;
  int __pyx_clineno = 0;
  __Pyx_RefNannySetupContext("__call__", 1);

  /* "sage/libs/gap/element.pyx":2504
 *             hello from the shell
 *         """
 *         cdef Obj result = NULL             # <<<<<<<<<<<<<<
 *         cdef Obj arg_list
 *         cdef int n = len(args)
 */
  __pyx_v_result = NULL;

  /* "sage/libs/gap/element.pyx":2506
 *         cdef Obj result = NULL
 *         cdef Obj arg_list
 *         cdef int n = len(args)             # <<<<<<<<<<<<<<
 * 
 *         if n > 0 and n <= 3:
 */
  __pyx_t_1 = __Pyx_PyTuple_GET_SIZE(__pyx_v_args); if (unlikely(__pyx_t_1 == ((Py_ssize_t)-1))) __PYX_ERR(0, 2506, __pyx_L1_error)
  __pyx_v_n = __pyx_t_1;

  /* "sage/libs/gap/element.pyx":2508
 *         cdef int n = len(args)
 * 
 *         if n > 0 and n <= 3:             # <<<<<<<<<<<<<<
 *             libgap = self.parent()
 *             a = [x if isinstance(x, GapElement) else libgap(x) for x in args]
 */
  __pyx_t_3 = (__pyx_v_n > 0);
  if (__pyx_t_3) {
  } else {
    __pyx_t_2 = __pyx_t_3;
    goto __pyx_L4_bool_binop_done;
  }
  __pyx_t_3 = (__pyx_v_n <= 3);
  __pyx_t_2 = __pyx_t_3;
  __pyx_L4_bool_binop_done:;
  if (__pyx_t_2) {

    /* "sage/libs/gap/element.pyx":2509
 * 
 *         if n > 0 and n <= 3:
 *             libgap = self.parent()             # <<<<<<<<<<<<<<
 *             a = [x if isinstance(x, GapElement) else libgap(x) for x in args]
 * 
 */
    __pyx_t_5 = __Pyx_PyObject_GetAttrStr(((PyObject *)__pyx_v_self), __pyx_n_s_parent); if (unlikely(!__pyx_t_5)) __PYX_ERR(0, 2509, __pyx_L1_error)
    __Pyx_GOTREF(__pyx_t_5);
    __pyx_t_6 = NULL;
    __pyx_t_7 = 0;
    #if CYTHON_UNPACK_METHODS
    if (likely(PyMethod_Check(__pyx_t_5))) {
      __pyx_t_6 = PyMethod_GET_SELF(__pyx_t_5);
      if (likely(__pyx_t_6)) {
        PyObject* function = PyMethod_GET_FUNCTION(__pyx_t_5);
        __Pyx_INCREF(__pyx_t_6);
        __Pyx_INCREF(function);
        __Pyx_DECREF_SET(__pyx_t_5, function);
        __pyx_t_7 = 1;
      }
    }
    #endif
    {
      PyObject *__pyx_callargs[2] = {__pyx_t_6, NULL};
      __pyx_t_4 = __Pyx_PyObject_FastCall(__pyx_t_5, __pyx_callargs+1-__pyx_t_7, 0+__pyx_t_7);
      __Pyx_XDECREF(__pyx_t_6); __pyx_t_6 = 0;
      if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2509, __pyx_L1_error)
      __Pyx_GOTREF(__pyx_t_4);
      __Pyx_DECREF(__pyx_t_5); __pyx_t_5 = 0;
    }
    __pyx_v_libgap = __pyx_t_4;
    __pyx_t_4 = 0;

    /* "sage/libs/gap/element.pyx":2510
 *         if n > 0 and n <= 3:
 *             libgap = self.parent()
 *             a = [x if isinstance(x, GapElement) else libgap(x) for x in args]             # <<<<<<<<<<<<<<
 * 
 *         try:
 */
    { /* enter inner scope */
      __pyx_t_4 = PyList_New(0); if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2510, __pyx_L8_error)
      __Pyx_GOTREF(__pyx_t_4);
      __pyx_t_5 = __pyx_v_args; __Pyx_INCREF(__pyx_t_5);
      __pyx_t_1 = 0;
      for (;;) {
        {
          Py_ssize_t __pyx_temp = __Pyx_PyTuple_GET_SIZE(__pyx_t_5);
          #if !CYTHON_ASSUME_SAFE_MACROS
          if (unlikely((__pyx_temp < 0))) __PYX_ERR(0, 2510, __pyx_L8_error)
          #endif
          if (__pyx_t_1 >= __pyx_temp) break;
        }
        #if CYTHON_ASSUME_SAFE_MACROS && !CYTHON_AVOID_BORROWED_REFS
        __pyx_t_6 = PyTuple_GET_ITEM(__pyx_t_5, __pyx_t_1); __Pyx_INCREF(__pyx_t_6); __pyx_t_1++; if (unlikely((0 < 0))) __PYX_ERR(0, 2510, __pyx_L8_error)
        #else
        __pyx_t_6 = __Pyx_PySequence_ITEM(__pyx_t_5, __pyx_t_1); __pyx_t_1++; if (unlikely(!__pyx_t_6)) __PYX_ERR(0, 2510, __pyx_L8_error)
        __Pyx_GOTREF(__pyx_t_6);
        #endif
        __Pyx_XDECREF_SET(__pyx_8genexpr3__pyx_v_x, __pyx_t_6);
        __pyx_t_6 = 0;
        __pyx_t_2 = __Pyx_TypeCheck(__pyx_8genexpr3__pyx_v_x, __pyx_ptype_4sage_4libs_3gap_7element_GapElement); 
        if (__pyx_t_2) {
          __Pyx_INCREF(__pyx_8genexpr3__pyx_v_x);
          __pyx_t_6 = __pyx_8genexpr3__pyx_v_x;
        } else {
          __Pyx_INCREF(__pyx_v_libgap);
          __pyx_t_9 = __pyx_v_libgap; __pyx_t_10 = NULL;
          __pyx_t_7 = 0;
          #if CYTHON_UNPACK_METHODS
          if (unlikely(PyMethod_Check(__pyx_t_9))) {
            __pyx_t_10 = PyMethod_GET_SELF(__pyx_t_9);
            if (likely(__pyx_t_10)) {
              PyObject* function = PyMethod_GET_FUNCTION(__pyx_t_9);
              __Pyx_INCREF(__pyx_t_10);
              __Pyx_INCREF(function);
              __Pyx_DECREF_SET(__pyx_t_9, function);
              __pyx_t_7 = 1;
            }
          }
          #endif
          {
            PyObject *__pyx_callargs[2] = {__pyx_t_10, __pyx_8genexpr3__pyx_v_x};
            __pyx_t_8 = __Pyx_PyObject_FastCall(__pyx_t_9, __pyx_callargs+1-__pyx_t_7, 1+__pyx_t_7);
            __Pyx_XDECREF(__pyx_t_10); __pyx_t_10 = 0;
            if (unlikely(!__pyx_t_8)) __PYX_ERR(0, 2510, __pyx_L8_error)
            __Pyx_GOTREF(__pyx_t_8);
            __Pyx_DECREF(__pyx_t_9); __pyx_t_9 = 0;
          }
          __pyx_t_6 = __pyx_t_8;
          __pyx_t_8 = 0;
        }
        if (unlikely(__Pyx_ListComp_Append(__pyx_t_4, (PyObject*)__pyx_t_6))) __PYX_ERR(0, 2510, __pyx_L8_error)
        __Pyx_DECREF(__pyx_t_6); __pyx_t_6 = 0;
      }
      __Pyx_DECREF(__pyx_t_5); __pyx_t_5 = 0;
      __Pyx_XDECREF(__pyx_8genexpr3__pyx_v_x); __pyx_8genexpr3__pyx_v_x = 0;
      goto __pyx_L12_exit_scope;
      __pyx_L8_error:;
      __Pyx_XDECREF(__pyx_8genexpr3__pyx_v_x); __pyx_8genexpr3__pyx_v_x = 0;
      goto __pyx_L1_error;
      __pyx_L12_exit_scope:;
    } /* exit inner scope */
    __pyx_v_a = ((PyObject*)__pyx_t_4);
    __pyx_t_4 = 0;

    /* "sage/libs/gap/element.pyx":2508
 *         cdef int n = len(args)
 * 
 *         if n > 0 and n <= 3:             # <<<<<<<<<<<<<<
 *             libgap = self.parent()
 *             a = [x if isinstance(x, GapElement) else libgap(x) for x in args]
 */
  }

  /* "sage/libs/gap/element.pyx":2512
 *             a = [x if isinstance(x, GapElement) else libgap(x) for x in args]
 * 
 *         try:             # <<<<<<<<<<<<<<
 *             sig_GAP_Enter()
 *             sig_on()
 */
  /*try:*/ {

    /* "sage/libs/gap/element.pyx":2513
 * 
 *         try:
 *             sig_GAP_Enter()             # <<<<<<<<<<<<<<
 *             sig_on()
 *             if n == 0:
 */
    sig_GAP_Enter();

    /* "sage/libs/gap/element.pyx":2514
 *         try:
 *             sig_GAP_Enter()
 *             sig_on()             # <<<<<<<<<<<<<<
 *             if n == 0:
 *                 result = GAP_CallFunc0Args(self.value)
 */
    __pyx_t_7 = sig_on(); if (unlikely(__pyx_t_7 == ((int)0))) __PYX_ERR(0, 2514, __pyx_L14_error)

    /* "sage/libs/gap/element.pyx":2515
 *             sig_GAP_Enter()
 *             sig_on()
 *             if n == 0:             # <<<<<<<<<<<<<<
 *                 result = GAP_CallFunc0Args(self.value)
 *             elif n == 1:
 */
    switch (__pyx_v_n) {
      case 0:

      /* "sage/libs/gap/element.pyx":2516
 *             sig_on()
 *             if n == 0:
 *                 result = GAP_CallFunc0Args(self.value)             # <<<<<<<<<<<<<<
 *             elif n == 1:
 *                 result = GAP_CallFunc1Args(self.value,
 */
      __pyx_v_result = GAP_CallFunc0Args(__pyx_v_self->__pyx_base.value);

      /* "sage/libs/gap/element.pyx":2515
 *             sig_GAP_Enter()
 *             sig_on()
 *             if n == 0:             # <<<<<<<<<<<<<<
 *                 result = GAP_CallFunc0Args(self.value)
 *             elif n == 1:
 */
      break;
      case 1:

      /* "sage/libs/gap/element.pyx":2519
 *             elif n == 1:
 *                 result = GAP_CallFunc1Args(self.value,
 *                                            (<GapElement>a[0]).value)             # <<<<<<<<<<<<<<
 *             elif n == 2:
 *                 result = GAP_CallFunc2Args(self.value,
 */
      if (unlikely(!__pyx_v_a)) { __Pyx_RaiseUnboundLocalError("a"); __PYX_ERR(0, 2519, __pyx_L14_error) }
      __pyx_t_4 = __Pyx_GetItemInt_List(__pyx_v_a, 0, long, 1, __Pyx_PyInt_From_long, 1, 0, 1); if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2519, __pyx_L14_error)
      __Pyx_GOTREF(__pyx_t_4);

      /* "sage/libs/gap/element.pyx":2518
 *                 result = GAP_CallFunc0Args(self.value)
 *             elif n == 1:
 *                 result = GAP_CallFunc1Args(self.value,             # <<<<<<<<<<<<<<
 *                                            (<GapElement>a[0]).value)
 *             elif n == 2:
 */
      __pyx_v_result = GAP_CallFunc1Args(__pyx_v_self->__pyx_base.value, ((struct __pyx_obj_4sage_4libs_3gap_7element_GapElement *)__pyx_t_4)->value);
      __Pyx_DECREF(__pyx_t_4); __pyx_t_4 = 0;

      /* "sage/libs/gap/element.pyx":2517
 *             if n == 0:
 *                 result = GAP_CallFunc0Args(self.value)
 *             elif n == 1:             # <<<<<<<<<<<<<<
 *                 result = GAP_CallFunc1Args(self.value,
 *                                            (<GapElement>a[0]).value)
 */
      break;
      case 2:

      /* "sage/libs/gap/element.pyx":2522
 *             elif n == 2:
 *                 result = GAP_CallFunc2Args(self.value,
 *                                            (<GapElement>a[0]).value,             # <<<<<<<<<<<<<<
 *                                            (<GapElement>a[1]).value)
 *             elif n == 3:
 */
      if (unlikely(!__pyx_v_a)) { __Pyx_RaiseUnboundLocalError("a"); __PYX_ERR(0, 2522, __pyx_L14_error) }
      __pyx_t_4 = __Pyx_GetItemInt_List(__pyx_v_a, 0, long, 1, __Pyx_PyInt_From_long, 1, 0, 1); if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2522, __pyx_L14_error)
      __Pyx_GOTREF(__pyx_t_4);

      /* "sage/libs/gap/element.pyx":2523
 *                 result = GAP_CallFunc2Args(self.value,
 *                                            (<GapElement>a[0]).value,
 *                                            (<GapElement>a[1]).value)             # <<<<<<<<<<<<<<
 *             elif n == 3:
 *                 result = GAP_CallFunc3Args(self.value,
 */
      if (unlikely(!__pyx_v_a)) { __Pyx_RaiseUnboundLocalError("a"); __PYX_ERR(0, 2523, __pyx_L14_error) }
      __pyx_t_5 = __Pyx_GetItemInt_List(__pyx_v_a, 1, long, 1, __Pyx_PyInt_From_long, 1, 0, 1); if (unlikely(!__pyx_t_5)) __PYX_ERR(0, 2523, __pyx_L14_error)
      __Pyx_GOTREF(__pyx_t_5);

      /* "sage/libs/gap/element.pyx":2521
 *                                            (<GapElement>a[0]).value)
 *             elif n == 2:
 *                 result = GAP_CallFunc2Args(self.value,             # <<<<<<<<<<<<<<
 *                                            (<GapElement>a[0]).value,
 *                                            (<GapElement>a[1]).value)
 */
      __pyx_v_result = GAP_CallFunc2Args(__pyx_v_self->__pyx_base.value, ((struct __pyx_obj_4sage_4libs_3gap_7element_GapElement *)__pyx_t_4)->value, ((struct __pyx_obj_4sage_4libs_3gap_7element_GapElement *)__pyx_t_5)->value);
      __Pyx_DECREF(__pyx_t_4); __pyx_t_4 = 0;
      __Pyx_DECREF(__pyx_t_5); __pyx_t_5 = 0;

      /* "sage/libs/gap/element.pyx":2520
 *                 result = GAP_CallFunc1Args(self.value,
 *                                            (<GapElement>a[0]).value)
 *             elif n == 2:             # <<<<<<<<<<<<<<
 *                 result = GAP_CallFunc2Args(self.value,
 *                                            (<GapElement>a[0]).value,
 */
      break;
      case 3:

      /* "sage/libs/gap/element.pyx":2526
 *             elif n == 3:
 *                 result = GAP_CallFunc3Args(self.value,
 *                                            (<GapElement>a[0]).value,             # <<<<<<<<<<<<<<
 *                                            (<GapElement>a[1]).value,
 *                                            (<GapElement>a[2]).value)
 */
      if (unlikely(!__pyx_v_a)) { __Pyx_RaiseUnboundLocalError("a"); __PYX_ERR(0, 2526, __pyx_L14_error) }
      __pyx_t_5 = __Pyx_GetItemInt_List(__pyx_v_a, 0, long, 1, __Pyx_PyInt_From_long, 1, 0, 1); if (unlikely(!__pyx_t_5)) __PYX_ERR(0, 2526, __pyx_L14_error)
      __Pyx_GOTREF(__pyx_t_5);

      /* "sage/libs/gap/element.pyx":2527
 *                 result = GAP_CallFunc3Args(self.value,
 *                                            (<GapElement>a[0]).value,
 *                                            (<GapElement>a[1]).value,             # <<<<<<<<<<<<<<
 *                                            (<GapElement>a[2]).value)
 *             else:
 */
      if (unlikely(!__pyx_v_a)) { __Pyx_RaiseUnboundLocalError("a"); __PYX_ERR(0, 2527, __pyx_L14_error) }
      __pyx_t_4 = __Pyx_GetItemInt_List(__pyx_v_a, 1, long, 1, __Pyx_PyInt_From_long, 1, 0, 1); if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2527, __pyx_L14_error)
      __Pyx_GOTREF(__pyx_t_4);

      /* "sage/libs/gap/element.pyx":2528
 *                                            (<GapElement>a[0]).value,
 *                                            (<GapElement>a[1]).value,
 *                                            (<GapElement>a[2]).value)             # <<<<<<<<<<<<<<
 *             else:
 *                 arg_list = make_gap_list(args)
 */
      if (unlikely(!__pyx_v_a)) { __Pyx_RaiseUnboundLocalError("a"); __PYX_ERR(0, 2528, __pyx_L14_error) }
      __pyx_t_6 = __Pyx_GetItemInt_List(__pyx_v_a, 2, long, 1, __Pyx_PyInt_From_long, 1, 0, 1); if (unlikely(!__pyx_t_6)) __PYX_ERR(0, 2528, __pyx_L14_error)
      __Pyx_GOTREF(__pyx_t_6);

      /* "sage/libs/gap/element.pyx":2525
 *                                            (<GapElement>a[1]).value)
 *             elif n == 3:
 *                 result = GAP_CallFunc3Args(self.value,             # <<<<<<<<<<<<<<
 *                                            (<GapElement>a[0]).value,
 *                                            (<GapElement>a[1]).value,
 */
      __pyx_v_result = GAP_CallFunc3Args(__pyx_v_self->__pyx_base.value, ((struct __pyx_obj_4sage_4libs_3gap_7element_GapElement *)__pyx_t_5)->value, ((struct __pyx_obj_4sage_4libs_3gap_7element_GapElement *)__pyx_t_4)->value, ((struct __pyx_obj_4sage_4libs_3gap_7element_GapElement *)__pyx_t_6)->value);
      __Pyx_DECREF(__pyx_t_5); __pyx_t_5 = 0;
      __Pyx_DECREF(__pyx_t_4); __pyx_t_4 = 0;
      __Pyx_DECREF(__pyx_t_6); __pyx_t_6 = 0;

      /* "sage/libs/gap/element.pyx":2524
 *                                            (<GapElement>a[0]).value,
 *                                            (<GapElement>a[1]).value)
 *             elif n == 3:             # <<<<<<<<<<<<<<
 *                 result = GAP_CallFunc3Args(self.value,
 *                                            (<GapElement>a[0]).value,
 */
      break;
      default:

      /* "sage/libs/gap/element.pyx":2530
 *                                            (<GapElement>a[2]).value)
 *             else:
 *                 arg_list = make_gap_list(args)             # <<<<<<<<<<<<<<
 *                 result = GAP_CallFuncList(self.value, arg_list)
 *             sig_off()
 */
      __pyx_t_11 = __pyx_f_4sage_4libs_3gap_7element_make_gap_list(__pyx_v_args); if (unlikely(__pyx_t_11 == ((Obj)NULL))) __PYX_ERR(0, 2530, __pyx_L14_error)
      __pyx_v_arg_list = __pyx_t_11;

      /* "sage/libs/gap/element.pyx":2531
 *             else:
 *                 arg_list = make_gap_list(args)
 *                 result = GAP_CallFuncList(self.value, arg_list)             # <<<<<<<<<<<<<<
 *             sig_off()
 *         finally:
 */
      __pyx_v_result = GAP_CallFuncList(__pyx_v_self->__pyx_base.value, __pyx_v_arg_list);
      break;
    }

    /* "sage/libs/gap/element.pyx":2532
 *                 arg_list = make_gap_list(args)
 *                 result = GAP_CallFuncList(self.value, arg_list)
 *             sig_off()             # <<<<<<<<<<<<<<
 *         finally:
 *             GAP_Leave()
 */
    sig_off();
  }

  /* "sage/libs/gap/element.pyx":2534
 *             sig_off()
 *         finally:
 *             GAP_Leave()             # <<<<<<<<<<<<<<
 *         if result == NULL:
 *             # We called a procedure that does not return anything
 */
  /*finally:*/ {
    /*normal exit:*/{
      GAP_Leave();
      goto __pyx_L15;
    }
    __pyx_L14_error:;
    /*exception exit:*/{
      __Pyx_PyThreadState_declare
      __Pyx_PyThreadState_assign
      __pyx_t_14 = 0; __pyx_t_15 = 0; __pyx_t_16 = 0; __pyx_t_17 = 0; __pyx_t_18 = 0; __pyx_t_19 = 0;
      __Pyx_XDECREF(__pyx_t_10); __pyx_t_10 = 0;
      __Pyx_XDECREF(__pyx_t_4); __pyx_t_4 = 0;
      __Pyx_XDECREF(__pyx_t_5); __pyx_t_5 = 0;
      __Pyx_XDECREF(__pyx_t_6); __pyx_t_6 = 0;
      __Pyx_XDECREF(__pyx_t_8); __pyx_t_8 = 0;
      __Pyx_XDECREF(__pyx_t_9); __pyx_t_9 = 0;
      if (PY_MAJOR_VERSION >= 3) __Pyx_ExceptionSwap(&__pyx_t_17, &__pyx_t_18, &__pyx_t_19);
      if ((PY_MAJOR_VERSION < 3) || unlikely(__Pyx_GetException(&__pyx_t_14, &__pyx_t_15, &__pyx_t_16) < 0)) __Pyx_ErrFetch(&__pyx_t_14, &__pyx_t_15, &__pyx_t_16);
      __Pyx_XGOTREF(__pyx_t_14);
      __Pyx_XGOTREF(__pyx_t_15);
      __Pyx_XGOTREF(__pyx_t_16);
      __Pyx_XGOTREF(__pyx_t_17);
      __Pyx_XGOTREF(__pyx_t_18);
      __Pyx_XGOTREF(__pyx_t_19);
      __pyx_t_7 = __pyx_lineno; __pyx_t_12 = __pyx_clineno; __pyx_t_13 = __pyx_filename;
      {
        GAP_Leave();
      }
      if (PY_MAJOR_VERSION >= 3) {
        __Pyx_XGIVEREF(__pyx_t_17);
        __Pyx_XGIVEREF(__pyx_t_18);
        __Pyx_XGIVEREF(__pyx_t_19);
        __Pyx_ExceptionReset(__pyx_t_17, __pyx_t_18, __pyx_t_19);
      }
      __Pyx_XGIVEREF(__pyx_t_14);
      __Pyx_XGIVEREF(__pyx_t_15);
      __Pyx_XGIVEREF(__pyx_t_16);
      __Pyx_ErrRestore(__pyx_t_14, __pyx_t_15, __pyx_t_16);
      __pyx_t_14 = 0; __pyx_t_15 = 0; __pyx_t_16 = 0; __pyx_t_17 = 0; __pyx_t_18 = 0; __pyx_t_19 = 0;
      __pyx_lineno = __pyx_t_7; __pyx_clineno = __pyx_t_12; __pyx_filename = __pyx_t_13;
      goto __pyx_L1_error;
    }
    __pyx_L15:;
  }

  /* "sage/libs/gap/element.pyx":2535
 *         finally:
 *             GAP_Leave()
 *         if result == NULL:             # <<<<<<<<<<<<<<
 *             # We called a procedure that does not return anything
 *             return None
 */
  __pyx_t_2 = (__pyx_v_result == NULL);
  if (__pyx_t_2) {

    /* "sage/libs/gap/element.pyx":2537
 *         if result == NULL:
 *             # We called a procedure that does not return anything
 *             return None             # <<<<<<<<<<<<<<
 *         return make_any_gap_element(self.parent(), result)
 * 
 */
    __Pyx_XDECREF(__pyx_r);
    __pyx_r = Py_None; __Pyx_INCREF(Py_None);
    goto __pyx_L0;

    /* "sage/libs/gap/element.pyx":2535
 *         finally:
 *             GAP_Leave()
 *         if result == NULL:             # <<<<<<<<<<<<<<
 *             # We called a procedure that does not return anything
 *             return None
 */
  }

  /* "sage/libs/gap/element.pyx":2538
 *             # We called a procedure that does not return anything
 *             return None
 *         return make_any_gap_element(self.parent(), result)             # <<<<<<<<<<<<<<
 * 
 * 
 */
  __Pyx_XDECREF(__pyx_r);
  __pyx_t_4 = __Pyx_PyObject_GetAttrStr(((PyObject *)__pyx_v_self), __pyx_n_s_parent); if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2538, __pyx_L1_error)
  __Pyx_GOTREF(__pyx_t_4);
  __pyx_t_5 = NULL;
  __pyx_t_12 = 0;
  #if CYTHON_UNPACK_METHODS
  if (likely(PyMethod_Check(__pyx_t_4))) {
    __pyx_t_5 = PyMethod_GET_SELF(__pyx_t_4);
    if (likely(__pyx_t_5)) {
      PyObject* function = PyMethod_GET_FUNCTION(__pyx_t_4);
      __Pyx_INCREF(__pyx_t_5);
      __Pyx_INCREF(function);
      __Pyx_DECREF_SET(__pyx_t_4, function);
      __pyx_t_12 = 1;
    }
  }
  #endif
  {
    PyObject *__pyx_callargs[2] = {__pyx_t_5, NULL};
    __pyx_t_6 = __Pyx_PyObject_FastCall(__pyx_t_4, __pyx_callargs+1-__pyx_t_12, 0+__pyx_t_12);
    __Pyx_XDECREF(__pyx_t_5); __pyx_t_5 = 0;
    if (unlikely(!__pyx_t_6)) __PYX_ERR(0, 2538, __pyx_L1_error)
    __Pyx_GOTREF(__pyx_t_6);
    __Pyx_DECREF(__pyx_t_4); __pyx_t_4 = 0;
  }
  __pyx_t_4 = ((PyObject *)__pyx_f_4sage_4libs_3gap_7element_make_any_gap_element(__pyx_t_6, __pyx_v_result)); if (unlikely(!__pyx_t_4)) __PYX_ERR(0, 2538, __pyx_L1_error)
  __Pyx_GOTREF(__pyx_t_4);
  __Pyx_DECREF(__pyx_t_6); __pyx_t_6 = 0;
  __pyx_r = __pyx_t_4;
  __pyx_t_4 = 0;
  goto __pyx_L0;

  /* "sage/libs/gap/element.pyx":2412
 * 
 * 
 *     def __call__(self, *args):             # <<<<<<<<<<<<<<
 *         """
 *         Call syntax for functions.
 */

  /* function exit code */
  __pyx_L1_error:;
  __Pyx_XDECREF(__pyx_t_4);
  __Pyx_XDECREF(__pyx_t_5);
  __Pyx_XDECREF(__pyx_t_6);
  __Pyx_XDECREF(__pyx_t_8);
  __Pyx_XDECREF(__pyx_t_9);
  __Pyx_XDECREF(__pyx_t_10);
  __Pyx_AddTraceback("sage.libs.gap.element.GapElement_Function.__call__", __pyx_clineno, __pyx_lineno, __pyx_filename);
  __pyx_r = NULL;
  __pyx_L0:;
  __Pyx_XDECREF(__pyx_v_libgap);
  __Pyx_XDECREF(__pyx_v_a);
  __Pyx_XDECREF(__pyx_8genexpr3__pyx_v_x);
  __Pyx_XGIVEREF(__pyx_r);
  __Pyx_RefNannyFinishContext();
  return __pyx_r;
}
```

`cython` does a great job annotating generated code with corresponding
source code. Very useful! While it's a lot of boilerplate the code is
straightforward.

A few important facts:

1. `sig_GAP_Enter()` is our `setjmp()` in disguise of a few
   macros defined at the beginning of the file. You can't see `longjmp()`
   calls here but they are lurking in various `GAP_CallFunc3Args()` calls.

2. There is a ton of local variables being updated in this file. And
   none of them has `volatile` annotations.

The above strongly suggests we are hitting a `volatile` `setjmp()`
`CAVEAT`. But how to find out for sure?

## Nailing down the specific variable corruption

The general "missing `volatile`" hint is a bit abstract. Is it easy to
find out what variable is being corrupted here? The `gdb` is
surprisingly useful here. Let's look at our `SIGSEGV` again:

```
(gdb) bt
#0  0x00007feb544b756f in _Py_IsImmortal (op=0x0) at /usr/include/python3.12/object.h:242
#1  Py_DECREF (op=0x0) at /usr/include/python3.12/object.h:700
#2  Py_XDECREF (op=0x0) at /usr/include/python3.12/object.h:798
#3  __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__ (
    __pyx_v_self=__pyx_v_self@entry=0x7feb4e274f40,
    __pyx_v_args=__pyx_v_args@entry=(<sage.rings.integer.Integer at remote 0x7feb533456e0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd8c0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd620>))
    at /usr/src/debug/sci-mathematics/sagemath-standard-10.3/sagemath-standard-10.3-python3_12/build/cythonized/sage/libs/gap/element.c:26535
#4  0x00007feb544b84e7 in __pyx_pw_4sage_4libs_3gap_7element_19GapElement_Function_3__call__ (
    __pyx_v_self=<sage.libs.gap.element.GapElement_Function at remote 0x7feb4e274f40>,
    __pyx_args=(<sage.rings.integer.Integer at remote 0x7feb533456e0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd8c0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd620>),
    __pyx_kwds=<optimized out>)
    at /usr/src/debug/sci-mathematics/sagemath-standard-10.3/sagemath-standard-10.3-python3_12/build/cythonized/sage/libs/gap/element.c:26105
```

Our function is in `frame 3` right now. Let's look at it:

```
(gdb) fr 3
#3  __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__ (
    __pyx_v_self=__pyx_v_self@entry=0x7feb4e274f40,
    __pyx_v_args=__pyx_v_args@entry=(<sage.rings.integer.Integer at remote 0x7feb533456e0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd8c0>, <sage.rings.integer.Integer at remote 0x7feb4e5dd620>))
    at /usr/src/debug/sci-mathematics/sagemath-standard-10.3/sagemath-standard-10.3-python3_12/build/cythonized/sage/libs/gap/element.c:26535
list
26535         __Pyx_XDECREF(__pyx_t_6); __pyx_t_6 = 0;
```

`__Pyx_XDECREF()` looks promising.

```
(gdb) list
26530         __Pyx_PyThreadState_assign
26531         __pyx_t_14 = 0; __pyx_t_15 = 0; __pyx_t_16 = 0; __pyx_t_17 = 0; __pyx_t_18 = 0; __pyx_t_19 = 0;
26532         __Pyx_XDECREF(__pyx_t_10); __pyx_t_10 = 0;
26533         __Pyx_XDECREF(__pyx_t_4); __pyx_t_4 = 0;
26534         __Pyx_XDECREF(__pyx_t_5); __pyx_t_5 = 0;
26535         __Pyx_XDECREF(__pyx_t_6); __pyx_t_6 = 0;
26536         __Pyx_XDECREF(__pyx_t_8); __pyx_t_8 = 0;
26537         __Pyx_XDECREF(__pyx_t_9); __pyx_t_9 = 0;
26538         if (PY_MAJOR_VERSION >= 3) __Pyx_ExceptionSwap(&__pyx_t_17, &__pyx_t_18, &__pyx_t_19);
26539         if ((PY_MAJOR_VERSION < 3) || unlikely(__Pyx_GetException(&__pyx_t_14, &__pyx_t_15, &__pyx_t_16) < 0)) __Pyx_ErrFetch(&__pyx_t_14, &__pyx_t_15, &__pyx_t_16);
```

`gdb` says that crash happens at line
`26535 __Pyx_XDECREF(__pyx_t_6); __pyx_t_6 = 0;`. Thus `__pyx_t_6 = 0;`
is our primary suspect.

To trace the life of `__pyx_t_6` in assembly code I built `element.c`
with `-S -fverbose-asm` flags and got this tiny snippet of variable
reference:

```asm
# /usr/include/python3.12/object.h:242:     return _Py_CAST(PY_INT32_T, op->ob_refcnt) < 0;
    .loc 5 242 12 is_stmt 0 view .LVU66168
    movq -200(%rbp), %rdx # %sfp, r
    movq (%rdx), %rax # __pyx_t_6_10(ab)->D.11083.ob_refcnt, _1070
```

It's the first few instructions of `__Pyx_XDECREF()` implementation.

The main take away here is that `__pyx_t_6` is stored on stack at the
address `%rbp-200`. We can trace all the updates at that location and
see what is missing.

Before doing that I navigated to the beginning of crashing
`__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__`
function as:

```
$ gdb -p `pgrep sage-ipython`
(gdb) break __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__
(gdb) continue

    # trigger break with with ` libgap.AbelianGroup(0,0,0)`

(gdb) disassemble
Dump of assembler code for function __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__:
=> 0x00007f4ed9981b60 <+0>:     push   %rbp
   0x00007f4ed9981b61 <+1>:     mov    %rsp,%rbp
```

And then executed first two instructions to initialize `%rbp`
register (as our variable lives at a constant offset from `%rbp` on
stack):

```
(gdb) nexti
(gdb) nexti
(gdb) disassemble
Dump of assembler code for function __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__:
   0x00007f4ed9981b60 <+0>:     push   %rbp
   0x00007f4ed9981b61 <+1>:     mov    %rsp,%rbp
=> 0x00007f4ed9981b64 <+4>:     push   %r15
```

Now I set the watch point at stack memory where we expect `__pyx_t_6` to
reside:

```
(gdb) print $rbp-200
$2 = (void *) 0x7ffd2824c5e8

(gdb) watch *(int*)(void *) 0x7ffd2824c5e8
Hardware watchpoint 2: *(int*)(void *) 0x7ffd2824c5e8

(gdb) continue
Continuing.

Thread 1 "sage-ipython" hit Hardware watchpoint 2: *(int*)(void *) 0x7ffd2824c5e8

Old value = 673498624
New value = 0
0x00007f98e609d2a8 in __pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__ (
    __pyx_v_self=__pyx_v_self@entry=0x7f98dfe70dc0,
    __pyx_v_args=__pyx_v_args@entry=(<sage.rings.integer.Integer at remote 0x7f98e4722c40>, <sage.rings.integer.Integer at remote 0x7f98dfe7afd0>, <sage.rings.integer.Integer at remote 0x7f98e01dd5c0>))
    at /usr/src/debug/sci-mathematics/sagemath-standard-10.3/sagemath-standard-10.3-python3_12/build/cythonized/sage/libs/gap/element.c:26192
26192       __pyx_t_6 = NULL;
```

This break is our initial `__pyx_t_6 = NULL;` store. Nothing unusual.
Moving to the next update:

```
(gdb) continue
Continuing.

Thread 1 "sage-ipython" hit Hardware watchpoint 2: *(int*)(void *) 0x7ffd2824c5e8

Old value = 0
New value = -538669696
__Pyx_GetItemInt_List_Fast (wraparound=0, boundscheck=1, i=2,
    o=[<sage.libs.gap.element.GapElement_Integer at remote 0x7f98e0ac5c00>, <sage.libs.gap.element.GapElement_Integer at remote 0x7f98dfe4b500>, <sage.libs.gap.element.GapElement_Integer at remote 0x7f98dfe48d80>])
    at /usr/src/debug/sci-mathematics/sagemath-standard-10.3/sagemath-standard-10.3-python3_12/build/cythonized/sage/libs/gap/element.c:38070
38070           Py_INCREF(r);
```

Here we create an object and increment a reference via `__pyx_t_6` address.
Moving on:

```
(gdb) continue
Continuing.

Thread 1 "sage-ipython" received signal SIGABRT, Aborted.
0x00007f99428617a7 in __GI_kill () at ../sysdeps/unix/syscall-template.S:120
120     T_PSEUDO (SYSCALL_SYMBOL, SYSCALL_NAME, SYSCALL_NARGS)
```

Got to `SIGABRT` signal, `longjmp()` is about to be called. Moving on.

```
(gdb) continue
Continuing.

Thread 1 "sage-ipython" received signal SIGSEGV, Segmentation fault.
0x00007f98e609c56f in _Py_IsImmortal (op=0x0) at /usr/include/python3.12/object.h:242
242         return _Py_CAST(PY_INT32_T, op->ob_refcnt) < 0;
```

We arrived at a final `SIGSEGV` signal.

Curiously we see only two stores (one `NULL` store and one
non-`NULL` store) at inspected address. Both are before the `SIGABRT`
signal (and thus `longjmp()` call). I was initially afraid that stack
got corrupted by stack reuse in nested functions. But it's not the case
and our case is a lot simpler than it could be. Phew.

This session allowed me to finally understand the control flow happening
here.

## `sagemath` breakage mechanics

Armed with `__pyx_t_6` `runtime` behaviour I think I got the properties
`__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__` had
to force `gcc` into `SIGSEGV`.

Slightly oversimplified function has the following form:

```c
__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__() {
    __pyx_t_6 = NULL;

    int mode = setjmp(jb);

    switch (mode) {
      case 1:                                  // longjmp() case
        break;
      case 0:                                  // regular case (`case 3:` in real code)
        __pyx_t_6 = something_else();          // set __pyx_t_6 to non-zero
        int done = use_post_setjmp(__pyx_t_6); // call longjmp(jb, 1) here
        __pyx_t_6 = NULL;
        break;
    }

    // get here via longjmp() or via natural execution flow
    //
    // Note: natural execution flow always gets here with `__pyx_t_6 = NULL`.
    if (__pyx_t_6 != NULL) deref(__pyx_t_6);
}
```

Similar to our contrived example we save the function state at the
beginning of a function and return back to it from `use_post_setjmp()`
helper after we changed the value of a local variable.

`__pyx_t_6` is not marked `volatile`. `gcc` noticed that `__pyx_t_6` is
always expected to be `NULL` when it reaches the final statement . And
`gcc` optimizes the function code into the following equivalent:

```c
__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__() {
    __pyx_t_6 = NULL;

    int mode = setjmp(jb);

    switch (mode) {
      case 1:                                  // longjmp() case
        break;
      case 0:                                  // regular case (`case 3:` in real code)
        __pyx_t_6 = something_else();          // set __pyx_t_6 to non-zero
        int done = use_post_setjmp(__pyx_t_6); // call longjmp(jb, 1) here
        __pyx_t_6 = NULL;
        break;
    }

    // Constant-fold `__pyx_t_6 = NULL` in `deref()` call site
    if (__pyx_t_6 != NULL) deref(NULL);
}
```

Note that `gcc` did not eliminate the `if (__pyx_t_6 != NULL)` even
though the condition is always expected to be `false` (Richard explains
it [here](https://gcc.gnu.org/PR114872#c24)).

The presence of `longjmp()` effectively skips the
`__pyx_t_6 = NULL;` assignment and executes `if (__pyx_t_6 != NULL) deref(NULL);`.
That leads to `SIGSEGV`.

Phew. We finally have the breakage mechanics caused by `gcc` code motion.

This bug is known for a while in `sagemath` bug tracker as an
[Issue#37026](https://github.com/sagemath/sage/issues/37026).

Fixing it will probably require adding `volatile` marking to quite a few
temporary variables in `.pyx` files of `sagemath`. Or alternatively
avoid the `setjmp()` / `longjmp()` pattern in inner functions if
feasible. They are just too large to be auditable for `setjmp()`
constraints.

## Bonus: how `setjmp()` / `longjmp()` works in `glibc`

Let's have a look what `glibc` does `<S-Del>` on `x86_64` at
[`sysdeps/x86_64/setjmp.S`](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/x86_64/setjmp.S;h=40807e73b51c362464730212d7c973848be612bf;hb=HEAD):

```asm
ENTRY (__sigsetjmp)
        /* Save registers.  */
        movq %rbx, (JB_RBX*8)(%rdi)
#ifdef PTR_MANGLE
        mov %RBP_LP, %RAX_LP
        PTR_MANGLE (%RAX_LP)
        mov %RAX_LP, (JB_RBP*8)(%rdi)
#else
        movq %rbp, (JB_RBP*8)(%rdi)
#endif
        movq %r12, (JB_R12*8)(%rdi)
        movq %r13, (JB_R13*8)(%rdi)
        movq %r14, (JB_R14*8)(%rdi)
        movq %r15, (JB_R15*8)(%rdi)
        lea 8(%rsp), %RDX_LP    /* Save SP as it will be after we return.  */
#ifdef PTR_MANGLE
        PTR_MANGLE (%RDX_LP)
#endif
        movq %rdx, (JB_RSP*8)(%rdi)
        mov (%rsp), %RAX_LP     /* Save PC we are returning to now.  */
        LIBC_PROBE (setjmp, 3, LP_SIZE@%RDI_LP, -4@%esi, LP_SIZE@%RAX_LP)
#ifdef PTR_MANGLE
        PTR_MANGLE (%RAX_LP)
#endif
        movq %rax, (JB_PC*8)(%rdi)

        /* Make a tail call to __sigjmp_save; it takes the same args.  */
        jmp __sigjmp_save
END (__sigsetjmp)
```

`%rdi` is our `env` parameter in `int setjmp(jmp_buf env);` signature.

`__sigsetjmp` does a few things:

- is saves `r12`, `r13`, `r14`, `r15`, `rbx`, `rsp`, `rbp` registers into
  `jmp_buf env` parameter. These registers happen to be all `callee-save`
  registers on `System V x86_64 ABI`. Any `ABI`-conformant `C` function
  does the same if it plans to use these registers locally.
- the only twist is that some of the stack-related registers are
  obfuscated with `PTR_MANGLE` macro. The macro mixes in stack canary
  into the value.

- `__sigsetjmp` also saves return address as `mov (%rsp), %RAX_LP` to be
  able to resume from it later.

- `__sigsetjmp` also handles shadow call stack if that exists, I skipped
  it entirely for simplicity

Importantly `__sigsetjmp` does not save any stack contents. It assumes
that `gcc` and the code author make sure it does not get corrupted on
return.

The `longjmp()` recovery is a mirror-image of `setjmp()`
in [`sysdeps/x86_64/__longjmp.S`](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/x86_64/__longjmp.S;h=22fedc49970eba0ab86cb8dec8411197bec95d37;hb=HEAD). I'll skip
pasting `longjmp()` implementation here.

## Bonus 2: `-Wclobbered` is able to detect some of the clobber cases

Alexander Monakov [pointed out](https://fosstodon.org/@amonakov@mastodon.gamedev.place/112429019564307874)
that `gcc` actually has an option to detect some of the clobbering cases
with `-Wclobbered`.

On a contrived example it reports nothing:

```
$ gcc -O2 -c example.c -Wclobbered -Wall -Wextra -W
```

It's unfortunate as we can clearly see the different output.

But on the real `element.c` it complains as:

```
$ gcc -O2 -Wclobbered -c element.i
...
element.c: In function '__pyx_pf_4sage_4libs_3gap_7element_19GapElement_Function_2__call__':
element.c:26122:13: warning: variable '__pyx_v_libgap' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26123:13: warning: variable '__pyx_v_a' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26125:13: warning: variable '__pyx_r' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26130:13: warning: variable '__pyx_t_4' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26131:13: warning: variable '__pyx_t_5' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26132:13: warning: variable '__pyx_t_6' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26134:13: warning: variable '__pyx_t_8' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26135:13: warning: variable '__pyx_t_9' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:26136:13: warning: variable '__pyx_t_10' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:38074:19: warning: variable 'r' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
element.c:38074:19: warning: variable 'r' might be clobbered by 'longjmp' or 'vfork' [-Wclobbered]
```

And that includes our `__pyx_t_6` variable!

## Parting words

Python ecosystem did not fully update to latest `python` release:
`python-3.12` was out in October 2023 and many packages did not yet
catch up with it. That makes it tedious to fix a long tail of issues
like `sagemath` crashes. Also makes it hard to reproduce issues on some
distributions that follow upstream packages without much patching.

While it was a bit unfortunate that `sagemath` is not packaged in
`::gentoo` I was pleasantly surprised by the responsive maintainer of
`::sage-on-gentoo` overlay. I managed to build `sagemath-standard` with
their help relatively quickly.

`scikit-build-core` package is invasive and is able to break the build
of packages that don't import it directly or indirectly, like
[`sagemath-standard`](https://github.com/cschwan/sage-on-gentoo/issues/783).

`cysignals` in `::gentoo` has a [broken path](https://bugs.gentoo.org/927767)
to the `gdb` helper which makes it more tedious to debug `sagemath`
crashes.

`cysignals` uses `SIGABRT` and `setjmp()` / `longjmp()` to recover from
errors. `sagemath` decided to use it in `C` bindings. Unfortunately it
does not mix well with `cython`'s use of local variables and leads to
broken code.

I was lucky to look at the `SIGABRT` handler first to notice `longjmp()`
there. If `gdb` hook would not fail for a wrong `share` path I would
take me a lot more time to discover `setjmp()` clue.

We were lucky that `gcc` did not delete `NULL`-dereference code and
generated something that `SIGSEGV`s on execution. Otherwise it would be
at best resource leak and use-after-free or data corruption at worst.

`setjmp()` / `longjmp()` is very hard to use correctly in large
functions. One has to be very careful to sprinkle enough `volatile`
keywords to inhibit enough compiler optimizations to get expected
semantics from the program.

It was not a compiler bug after all but the `sagemath` one. It was
interaction between two aspects `sagemath` used:

- `cython` usage that generates a ton of local variables it mutates
  along the way without the `volatile` annotations
- use of `cysignals`' `sig_GAP_Enter()` (aka `setjmp()`) error recovery
  in the same function

`gcc` can detect some of the clobber cases with `-Wclobbered` flag.

Have fun!

