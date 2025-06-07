---
title: "Delay Slots strike back"
date: February 27, 2021
---

Jeroen found another problem on `hppa` (`PA-RISC`) after switch to
`gcc-10`: `Python-3.9.0b3` started crashing at build time:

``` 
$ ./configure ... --host=hppa2.0-unknown-linux-gnu ...
$ make
...

if test $? -ne 0 ; then \
echo "generate-posix-vars failed" ; \
rm -f ./pybuilddir.txt ; \
exit 1 ; \
fi
Python path configuration:
  PYTHONHOME = (not set)
  PYTHONPATH = (not set)
  program name = './python'
  isolated = 0
  environment = 0
  user site = 1
  import site = 0
  sys._base_executable = '/var/tmp/portage/dev-lang/python-3.9.0_beta3/work/Python-3.9.0b3/python'
  sys.base_prefix = '/usr'
  sys.base_exec_prefix = '/usr'
  sys.platlibdir = 'lib'
  sys.executable = '/var/tmp/portage/dev-lang/python-3.9.0_beta3/work/Python-3.9.0b3/python'
  sys.prefix = '/usr'
  sys.exec_prefix = '/usr'
  sys.path = [
    '/usr/lib/python39.zip',
    '/var/tmp/portage/dev-lang/python-3.9.0_beta3/work/Python-3.9.0b3/Lib',
    '/var/tmp/portage/dev-lang/python-3.9.0_beta3/work/Python-3.9.0b3/none\n',
  ]
Fatal Python error: init_fs_encoding: failed to get the Python codec of the filesystem encoding
Python runtime state: core initialized
Traceback (most recent call last):
  File "<frozen importlib._bootstrap>", line 1007, in _find_and_load
  File "<frozen importlib._bootstrap>", line 161, in __exit__
  File "<frozen importlib._bootstrap>", line 116, in release
RuntimeError: cannot release un-acquired lock
generate-posix-vars failed
make: *** [Makefile:611: pybuilddir.txt] Error 1
```

We have two strange errors here:

1.  `Fatal Python error: init_fs_encoding: failed to get the Python
    codec of the filesystem encoding`
2.  `RuntimeError: cannot release un-acquired lock`

Both errors seem unrelated. They signal internal inconsistency in the
interpreter.

## Debugging

Is it a `gcc` bug? I usually assume it's not and try to find
inconsistency in the program. If it's luck I'll find the bug in the
program. Otherwise I'll get a small reproducer to fix the compiler.

First, I double-checked it's a `gcc-10 -O2`:

``` 
# on hppa host
# CC=gcc-10.1.0 CFLAGS=-O2 emerge -v1 dev-lang/python:3.9
<fails>
# CC=gcc-10.1.0 CFLAGS=-O1 emerge -v1 dev-lang/python:3.9
<works>
# CC=gcc-9.3.0 CFLAGS=-O2 emerge -v1 dev-lang/python:3.9
<works>
```

The bug is reproducible. Assuming it's an obscure python bug I tried
`AddressSanitizer` and `valigrind` on `amd64`. Neither reported
any relevant errors. `valgrind` found a seemingly legitimate
`use-after-free`: <https://bugs.gentoo.org/729570#c8>.
`--with-valgrind` makes that `valgrind` report go away. It also
makes original bug away. Which is not very useful. I expected some
diagnostic that would report heap corruption.
Falling back to original setup. We need to find a function where input
is the same but the output is different.
After much `printf` poking I found simple way to observe the problem:

``` 
$ git init .; git add .; git commit -m "initial state"
$ ./configure ... --host=hppa2.0-unknown-linux-gnu ...
$ make regen-importlib
$ git diff
--- a/Python/importlib_external.h
+++ b/Python/importlib_external.h
@@ -172,7 +172,7 @@ const unsigned char _Py_M__importlib_bootstrap_external[] = {
     83,0,113,44,100,3,124,0,102,2,83,0,41,4,122,32,
     82,101,112,108,97,99,101,109,101,110,116,32,102,111,114,32,
     111,115,46,112,97,116,104,46,115,112,108,105,116,40,41,46,
-    233,1,0,0,0,41,1,90,8,109,97,120,115,112,108,105,
+    114,15,0,0,0,41,1,90,8,109,97,120,115,112,108,105,
     116,218,0,41,6,114,23,0,0,0,114,31,0,0,0,218,
     10,114,112,97,114,116,105,116,105,111,110,114,35,0,0,0,
     218,8,114,101,118,101,114,115,101,100,218,6,114,115,112,108,
...
```

This output means that freshly built interpreter generates byte code
slightly different from already pre-generated. The diff is not expected
as building python with `-O1` produces no diff on the same test.
This is a simple enough test to move debugging from debugging on
`hppa` host to `amd64` host. With `binfmt_misc` wrappers I was
able to use exactly the same build commands on `amd64` to get the same
byte code diff.

## Shrinking the delta down

To shrink the trigger I wanted to find smallest piece of code to
build with `-O1` to see the bug disappear (while rest of code is built
with `-O2`).
Python build system outputs exact commands used to generate everything.
I dumped all commands and tweaked `-O2` to `-O1` in a binary search
fashion:

``` 
$ make clean
$ make | tee build.sh
hppa2.0-unknown-linux-gnu-gcc -c -Wno-unused-result -Wsign-compare -DNDEBUG  -O2 -fdelayed-branch -frecord-gcc-switches -fwrapv   -std=c99 -Wextra -Wno-unused-result -Wno-unused-parameter -Wno-missing-field-initializers -Werror=implicit-function-declaration -fvisibility=hidden  -I./Include/internal  -I. -I./Include -I/usr/include/ncursesw  -fPIC -DPy_BUILD_CORE -o Programs/python.o ./Programs/python.c
hppa2.0-unknown-linux-gnu-gcc -c -Wno-unused-result -Wsign-compare -DNDEBUG  -O2 -fdelayed-branch -frecord-gcc-switches -fwrapv   -std=c99 -Wextra -Wno-unused-result -Wno-unused-parameter -Wno-missing-field-initializers -Werror=implicit-function-declaration -fvisibility=hidden  -I./Include/internal  -I. -I./Include -I/usr/include/ncursesw  -fPIC -DPy_BUILD_CORE -o Parser/acceler.o Parser/acceler.c
...
./Programs/_freeze_importlib zipimport \
./Lib/zipimport.py \
./Python/importlib_zipimport.h.new
python3.9 ./Tools/scripts/update_file.py ./Python/importlib_zipimport.h ./Python/importlib_zipimport.h.new
```

Now I can just edit `build.sh` slightly and rerun it. To avoid
recompilation impact I used `ccache` shadows for
`hppa2.0-unknown-linux-gnu-gcc`:

``` 
$ which hppa2.0-unknown-linux-gnu-gcc
/usr/lib/ccache/bin/hppa2.0-unknown-linux-gnu-gcc
```

That way our "full rebuild" is as cheap as incremental rebuild:

``` 
$ time bash build.sh
real 0m2,258s
user 0m1,673s
sys  0m0,595s
```

2.5 seconds on my 10 years old machine. That gives us very responsive
debugging environment.
After a bit of poking I found that rebuilding `Objects/longobject.c`
with `-O1` is enough to make bug disappear:

``` diff
--- a/build.sh
+++ b/build.sh
@@ -39 +39 @@ hppa2.0-unknown-linux-gnu-gcc -c -Wno-unused-result -Wsign-compare -DNDEBUG  -O2
-hppa2.0-unknown-linux-gnu-gcc ... -O2 ... Objects/longobject.c
+hppa2.0-unknown-linux-gnu-gcc ... -O1 ... Objects/longobject.c
```

Now we can use advanced pragmas and attributes to re-enable `-O2` only
for subset of `Objects/longobject.c`. The tools are:

1.  `#pragma GCC push_options` / `#pragma GCC optimize(2)` /
    `#pragma GCC pop_options`: change optimization level only for a
    subset of functions in a file to narrow down the code which triggers
    problematic behavior.
2.  `__attribute__((noipa))`: make a function opaque to inliner as
    if it was in a separate compilation unit. Useful when shrinking test
    example down to a single file.

Usage example looks like that:

``` diff
--- a/Objects/longobject.c
+++ b/Objects/longobject.c
@@ -1,3 +1,5 @@
+#pragma GCC push_options
+#pragma GCC optimize(2)
 /* Long (arbitrary precision) integer object implementation */

 /* XXX The functional organization of this file is terrible */
@@ -3009,6 +3011,7 @@ PyLong_AsDouble(PyObject *v)
    if a == b, return 0
    if a > b, return a positive number */

+
 static Py_ssize_t
 long_compare(PyLongObject *a, PyLongObject *b)
 {
@@ -3027,6 +3030,8 @@ long_compare(PyLongObject *a, PyLongObject *b)
     return sign;
 }

+static PyObject *
+long_richcompare(PyObject *self, PyObject *other, int op) __attribute__((noipa));
 static PyObject *
 long_richcompare(PyObject *self, PyObject *other, int op)
 {
@@ -5807,3 +5812,4 @@ _PyLong_Fini(PyThreadState *tstate)
     }
 #endif
 }
+#pragma GCC pop_options
```

With the above tricks I arrived at `long_richcompare()`. I could not
see the immediate bug in the code. It looks very clean and simple.

## Shrinking down test example

`long_richcompare()` implements a comparison operator for `int`
class (arbitrary precision integer) in python. It's a simple
mathematical operation.
I added a few `printf()` statements to extract exact exact
inputs/outputs where `long_richcompare()` changes it's behaviour. It
was `long_richcompare(0xFFFFffff, 1, EQ)`.
The full extracted example was:

``` c
/*
   The test is extracted from Python-3.9.0 miscompilation
   on hppa2.0: https://bugs.gentoo.org/729570

   Original bug happens as an invalid bytecode generation
   due to bad results from 'long_richcompare(0xFFFFffff, 1, EQ)' calls.

Failure example:
  $ hppa2.0-unknown-linux-gnu-gcc -lm -Wsign-compare -Wall -O1 bug_test.c -o good-bug
  $ hppa2.0-unknown-linux-gnu-gcc -lm -Wsign-compare -Wall -O2 bug_test.c -o bad-bug
  $ ./good-bug
  long_richcompare(2, 1, EQ) = FALSE (expect FALSE)
  $ ./bad-bug
  long_richcompare(2, 1, EQ) = TRUE (expect FALSE)

*/

// We use '__attribute__((noipa));' aggressively to simulate
// unavailable function definitions from outside translation units.

static int cmp(int *lhs, int *rhs)
{
    int sign = *lhs - *rhs;

    // semantically this should be 'return 0;' but this condition is not
    // supposed to trigger on our input data.
    if (sign == 0) return 1;

    return sign;
}

static int yes(void) __attribute__((noipa));
static int yes(void) { return 1; }

static int long_richcompare(int *self, int *other, int op) __attribute__((noipa));
static int long_richcompare(int *self, int *other, int op)
{
    int result;

    if (!yes() || !yes())
        return 0;

    if (self == other)
        result = 0;
    else
        result = cmp(self, other);

    // has to force jump table
    switch (op) {
        // only 0 case is used on actual data
        case 0: return (result == 0);

        case 1: return 0;
        case 3: return 0;
        case 5: if (result == 0) return 1; else return 0;
        default:
            __builtin_unreachable();
    }
}

#include <stdio.h>

int main() {
    int l = 2;
    int r = 1;

    int res = long_richcompare(&l, &r, 0);
    printf("long_richcompare(2, 1, EQ) = %s (expect FALSE)\n", res ? "TRUE" : "FALSE");
}
```

Note: it's not a real comparison anymore. `cmp()` is an elaborate
no-op. This file is very easy to trace through and verify that it has no
problems related to the undefined behavior.
I asked `gcc` developers for help in <https://gcc.gnu.org/PR96015> and
Eric immediately suggested trying `-fno-delayed-branch` to see if it
makes a difference. It did!
Using `-fno-delayed-branch` allowed me to simplify the example even
further with `cvise`. It produced the following example:

``` c
int b, c;
int a() __attribute__((noipa));
int a(int *d, int *f, int g) {
  int e;
  if (d == f)
    e = 0; // never gets here on our input data
  else
    e = 1; // always gets here on our input data
  switch (g) {
  case 0:
    return e; // 'return 1'; always gets here on our input data
  case 1:
  case 3:
  case 5:
    if (e)
      return 10;
  default:
    __builtin_unreachable();
  }
}
int main() { return a(&b, &c, 0); }
```

``` 
$ hppa2.0-unknown-linux-gnu-gcc -O2 bug_test.c -o bad; ./bad; echo $?
0
$ hppa2.0-unknown-linux-gnu-gcc -O2 bug_test.c -o good -fno-delayed-branch; ./good; echo $?
1
```

## Generated code

Let's peek at generated code in this example:

``` asm
;; hppa2.0-unknown-linux-gnu-gcc -O2 -S ../bug_test.c -o bug.S
a:
        bv %r0(%r2) ; ret = 0; return ret;
         ldi 0,%r28 ; delayed slot for 'ret = 0'
```

This is very short and wrong `return 0` code.

``` asm
;; hppa2.0-unknown-linux-gnu-gcc -O2 -S ../bug_test.c -o bug.S -fno-delayed-branch
a:
        comclr,<> %r26,%r25,%r0 ; compare d == f
        b,n .L11                ; if (d == f) goto .L11 else no-op;
        nop
.L4:
.L12:
        ldil L'.L6,%r28
        ldo R'.L6(%r28),%r28    ; load address of jump table at .L6
        ldwx,s %r24(%r28),%r28  ; fetch .L6[g(arg2)] target address
        bv,n %r0(%r28)          ; goto at .L6[g(arg2)]
.L6:
        .begin_brtab
        .word .L8 ; 'case 0:' code (our case)
        .word .L5 ; 'case 1:' code
        .word .L4 ; 'case 2:' code
        .word .L5 ; 'case 3:' code
        .word .L4 ; 'case 4:' code
        .word .L5 ; 'case 5:' code
        .end_brtab
.L5:
        ldi 10,%r28   ; ret = 10
        bv,n %r0(%r2) ; return ret;
.L11:
        ldi 0,%r28    ; ret = 0
        bv,n %r0(%r2) ; return ret;
.L8:
        ldi 1,%r28    ; ret = 1
        bv,n %r0(%r2) ; return ret;
```

This is somewhat long and correct code.
So what is so special about `-fno-delayed-branch`? Why does it turn
things upside down?

## Delay slots

Delay slot (<https://en.wikipedia.org/wiki/Delay_slot>) is a simple
concept: on simple architectures some instructions take not usual one
clock but two clock cycles. Instead of stalling the CPU pipeline for
extra cycle CPU just executes next instruction (or a few of those)
following such heavyweight instruction (wat?). Such place in code is
called a "delay slot". Simple example on `hppa`:

``` 
a:
    bv %r0(%r2) ; 'return ret' ; takes 2 cycles
     ldi 0,%r28 ; 'ret = 0'    ; takes 1 cycle, executes in parallel to 'bv'
; total execution time: 2 cycles
```

Here `bv` (branch vectored) takes clock 2 cycles and CPU always
executes one instruction after it. The actual execution sequence is
equivalent to:

``` asm
a:
    ldi 0,%r28  ; takes 1 cycle
    bv %r0(%r2) ; takes 2 cycles
     nop        ; takes 1 cycle; executes in parallel to 'bv'
; total execution time: 3 cycles
```

It sounds like a simple transformation. But it's full of fancy corner
cases:

``` asm
a:
    ; invalid
    bv %r0(%r2)  ; 'return ret' ; takes 2 cycles
     bv %r0(%r2) ; 'return ret' ; takes 2 cycles, overlaps with previous
      nop        ; does it get executed?
```

Thankfully `hppa` forbids putting branch instructions themselves into
delay slot.
A few other architectures that use delay slot are: `mips`, `superh`,
`sparc`. But not `arm`, not `powerpc` and not `riscv`.
Having slightly tweaked my example I managed to reproduce the same bug
on `sh4`(`superh`): <https://gcc.gnu.org/PR96015#c27>.
The bug ended up being in `gcc` `reporg` pass: a late pass that
handles instruction reordering to pick the better sequence. It made
incorrect assumptions about instructions with delay slots.

Have fun!
