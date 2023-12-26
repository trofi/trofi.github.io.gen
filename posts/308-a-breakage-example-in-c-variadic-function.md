---
title: "a breakage example in C variadic function"
date: December 20, 2023
---

## Intro

This post is about `C` functions with ellipsis (`...`) in their
signatures. The most famous example of such is probably `printf()`:

```c
int printf(const char *format, ...);
```

If you ever tried to use it you probably know that using wrong types
that don't match format arguments might crash your program. A simple
faulty example could be:

```c
printf("%s", 42); // this will crash
```

Luckily `gcc` and `clang` do have `-Wformat` warning that complains
about the mismatch between expected types by a format string and
actually passed types:

```c
// $ cat simple.c
#include <stdio.h>

int main(void) {
    printf("%s", 42);
}
```

```
$ gcc -Wformat -c simple.c

simple.c: In function 'main':
simple.c:5:14: warning: format '%s' expects argument of type 'char *', but argument 2 has type 'int' [-Wformat=]
    5 |     printf("%s", 42);
      |             ~^   ~~
      |              |   |
      |              |   int
      |              char *
      |             %d
```

Many distributions turn these warnings into errors by default.

## Argument count

An ellipsis (`...`) means that the function accepts unknown count and
unknown type of parameters. There has to be a way to somehow signal
actual argument list in ellipsis. As `<stdarg.h>` does not provide a
standard way to do it code author has to come up with their own scheme
to solve the problem.

A few examples come to mind:

- `int printf(const char *format, ...)` uses `format` parameter to pass
  the argument count by interpreting the format string.
- `int open(const char *pathname, int flags, mode_t mode)` uses `flags`
  to distinguish 3-argument form from 2-argument
  `int open(const char *pathname, int flags)`.
- `glib`'s `gchar* g_strconcat (const gchar* string1, ...)` consumes
  parameters until `NULL` parameter is encountered.

Each of the examples above implements a different scheme.

`g_strconcat` is especially scary as you might pass non-strings there
and get no warning from the compiler. Or forget to pass a `NULL` value
([`proftpd` example](https://github.com/proftpd/proftpd/pull/1028/files)
comes to mind). But at least all the arguments are expected to have the
same `const gchar*` type.

But the above is not an exhaustive list. You can bake any assumption
into ellipsis meaning.

## An example of variadic C function

I'll use yet another (arguably the simplest) form to pass argument count
to a variadic function: I'll pass the number explicitly. Let's explore
the following example:

```c
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void foo(int n, ...) {
    va_list va;

    va_start(va, n);
    for (int i = 0; i < n; i++) {
        size_t l = va_arg(va, size_t);
        printf("[%u]: %zu\n", i, l);
    }
    va_end(va);
}

int main(void) {
    foo(3, strlen("foo"), strlen("barr"), strlen("bazzz"));
}
```

Here we explicitly pass count of variadic arguments as the first `n`
parameter of the `foo()` function. The program should be correct.

Running it:

```
$ gcc a.c -o a &&./a
[0]: 3
[1]: 4
[2]: 5
```

All good.

Now I'll change the above program slightly:

```c
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void foo(int n, ...) {
    va_list va;

    va_start(va, n);
    for (int i = 0; i < n; i++) {
        size_t l = va_arg(va, size_t);
        printf("[%u]: %zu\n", i, l);
    }
    va_end(va);
}

int main(void) {
    foo(3, 3, 4, 5);
}
```

I inlined the results of `strlen()` calls to their literal values. Is it
still a correct program? Is it always expected to print `3 4 5`?

Let's run it:

```
$ gcc a.c -o a &&./a
[0]: 3
[1]: 4
[2]: 5
```

Seems to work. Let's throw more arguments just for fun:

```c
// $ cat a.c
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void foo(int n, ...) {
    va_list va;

    va_start(va, n);
    for (int i = 0; i < n; i++) {
        size_t l = va_arg(va, size_t);
        printf("[%u]: %zu\n", i, l);
    }
    va_end(va);
}

int main() {
    foo(16, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
}
```

I increased argument count to `16`. Running it on `x86_64`:

```
$ gcc a.c -o a &&./a
[0]: 1
[1]: 2
[2]: 3
[3]: 4
[4]: 5
[5]: 6
[6]: 7
[7]: 8
[8]: 9
[9]: 10
[10]: 11
[11]: 12
[12]: 13
[13]: 14
[14]: 15
[15]: 16
```

Still all good!

Running on `aarch64-linux` just in case:

```
$ aarch64-unknown-linux-gnu-gcc a.c -o a &&./a
[0]: 1
[1]: 2
[2]: 3
[3]: 4
[4]: 5
[5]: 6
[6]: 7
[7]: 70368744177672
[8]: 70368744177673
[9]: 70368744177674
[10]: 70368744177675
[11]: 70368744177676
[12]: 13
[13]: 70368744177678
[14]: 15
[15]: 70368744177680
```

Uh-oh. It's broken!

What is worse: first seven parameters look totally fine and degradation
start only `8th` one. Is it a coincidence? Some architecture-specific
property? Or maybe a compiler bug?

Or maybe you noticed a bug in the original program? How would you fix it
or work it around?

## Argument passing mechanics

Let's have a look at the generated code and check how parameters are
passed across the call boundary. I'll use the same `17`-argument example
above:

```c
#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void foo(int n, ...) {
    va_list va;

    va_start(va, n);
    for (int i = 0; i < n; i++) {
        size_t l = va_arg(va, size_t);
        printf("[%u]: %zu\n", i, l);
    }
    va_end(va);
}

int main(void) {
    foo(16, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
}
```

## `x86_64`

Full `x86_64` code on `gcc` looks this way:

```asm
; gcc -O1 -S a.c -o -

main:
        subq    $16, %rsp
        ; push some of the arguments on stack:
        pushq   $16 ; va[15] = 16
        pushq   $15 ; va[14] = 15
        pushq   $14 ; va[13] = 14
        pushq   $13 ; va[12] = 13
        pushq   $12 ; va[11] = 12
        pushq   $11 ; va[10] = 11
        pushq   $10 ; va[9] = 10
        pushq   $9  ; va[8] = 9
        pushq   $8  ; va[7] = 8
        pushq   $7  ; va[6] = 7
        pushq   $6  ; va[5] = 6

        movl    $5, %r9d  ; va[4] = 5
        movl    $4, %r8d  ; va[3] = 4
        movl    $3, %ecx  ; va[2] = 3
        movl    $2, %edx  ; va[1] = 2
        movl    $1, %esi  ; va[0] = 1
        movl    $16, %edi ; n = 16

        movl    $0, %eax
        call    foo
        movl    $0, %eax
        addq    $104, %rsp
        ret

.LC0:
        .string "[%u]: %zu\n"

foo:
        pushq   %rbp
        pushq   %rbx
        subq    $88, %rsp
        movq    %rsi, 40(%rsp)
        movq    %rdx, 48(%rsp)
        movq    %rcx, 56(%rsp)
        movq    %r8, 64(%rsp)
        movq    %r9, 72(%rsp)
        movl    $8, 8(%rsp)
        leaq    112(%rsp), %rax
        movq    %rax, 16(%rsp)
        leaq    32(%rsp), %rax
        movq    %rax, 24(%rsp)
        testl   %edi, %edi
        jle     .L1
        movl    %edi, %ebp
        movl    $0, %ebx
        jmp     .L5
.L3:
        movq    16(%rsp), %rdx
        leaq    8(%rdx), %rax
        movq    %rax, 16(%rsp)
.L4:
        movq    (%rdx), %rdx  ; size_t l = va_arg(va, size_t);
        movl    %ebx, %esi    ; int i (loop variable)
        movl    $.LC0, %edi   ; format = "%[u]: %zu\n"
        movl    $0, %eax
        call    printf
        addl    $1, %ebx
        cmpl    %ebx, %ebp
        je      .L1
.L5:
        movl    8(%rsp), %eax
        cmpl    $47, %eax
        ja      .L3
        movl    %eax, %edx
        addq    24(%rsp), %rdx
        addl    $8, %eax
        movl    %eax, 8(%rsp)
        jmp     .L4
.L1:
        addq    $88, %rsp
        popq    %rbx
        popq    %rbp
        ret
```

It's a lot of text! We can ignore most of it and focus on the following
few lines to get to the argument passing mechanics:

```asm
main:
        ; ...
        pushq   $6  ; va[5] = 6
        movl    $5, %r9d  ; va[4] = 5
        ; ...

foo:
        ; ...
        movq    (%rdx), %rdx  ; size_t l = va_arg(va, size_t);
        movl    %ebx, %esi    ; int i (loop variable)
        movl    $.LC0, %edi   ; format = "%[u]: %zu\n"
        movl    $0, %eax
        call    printf
        ; ...
```

The `main` function is trivial: it shows us that first 6 arguments are
passed in registers alone (`%edi = 16`, `%esi = 1`, `%edx = 2`,
`%ecx = 3`, `%r8d = 4` `%r9d = 5`). And starting from `7`th argument
they are passed via stack (`pushq $6`). This is a standard
`x86_64-linux` calling convention.

The `foo` is more complicated. The gist of it is that our `va_arg`
always gets fetched from stack as a 64-bit value via `movq (%rdx), %rdx`
instruction. To make it work `foo` stores all register-passed arguments
on stack. The fetch result gets passed later as a third argument to
`printf("%[u]: %zu\n", i, l)` call in `%rdx` register.

A few notes before we continue:

Instructions like `movl $1, %esi` tell the CPU to store `$1` to
32-bit `esi` register (lower half of `rsi` register). `movl` (or any
other write instruction that works on 32-bit registers) also zeroes out
upper 64-bits of `rsi` register. Thus it's a functional equivalent of
`movq $1, %rsi`. But the encoding might be more efficient as it does not
need a `REX` prefix.

Instructions like `pushq $6` write full 64-bit constant on stack as if
we pushed full `size_t` value instead of `int`.

In both register store and memory store cases `int` literals are stored
as 64-bit values. This means that on `x86_64` it's not too bad to mix
these two types as the example does.

## `aarch64`

Now let's do the same exercise for `aarch64` target:

```asm
; aarch64-unknown-linux-gnu-gcc -O1 -S a.c -o -
main:
        sub     sp, sp, #96
        stp     x29, x30, [sp, 80]
        add     x29, sp, 80
        mov     w0, 16 ; n = 16
        str     w0, [sp, 64] ; va[15] = 16
        mov     w1, 15
        str     w1, [sp, 56] ; va[14] = 15
        mov     w1, 14
        str     w1, [sp, 48] ; va[13] = 14
        mov     w1, 13
        str     w1, [sp, 40] ; va[12] = 13
        mov     w1, 12
        str     w1, [sp, 32] ; va[11] = 12
        mov     w1, 11
        str     w1, [sp, 24] ; va[10] = 11
        mov     w1, 10
        str     w1, [sp, 16] ; va[9]  = 10
        mov     w1, 9
        str     w1, [sp, 8]  ; va[8]  = 9
        mov     w1, 8
        str     w1, [sp]     ; va[7]  = 8
        mov     w7, 7        ; va[6]  = 7
        mov     w6, 6        ; va[5]  = 6
        mov     w5, 5        ; va[4]  = 5
        mov     w4, 4        ; va[3]  = 4
        mov     w3, 3        ; va[2]  = 3
        mov     w2, 2        ; va[1]  = 2
        mov     w1, 1        ; va[0]  = 1
        bl      foo
        mov     w0, 0
        ldp     x29, x30, [sp, 80]
        add     sp, sp, 96
        ret

.LC0:
        .string "[%u]: %zu\n"

foo:
        stp     x29, x30, [sp, -144]!
        mov     x29, sp
        stp     x19, x20, [sp, 16]
        mov     w20, w0
        str     x1, [sp, 88]
        str     x2, [sp, 96]
        str     x3, [sp, 104]
        str     x4, [sp, 112]
        str     x5, [sp, 120]
        str     x6, [sp, 128]
        str     x7, [sp, 136]
        add     x0, sp, 144
        str     x0, [sp, 48]
        str     x0, [sp, 56]
        add     x0, sp, 80
        str     x0, [sp, 64]
        mov     w0, -56
        str     w0, [sp, 72]
        str     wzr, [sp, 76]
        cmp     w20, 0
        ble     .L1
        str     x21, [sp, 32]
        mov     w19, 0
        adrp    x21, .LC0
        add     x21, x21, :lo12:.LC0
        b       .L6
.L3:
        add     w0, w2, 8
        str     w0, [sp, 72]
        cmp     w0, 0
        ble     .L5
        add     x0, x1, 15
        and     x0, x0, -8
        str     x0, [sp, 48]
.L4:
        ldr     x2, [x1] ; size_t l = va_arg(va, size_t);
        mov     w1, w19  ; int i (loop variable)
        mov     x0, x21  ; format = "%[u]: %zu\n"
        bl      printf
        add     w19, w19, 1
        cmp     w20, w19
        beq     .L9
.L6:
        ldr     w2, [sp, 72]
        ldr     x1, [sp, 48]
        tbnz    w2, #31, .L3
        add     x2, x1, 15
        and     x2, x2, -8
        str     x2, [sp, 48]
        b       .L4
.L5:
        ldr     x1, [sp, 56]
        add     x1, x1, w2, sxtw
        b       .L4
.L9:
        ldr     x21, [sp, 32]
.L1:
        ldp     x19, x20, [sp, 16]
        ldp     x29, x30, [sp], 144
        ret
```

Again, it's a lot of repetitive text. We can ignore most of it and focus
on arguments passed over call boundary:

```asm
main:
        ; ...
        mov     w1, 8
        str     w1, [sp]     ; va[7]  = 8
        mov     w7, 7        ; va[6]  = 7
        mov     w6, 6        ; va[5]  = 6

foo:
        ; ...
        ldr     x2, [x1] ; size_t l = va_arg(va, size_t);
        mov     w1, w19  ; int i (loop variable)
        mov     x0, x21  ; format = "%[u]: %zu\n"
        bl      printf
```

The `main` structure is very similar to `x86_64`: first few parameters
(`8` this time) get passed via registers: `w0 = 16`, `w1 = 1`, `w2 = 2`,
.... `w7 = 7`. The rest goes to stack: `mov w1, 8` / `str w1, [sp]`,
`mov w1, 9` / `str w1, [sp, 8]`, and so on.

Similarly to `x86_64` the instruction `mov w1, 1` sets lower 32-bit part
of 64-bit `x1` register to an immediate value. Higher 32-bit part is
zeroed out. This makes it equivalent to `mov x1, 1` instruction.

The difference starts in the way stack variables are stored: while
`mov w1, 8` initializes both `w1` and `x1` to value `8` the
`str w1, [sp]` instruction writes only 32 bits of value on stack. Upper
32 bits of stack value contain existing value (some garbage). If we
wanted to fix it then `str x1, [sp]` would place all `64` bits as. In
theory `gcc` could have used that instruction even for your unmodified
case. But it does not have to.

This is our corruption mechanic: we store 32 bits on stack for
parammeters `8` and above and then read 64-bit values from store
locations in `C` pseudo-code:

```c
// main():
    // ...
    int val;
    size_t location = uninitialized();
    // ...
    *(int*)(&location) = val; // store 32 initialized bits
// foo():
    // ...
    size_t result = *(size_t*)(&location); // load 64 bits
```

## Possible fix

Once the breakage is clear the fix is simple: use exact expected type
at call site. In this case `size_t` instead of `int`:

```diff
--- a.c
+++ a.fixed.c
@@ -16,3 +16,3 @@
 int main() {
-    foo(16, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
+    foo(16, (size_t)1,(size_t)2,(size_t)3,(size_t)4,(size_t)5,(size_t)6,(size_t)7,(size_t)8,(size_t)9,(size_t)10,(size_t)11,(size_t)12,(size_t)13,(size_t)14,(size_t)15,(size_t)16);
 }
```

Not the prettiest change, but it gets the job done:

```
$ aarch64-unknown-linux-gnu-gcc -O1 a.fixed.c -o a && ./a
[0]: 1
[1]: 2
[2]: 3
[3]: 4
[4]: 5
[5]: 6
[6]: 7
[7]: 8
[8]: 9
[9]: 10
[10]: 11
[11]: 12
[12]: 13
[13]: 14
[14]: 15
[15]: 16
```

The code change looks as expected:

```diff
diff -U0 a.S a.fixed.S
--- a.S 2023-12-17 23:51:24.749893552 +0000
+++ a.fixed.S   2023-12-17 23:51:29.687979158 +0000
@@ -2 +2 @@
-       .file   "a.c"
+       .file   "a.fixed.c"
@@ -102,13 +102,13 @@
-       mov     w0, 16
-       str     w0, [sp, 64]
-       mov     w1, 15
-       str     w1, [sp, 56]
-       mov     w1, 14
-       str     w1, [sp, 48]
-       mov     w1, 13
-       str     w1, [sp, 40]
-       mov     w1, 12
-       str     w1, [sp, 32]
-       mov     w1, 11
-       str     w1, [sp, 24]
-       mov     w1, 10
-       str     w1, [sp, 16]
-       mov     w1, 9
-       str     w1, [sp, 8]
-       mov     w1, 8
-       str     w1, [sp]
+       mov     x0, 16
+       str     x0, [sp, 64]
+       mov     x1, 15
+       str     x1, [sp, 56]
+       mov     x1, 14
+       str     x1, [sp, 48]
+       mov     x1, 13
+       str     x1, [sp, 40]
+       mov     x1, 12
+       str     x1, [sp, 32]
+       mov     x1, 11
+       str     x1, [sp, 24]
+       mov     x1, 10
+       str     x1, [sp, 16]
+       mov     x1, 9
+       str     x1, [sp, 8]
+       mov     x1, 8
+       str     x1, [sp]
@@ -119,7 +119,7 @@
-       mov     w7, 7
-       mov     w6, 6
-       mov     w5, 5
-       mov     w4, 4
-       mov     w3, 3
-       mov     w2, 2
-       mov     w1, 1
+       mov     x7, 7
+       mov     x6, 6
+       mov     x5, 5
+       mov     x4, 4
+       mov     x3, 3
+       mov     x2, 2
+       mov     x1, 1
```

Only `main` saw the change. The change is shift from 32-bit to 64-bit
registers in value assignments and value stores.

## Is this bug real?

The main takeaway from the above is that on `aarch64` arguments `9` and
above must not mix `int` / `size_t` and pass exact type if those
arguments are present in variadic template.

Could such bug happen on real code or 9 arguments are too much to be
seen in the wild? Guess how I found this obscurity!

Here is the [`iwd-0.11`](https://git.kernel.org/pub/scm/network/wireless/iwd.git/tree/src/dpp-util.c?h=2.11#n1379) code:

```c
bool prf_plus(enum l_checksum_type type, const void *key, size_t key_len,
              void *out, size_t out_len,
              size_t n_extra, ...)
{
    // ...
    struct iovec iov[n_extra + 2];
    va_list va;
    size_t i;

    va_start(va, n_extra);

    for (i = 0; i < n_extra; i++) {
        iov[i + 1].iov_base = va_arg(va, void *);
        iov[i + 1].iov_len  = va_arg(va, size_t);
    }
    // ...
}
// ...
bool dpp_derive_z(const uint8_t *mac_i, more params, void *z_out, size_t *z_len)
{
    // ...
    prf_plus(sha, prk, bytes, z_out, bytes,
             5,
             mac_i, 6,
             mac_r, 6,
             m_x, bytes,
             n_x, bytes,
             key, strlen(key));
```

Do you see where the thing breaks?

Here `prf_plus()` expects `n_extra` pairs of `void *` / `size_t` in the
variadic arguments. But `dpp_derive_z()` passes `void *` / `int` as
first two pairs.

I would never notice it if not for mysteriously failing `iwd` test on
`aarch64` platform:

```
    $ unit/test-dpp
    TEST: DPP test responder-only key derivation
    TEST: DPP test mutual key derivation
    TEST: DPP test PKEX key derivation
    test-dpp: unit/test-dpp.c:514: test_pkex_key_derivation: Assertion `!memcmp(tmp, __tmp, 32)' failed.
```

`strace` shown the smoking gun this way:

```
$ strace unit/test-dpp
...
sendmsg(4, {
    msg_name=NULL,
    msg_namelen=0,
    msg_iov=[
        {iov_base="", iov_len=0},
        {iov_base="\254d\221\364R\7", iov_len=6},
        {iov_base="n^\316n\363\335\0\0\0\0"..., iov_len=281470681743366},
        {iov_base="\274\312\216#\345\300P2"..., iov_len=32},
        {iov_base="\n\221\340r\210\t\273\2"..., iov_len=32},
        {iov_base="thisisreallysecret", iov_len=18},
        {iov_base="\1", iov_len=1}],
    msg_iovlen=7, msg_controllen=0, msg_flags=0}, MSG_MORE) = 3136
```

See anything suspicious?

Length of the third element of `msg_iov` array is `281470681743366` (or
`0xffff00000006` in hex). It should have been `6` if not for higher
`0xffff` garbage bits.

While we are at it: `sendmsg()` did not fail with an `-EINVAL` error and
consumed `3K` of data. At best it will fail at the key derivation. At
worst it might send your unrelated process memory over the network.

A nasty kind of bug.

## Bonus section

If we go back to our original broken example are there any other 64-bit
architectures where `size_t` / `int` mismatch is as problematic as on
`aarch64`?

I'll show the result for the following list of `8` `64`-bit
architectures I could remember:

- `alpha`
- `mips64 -mabi=64`
- `loongarch64`
- `s390x`
- `powerpc64`
- `sparc64`
- `riscv64`
- `ia64`

What is your guess? Is `aarch64` the unique one being broken here, or
maybe `x86_64` is unique in that it happens to work anyway? Will it be
endianness-specific? Or it's closer to `50/50`?

Let's see:

- `alpha`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      lda t0,6
      stq t0,0(sp)
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ldq a3,0(t0)
  ```

  The target passes first `6` arguments in registers.

- `mips64 -mabi=64`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      li v0,8
      sd v0,0(sp)
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ld a3,-8(s1)
  ```

  The target passes first `8` arguments in registers.

- `loongarch64`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      addi.w $t0, $zero, 9(0x9)
      st.d $t0, $sp, 8(0x8)
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ld.d $a3, $s1, -8(0xff8)
  ```

  The target passes first `8` arguments in registers.

- `s390x`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      lghi %r1,5
      stg %r1,160(%r15)
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      lg %r5,0(%r1)
  ```

  The target passes first `5` arguments in registers.

- `powerpc64`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      li r7,8
      std r7,112(r1)
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ldu r6,8(r29)
  ```

  The target passes first `8` arguments in registers.

- `sparc64`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      mov  6, %g1
      stx  %g1, [ %sp + 0x8af ]
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ldx  [ %i5 ], %o3
  ```

  The target passes first `6` arguments in registers.

- `riscv64`: no corruption. Stores 64-bit values on stack:

  ```asm
  main:
      ; ...
      li a5
      sd a5,0(sp)
  ```

  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ld a3,-8(s1)
  ```

  The target passes first `8` arguments in registers.

- `ia64`: has corruption:

  ```
  $ ./a
  [0]: 1
  [1]: 2
  [2]: 3
  [3]: 4
  [4]: 5
  [5]: 6
  [6]: 7
  [7]: 8
  [8]: 2305843009213693961
  [9]: 2305843009213693962
  [10]: 11
  [11]: 12
  [12]: 13
  [13]: 14
  [14]: 15
  [15]: 16
  ```

  `ia64` stores only 32-bit value on stack (just like `aarch64`):

  ```asm
  main:
      ; ...
      mov r15=8
      ;;
      st4 [r14]=r15
  ```
  and loads it as 64-bit value:

  ```asm
  foo:
      ; ...
      ld8 r47=[r14]
  ```

  The target passes first `8` arguments in registers.

Final table:

| target | Is affected | In registers | First on stack |
| :----: | :---------: | :----------: | :------------: |
| `alpha` | no | 6 | 7 |
| `mips64` | no | 8 | 9 |
| `loongarch64` | no | 8 | 9 |
| `s390x` | no | 5 | 6 |
| `powerpc64` | no | 8 | 9 |
| `sparc64` | no | 6 | 7 |
| `riscv64` | no | 8 | 9 |
| **`ia64`** | **yes** | 8 | 9 |
| `x86_64` | no | 6 | 7 |
| **`aarch64`** | **yes** | 8 | 9 |

Thus `aarch64` is not unique, but very close to it :)

## Parting words

One has to be careful at specifying exact types expected by variadic
functions. Integral type conversion rules do not apply the same way you
would expect for a non-variadic function call.

If you are writing your function with variadic parameters and it's not
a `printf()`-style function then compiler will not be able to help you
with warnings. Make sure you have a way to validate passed types via
other means.

Sometimes breakages are very subtle: first `8` parameters would work
just fine and `9`-th one will eat all your data. And it will happen only
on small set of architectures: `aarch64` and `ia64` :)

`iwd` fix went upstream as
[this patch](https://git.kernel.org/pub/scm/network/wireless/iwd.git/commit/?id=688d27700833258a139a6fbd5661334bd2c9fa98).

Have fun!

## Is AMD64 actually immune to this?

[No](https://en.wikipedia.org/wiki/Betteridge%27s_law_of_headlines).
GCC's use of `push` instructions is controlled by two options,
`-mpush-args` and `-mno-accumulate-outgoing-args`:

```c
static bool
ix86_push_argument (unsigned int npush)
{
  /* If SSE2 is available, use vector move to put large argument onto
     stack.  NB:  In 32-bit mode, use 8-byte vector move.  */
  return ((!TARGET_SSE2 || npush < (TARGET_64BIT ? 16 : 8))
          && TARGET_PUSH_ARGS
          && !ACCUMULATE_OUTGOING_ARGS);
}
```

The latter option is automatically adjusted depending on which CPU
family GCC should tune for:

```c
/* X86_TUNE_ACCUMULATE_OUTGOING_ARGS: Allocate stack space for outgoing
   arguments in prologue/epilogue instead of separately for each call
   by push/pop instructions.
   This increase code size by about 5% in 32bit mode, less so in 64bit mode
   because parameters are passed in registers.  It is considerable
   win for targets without stack engine that prevents multple push operations
   to happen in parallel.  */

DEF_TUNE (X86_TUNE_ACCUMULATE_OUTGOING_ARGS, "accumulate_outgoing_args",
          m_PPRO | m_P4_NOCONA | m_BONNELL | m_SILVERMONT | m_KNL | m_KNM | m_INTEL
          | m_GOLDMONT | m_GOLDMONT_PLUS | m_ATHLON_K8 | m_LUJIAZUI)
```

So the use of `push` vs. `mov` is a tuning choice, which makes this bug
even more subtle: it can surface depending on which CPU is specified via
the `-march=` or `-mtune` option.
