---
title: "C union initialization and gcc-15"
date: December 1, 2024
---

## a contrived example

Let's start from a quiz. What do you think will this program print:

```c
#include <stdio.h>

__attribute__((noipa)) static void use_stack(void) {
    volatile int foo[] = { 0x40, 0x41, 0x42, 0x43, };
}

__attribute__((noipa)) static int do_it(void) {
    // use 'volatile' to inhibit constant propagation
    volatile union {
        int dummy;
        struct { int fs[4]; } s;
    } v = { 0 };
    return v.s.fs[3];
}

int main(void) {
    use_stack();
    int r = do_it();
    printf("v.s:\n");
    printf("  .fs[3] = %#08x\n", r);
}
```

The program initializes `v` union with `{ 0 }`. Which should be an
equivalent of `v.dummy = 0;`. Then the program accesses `v.s.fs[3]`.
That element does not overlap in memory with `v.dummy`. What should it
do?

One of the possible answers is: `v.s.fs[3]` is a garbage value.

Let's try to run it on `gcc-14`:

```
$ gcc-14 a.c -o a -O2 && ./a
v.s:
  .fs[3] = 00000000
```

The value is all zeros. Is it a coincidence? `valgrind` does not
complain either. Let's have a peek at the disassembly dump:

```asm
; $ objdump --no-addresses --no-show-raw-insn -d a
<use_stack>:
        movdqa 0xea8(%rip),%xmm0 ; load the constant from memory
        movaps %xmm0,-0x18(%rsp) ; store the constant on stack
        ret
        xchg   %ax,%ax

<do_it>:
        pxor   %xmm0,%xmm0       ; zero-initialize 16 bytes
        movaps %xmm0,-0x18(%rsp) ; store all 16 bytes of zeros on stack
        mov    -0xc(%rsp),%eax   ; read 32-bits of zeros (part of 16-byte
                                 ; zeroing one line above)
        ret
```

`gcc-14` implements `v = { 0 };` as a 128-bit (16-byte)
zero initialization of `sizeof(v)` via
`pxor %xmm0,%xmm0; movaps %xmm0,-0x18(%rsp)`.

How about `gcc-15`?

```
$ gcc a.c -o a -O2 && ./a
v.s:
  .fs[3] = 0x000043
```

Whoops. That that is clearly uninitialized value left after `use_stack()`
execution. `valigrind` is also not happy about it:

```
$ valgrind --quiet --track-origins=yes ./a
v.s:
Use of uninitialised value of size 8
   at 0x48B954A: _itoa_word (in ...-glibc-2.40-36/lib/libc.so.6)
   by 0x48C43EB: __printf_buffer (in ...-glibc-2.40-36/lib/libc.so.6)
   by 0x48C6300: __vfprintf_internal (in ...-glibc-2.40-36/lib/libc.so.6)
   by 0x48BA71E: printf (in ...-glibc-2.40-36/lib/libc.so.6)
   by 0x401074: main (a.c:20)
 Uninitialised value was created by a stack allocation
   at 0x401190: do_it (a.c:12)
```


Disassembly:

```
; $ objdump --no-addresses --no-show-raw-insn -d a
<use_stack>:
        movabs $0x4100000040,%rax ; load 64-bit part 1
        movabs $0x4300000042,%rdx ; load 64-bit part 2
        mov    %rax,-0x18(%rsp)   ; store part 1 on stack
        mov    %rdx,-0x10(%rsp)   ; store part 2 on stack
        ret
        nop

<do_it>:
        movl   $0x0,-0x18(%rsp)   ; zero-initialize first 32 bits of a union
        mov    -0xc(%rsp),%eax    ; read uninitialized 32-bit value at 12-byte
                                  ; offset from a union start
        ret
```

`gcc-15` implements `v = { 0 }` as a single 32-bit store as if it was
`v.dummy = 0;` and leaves the rest of the union intact.

Is it a bug?

`gcc-15` intentionally changed the zeroing behaviour to do less in
[`PR116416`](https://gcc.gnu.org/PR116416) with
[this commit](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=0547dbb725b6d8e878a79e28a2e171eafcfbc1aa)
to generate more optimal code.

Fun fact: the patch also adds a `-fzero-init-padding-bits=unions`
option to enable the old behaviour.

## the real bug

The above example might sound theoretical, but I extracted it from an
`mbedtls` test suite failure. After a recent `gcc-15` update the tests
are now failing as:

```
The following tests FAILED:
        91 - psa_crypto-suite (Failed)
       113 - psa_crypto_storage_format.v0-suite (Failed)
```

I initially thought it's a compiler bug related to arithmetics. But
exploring the failing test I found the following pattern:

```c
// at tests/src/psa_exercise_key.c:
  psa_mac_operation_t operation = PSA_MAC_OPERATION_INIT;

// library/psa_crypto.c:

  if (operation.hash_ctx.id != 0) { return error; }
  //...

// include/psa/crypto_struct.h:
  #define PSA_MAC_OPERATION_INIT { 0, 0, 0, { 0 } }

// include/psa/crypto.h:
  typedef struct psa_mac_operation_s psa_mac_operation_t;

// include/psa/crypto_struct.h:
  struct psa_mac_operation_s {
    unsigned int id;
    uint8_t mac_size;
    unsigned int is_sign : 1;
    psa_driver_mac_context_t ctx;
  };

// include/psa/crypto_driver_contexts_composites.h:
  typedef union {
    unsigned dummy; /* Make sure this union is always non-empty */
    mbedtls_psa_mac_operation_t mbedtls_ctx;
  } psa_driver_mac_context_t;

// include/psa/crypto_builtin_composites.h:
  typedef struct {
    psa_algorithm_t alg;
    union {
        unsigned dummy; /* Make the union non-empty even with no supported algorithms. */
        mbedtls_psa_hmac_operation_t hmac;
        mbedtls_cipher_context_t cmac;
    } ctx;
  } mbedtls_psa_mac_operation_t;

// include/psa/crypto_builtin_composites.h
  typedef struct {
    /** The HMAC algorithm in use */
    psa_algorithm_t alg;
    /** The hash context. */
    struct psa_hash_operation_s hash_ctx;
    /** The HMAC part of the context. */
    uint8_t opad[PSA_HMAC_MAX_HASH_BLOCK_SIZE];
  } mbedtls_psa_hmac_operation_t;

// include/psa/crypto_types.h
  typedef uint32_t psa_algorithm_t;

// include/psa/crypto_struct.h
  struct psa_hash_operation_s {
    /** Unique ID indicating which driver got assigned to do the
     * operation. Since driver contexts are driver-specific, swapping
     * drivers halfway through the operation is not supported.
     * ID values are auto-generated in psa_driver_wrappers.h.
     * ID value zero means the context is not valid or not assigned to
     * any driver (i.e. the driver context is not active, in use). */
    unsigned int id;
    psa_driver_hash_context_t ctx;
  };
```

It's quite a bit of indirection, but if we compress it into a single
`struct` definition and remove irrelevant bits we will get something
like that:

```c
  struct {
    unsigned int id; // initialized below
    uint8_t mac_size; // initialized below
    unsigned int is_sign : 1; // initialized below
    union {
      unsigned dummy; // initialized below
      struct {
        uint32_t alg; // initialized below, alias of `dummy`

        // anything below is NOT initialized

        union {
          unsigned dummy;
          struct {
              uint32_t alg;
              struct {
                  unsigned int id; // <- we are about to use this field
                  psa_driver_hash_context_t ctx;
              } hash_ctx;
              uint8_t opad[PSA_HMAC_MAX_HASH_BLOCK_SIZE];
          } hmac;
          // ..
       } mbedtls_ctx;
    } ctx;
  } operation = {
    0, // id
    0, // mac_size
    0, // is_sign
    { 0 } // ctx.dummy
  };

  if (operation.hash_ctx.id != 0) { return error; }
```

`valgrind` complains about the use of an uninitialized value as:

```
$ valgrind --track-origins=yes --trace-children=yes --num-callers=50 --track-fds=yes --leak-check=full --show-reachable=yes --malloc-fill=0xE1 --free-fill=0xF1 tests/test_suite_psa_crypto
...
==2758824== Conditional jump or move depends on uninitialised value(s)
==2758824==    at 0x483C6B: psa_hash_setup (psa_crypto.c:2298)
==2758824==    by 0x490ADA: psa_hmac_setup_internal (psa_crypto_mac.c:90)
==2758824==    by 0x490ADA: psa_mac_setup (psa_crypto_mac.c:299)
==2758824==    by 0x48412C: psa_driver_wrapper_mac_sign_setup (psa_crypto_driver_wrappers.h:2297)
==2758824==    by 0x48412C: psa_mac_setup (psa_crypto.c:2619)
==2758824==    by 0x4083BC: test_mac_key_policy (test_suite_psa_crypto.function:2192)
==2758824==    by 0x408877: test_mac_key_policy_wrapper (test_suite_psa_crypto.function:2264)
==2758824==    by 0x429F4E: dispatch_test (main_test.function:170)
==2758824==    by 0x42A813: execute_tests (host_test.function:676)
==2758824==    by 0x40247A: main (main_test.function:263)
==2758824==  Uninitialised value was created by a stack allocation
==2758824==    at 0x40822C: test_mac_key_policy (test_suite_psa_crypto.function:2167)
```

Unfortunately I don't think there is a simple fix for that (apart from
enabling new `-fzero-init-padding-bits=unions` compiler flag if it's
supported.

I filed the issue upstream as
[Issue #9814](https://github.com/Mbed-TLS/mbedtls/issues/9814) hoping to
get some guidance.

## parting words

`gcc-15` will be more efficient at handling partial union initialization.

It will likely be at expense of exposing real code bugs like the
[`mbedtls`](https://github.com/Mbed-TLS/mbedtls/issues/9814) one to the
users. It's a bit scary to discover it in security library first.

At least `valgrind` is able to detect trivial cases of uninitialized
use of partially initialized unions.

`gcc-15` also provides `-fzero-init-padding-bits=unions` to flip the old
behaviour back on. This will allow nailing down bugs using a single
compiler version instead of comparing to `gcc-14`.

I suspect `gcc` historically zero-initialized the whole enums to be
closer to incomplete `struct` initialization
[rule](https://en.cppreference.com/w/c/language/struct_initialization).
But now it causes performance problems if the union branches are
different in size.

I suspect we'll see a few more projects affected by this change.

Have fun!
