---
title: "gcc-12 journey"
date: April 24, 2022
---

It's the end of April. This [means](https://gcc.gnu.org/releases.html)
`gcc-12` will likely get released soon.
6 months passed since I wrote about fancy bugs in development versions
of `gcc`. Nowadays I use [weekly `gcc` snapshots](https://gcc.gnu.org/pub/gcc/snapshots/LATEST-12/)
against `nixpkgs` tree. Usually I rebuild just my system against
weekly `gcc` shapshots. But sometimes I build random packages to check
how `gcc` behaves on more exotic codebases. Most of the time
everything Just Works. It would be too boring if things worked all the
time.

Since November 2021 I encountered ~30 `gcc` bugs which
makes it roughly one bug a week. I could hardly handle more than that.
I'll list most of these bugs below. `ICE`s (internal compiler errors) are
usually the easiest to find and report. Wrong-code bugs are more
interesting and are also way harder to minimize and report.

I usually get to know a lot more about `gcc` from wrong-code cases.
This time I got only two of them. I'll start from them.

## `-fipa-modref` strikes again

[`ipa/103432`](https://gcc.gnu.org/PR103432): `gcc` miscompiled
`libjxl` in a way that test suite started failing.

I can't provide a small failure example as the bug requires specific
inline to happen within one compilation.
The effect program result difference when built with `-O0` versus
`-O2`. The test was a floating point workload. Those always have a
potential of being not-a-bug, but an expected precision loss (like
`fma()` precision change).
It took me a while to reduce original test manually to something that
is still a valid program but is small enough to be able to debug it.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=a70faf6e4df7481c2c9a08a06657c20beb3043de)
tells us what kind of error it was in `gcc`: information about inferred
effects of functions was calculated but not actually used, something
else's details were used. It's a nasty kind of bugs: things work for
smaller programs and break only in non-trivial scenarios.
I'm glad I spent some time to extract the reproducer.

## global constructors wrong code

[`c++/104031`](https://gcc.gnu.org/PR104031): `gcc` generated
wrong global initializer for `nix`. Minimal example:

```c++
struct vector
{
  vector(){}  ~vector(){}
};
struct Info {
    vector args;
    int arity = 0;
};
struct RegisterPrimOp
{
    [[gnu::noipa, gnu::noinline]]
    RegisterPrimOp(Info info) {
        if (info.arity != 0)
            __builtin_trap();
    }
};
static RegisterPrimOp s_op({
    .args = vector{},
    .arity = 0,
});
int main() {}
```

The code's idea is to always have `arity` initialized to `0`. In
practice `gcc-12` managed to put something else into `arity`:

```
# ok:
$ g++-11.2.0 main.cc -o main -O2 && ./main

# bad:
$ g++-12.0.0 main.cc -o main -O2 && ./main
Illegal instruction (core dumped)
```

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=aeca44768d54b089243004d1ef00d34dfa9f6530)
sheds some light into why it happens: this declaration syntax is special
and it was not handled in some definitions. As a result `arity` was
uninitialized.

## type comparison ICE

[`c++/101715`](https://gcc.gnu.org/PR103279): `gcc` `ICE`
on `clang`'s `compiler-rt` library. Minimized example:

```c++
template <class A> struct S {
  S<A> & bar() noexcept(A::value);
  S<A> & foo() noexcept(A::value);
};

template <class A> S<A> & S<A>::foo() noexcept(A::value) {}
```

`ICE`:

```
$ g++-12.0.0 -c a.c.cpp -std=c++14
# no crash

$ g++-12.0.0 -c a.c.cpp -std=c++17
a.c.cpp:6:56: internal compiler error: canonical types differ for identical types 'S<A>& (S<A>::)() noexcept (A::value)' and 'S<A>& (S<A>::)() noexcept (A::value)'
    6 | template <class A> S<A> & S<A>::foo() noexcept(A::value) {}
      |                                                        ^
0xc3f6ee comptypes(tree_node*, tree_node*, int)
        ../../gcc-12-20211226/gcc/cp/typeck.c:1558
...
```

Note that `-std=c++14` is not enough to trigger the failure. `gcc-11`
did change the default from `gnu++14` to `gnu++17`.
Sometimes `gcc` needs to compare types for equality. That usually
happens in template instantiation when closest specialization is picked.
It might sound easy, but the subtlety is in detail: template types can
refer to other (possibly not yet defined) template types in their
definition.
When instantiation happens it's crucial to resolve identical types into
the same canonical type.

In the [commit message](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=3abcbf243239f9576a60f4ce7f8ee4b3fa14784b)
Marek added great explataion how canonical types came into divergence.

## null warnings in dynamic_cast expressions

[`c++/96003`](https://gcc.gnu.org/PR96003): `gcc` failed to build
`firefox` (due to `-Werror=null`). Minimized example:

```c++
struct A { virtual ~A (); };
struct B { virtual ~B (); void f (); };

void f (A *p)
{
    if (dynamic_cast<B*>(p))
        dynamic_cast<B*>(p)->f ();
}
```

Build error:

```
$ gcc -O2 -S -Wall t.C
t.C: In function ‘void f(A*)’:
t.C:7:29: warning: ‘this’ pointer is null [-Wnonnull]
    7 |     dynamic_cast<B*>(p)->f ();
      |                             ^
t.C:2:32: note: in a call to non-static member function ‘void B::f()’
    2 | struct B { virtual ~B (); void f (); };
      |                                ^
```

This time it's not an `ICE` but a seemingly reasonable warning.
Ideally original code should be restructured into something simpler, like:

```c++
struct A { virtual ~A (); };
struct B { virtual ~B (); void f (); };

void f (A *p)
{
    B * b = dynamic_cast<B*>(p);
    if (b)
        b->f ();
}
```

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=df5cf47a978aaeb53fc2b18ff0b22eb4531a27d8)
suppressed warnings for some cases where similar code is generated by
`gcc` itself (instead of being handwritten). More complicated cases
will still issue warnings.
You might need to clean your code up to avoid similar warnings.

## vectored intrinsics `ICE`

[`middle-end/102080`](https://gcc.gnu.org/PR102080): `gcc` ICE
on `firefox` build. Minimized example:

```c++
// cat dec_reconstruct.cc.cc
#pragma GCC target "avx"
typedef float __m256 __attribute__((__vector_size__(32)));
__m256 _mm256_blendv_ps___Y, _mm256_blendv_ps___M, _mm256_mul_ps___A,
    _mm256_mul_ps___B, IfThenElse___trans_tmp_9;
void IfThenElse(__m256 no) {
  IfThenElse___trans_tmp_9 = __builtin_ia32_blendvps256(
      no, _mm256_blendv_ps___Y, _mm256_blendv_ps___M);
}
#pragma GCC target "avx512vl"
void EncodedFromDisplay() {
  __m256 __trans_tmp_11 = _mm256_mul_ps___A * _mm256_mul_ps___B;
  IfThenElse(__trans_tmp_11);
}
```

`ICE`:

```
$ /tmp/gcc-c/gcc/xg++ -B/tmp/gcc-c/gcc -c dec_reconstruct.cc.cc -O0
# no crash

$ /tmp/gcc-c/gcc/xg++ -B/tmp/gcc-c/gcc -c dec_reconstruct.cc.cc -O2
during RTL pass: expand
dec_reconstruct.cc.cc: In function 'void EncodedFromDisplay()':
dec_reconstruct.cc.cc:10:6: internal compiler error: in expand_insn, at optabs.c:7946
   10 | void EncodedFromDisplay() {
      |      ^~~~~~~~~~~~~~~~~~
```

The sample is not very readable but it's essentially a direct call of
`__builtin_ia32_blendvps256()` `AVX` intrinsic. Those usually get
translated 1-to-1 into CPU instructions.
Here the crash happens in `expand_insn()` where middle end expands
`GIMPLE` (`C` style tree-like representation) into `RTL` (assembly style
instruction-like representation). Assertion complains about unexpected
arguments.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=a26ff83ed07e33c4aa46f3314553c0d15ca21100)
adjusts one of `GIMPLE` simplifiers to not generate invalid `GIMPLE`
around vectored conditionals.

## atomic intrinsics `ICE`

[`tree-optimization/103268`](https://gcc.gnu.org/PR103268): `gcc` ICE
on `glib` build. Minimized example:

```c
static int si;
long
test_types (long n)
{
  unsigned int u2 = __atomic_fetch_xor (&si, 0, 5);
  return u2;
}
```

`ICE`:

```
$ gcc -O2 -S x.c
during GIMPLE pass: fab
x.c: In function ‘test_types’:
x.c:3:1: internal compiler error: in optimize_atomic_bit_test_and, at tree-ssa-ccp.c:3645
    3 | test_types (long n)
      | ^~~~~~~~~~
0x1515c9d optimize_atomic_bit_test_and
    /export/gnu/import/git/gitlab/x86-gcc/gcc/tree-ssa-ccp.c:3645
```

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=074ee8d9a91d7573c1e8774a22e4e08f923adb18)
suggests the tree matcher had an accident fall-through code.
It was supposed to reject unhandled cases instead of attempting
to generate something nonsensical.

## vectored instructions selector `ICE`

[`target/103557`](https://gcc.gnu.org/PR103557): `gcc` `ICE`
on `tpm2-tss` build. Minimized example:

```c
struct {
  char sm3_256[32];
} TPMU_HA;
typedef struct {
  short size;
  char buffer[sizeof(TPMU_HA)];
} TPM2B_DIGEST;
typedef struct {
  TPM2B_DIGEST auths[3];
} TSS2L_SYS_AUTH_COMMAND;

void Tss2_Sys_Clear(TSS2L_SYS_AUTH_COMMAND);

void sysContext(void) {
  TPM2B_DIGEST nonce = {};
  TSS2L_SYS_AUTH_COMMAND sessionsDataIn = {{nonce}};
  Tss2_Sys_Clear(sessionsDataIn);
}
```

`ICE`:

```
$ gcc-12.0.0 -O2  -c a.c.c -o a.o
during RTL pass: sched2
a.c.c: In function 'sysContext':
a.c.c:19:1: internal compiler error: Segmentation fault
   19 | }
      | ^
0x1e22687 internal_error(char const*, ...)
        ???:0
0xe7e43c memory_operand(rtx_def*, machine_mode)
        ???:0
0x167e598 get_attr_memory(rtx_insn*)
        ???:0
0x19a4bc0 insn_default_latency_generic(rtx_insn*)
        ???:0
0x1c74ad3 insn_sched_cost(rtx_insn*)
        ???:0
0x1c77934 dep_cost_1(_dep*, unsigned int)
        ???:0
0x1c79d7f set_priorities(rtx_insn*, rtx_insn*)
        ???:0
0xec28e2 compute_priorities()
        ???:0
```

In this case instruction selector fails to fetch the details
around memory reference accessed by instruction.
[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=9519b694afbf9a35c36cf9f14d35d1c0e9e8cacc)
fixes `x86`-specific vectored instruction definitions to accept
expected inputs.

## another atomics `ICE`

[`tree-optimization/103682`](https://gcc.gnu.org/PR103682): `gcc` `ICE`
on some cryptographic code. Minimized example:

```c++
#include <atomic>

bool bug(std::atomic<unsigned> & ready, unsigned u) {
  return (ready.fetch_and(~u) & u);
}
```

`ICE`:

```
$ g++-12.0.0 -Ofast -c bug.cpp
during GIMPLE pass: fab
bug.cpp: In function 'bool bug(std::atomic<unsigned int>&, unsigned int)':
bug.cpp:6:6: internal compiler error: gimple check: expected gimple_assign(error_mark), have gimple_nop() in gimple_assign_rhs_code, at gimple.h:2852
    6 | bool bug(std::atomic<unsigned> & ready, unsigned u) {
      |      ^~~
0x20a58f7 internal_error(char const*, ...)
        ???:0
0x7c76dd gimple_check_failed(gimple const*, char const*, int, char const*, gimple_code, tree_code)
        ???:0
```

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=a2a0c91b47537b16908981e206f4e42db8425eca)
shows us it was another case where one particular case of `GIMPLE`
form was not handled.

## another template instantiation ICE

[`c++/103769`](https://gcc.gnu.org/PR103769): `gcc` `ICE`
on `openttd`. Minimized example:

```c++
template <typename T> using t = T;
template <typename...> struct s;
template <typename... Args> s<t<Args>...> f() { f<void>(); }
```

`ICE`:

```
$ g++-12.0.0 --param=hash-table-verification-limit=1000 -O1 -o a.o -c bug.cpp

hash table checking failed: equal operator returns true for a pair of values with a different hash value
bug.cpp: In substitution of 'template<class ... Args> s<Args ...> f() [with Args = {void}]':
bug.cpp:3:56:   required from here
bug.cpp:3:43: internal compiler error: in hashtab_chk_error, at hash-table.c:137
    3 | template <typename... Args> s<t<Args>...> f() { f<void>(); }
      |                                           ^
0x9c316d hashtab_chk_error()
        ../../gcc-12-20220102/gcc/hash-table.c:137
0xbeca15 hash_table<spec_hasher, false, xcallocator>::verify(spec_entry* const&, unsigned int)
        ../../gcc-12-20220102/gcc/hash-table.h:1036
0xbecb6f hash_table<spec_hasher, false, xcallocator>::find_with_hash(spec_entry* const&, unsigned int)
        ../../gcc-12-20220102/gcc/hash-table.h:921
0xbd58bc lookup_template_class_1
        ../../gcc-12-20220102/gcc/cp/pt.c:9905
```

This is another `c++` frontend bug where identical types have
different objects. Here I found out about `--param=hash-table-verification-limit=1000`
option which enables more frequent type mismatches like this.
Without this option the bug disappears when program is still large.

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=07be8f8da4c6840a1fd6b2229b147e50cc6f03dc)
shows it was a case of reused type object in `typedef` simplification.

## yet another vectored expansion `ICE`

[`target/103842`](https://gcc.gnu.org/PR103842): `gcc` `ICE`
on `ilmbase-2.5.7`. Minimized example:

```c
void abs(float *);
struct Matrix33 {
  float x[3][3];
  float *operator[](int i) { return x[i]; }
  Matrix33();
  Matrix33(float f, float g) {
    x[1][0] = x[1][1] = x[1][2] = f;
    x[2][0] = g;
  }
  void equalWithAbsError();
  Matrix33 inverse() {
    Matrix33 s(x[1][2] - x[1][2], x[1][1] - x[1][1]);
    float r = s[2][0];
    if (r)
      for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j)
          s[i][j] /= r;
    for (int i = 0;;) {
      float *__trans_tmp_2 = s[i];
      abs(__trans_tmp_2);
    }
  }
};
void __assert_fail() {
  Matrix33 m, inv1 = m.inverse(), ident1 = inv1;
  ident1.equalWithAbsError();
}
```

`ICE`:

```

$ g++-12.0.0 -O3 -c bug.cpp.cpp
during RTL pass: expand
bug.cpp.cpp: In function 'void __assert_fail()':
bug.cpp.cpp:27:5: internal compiler error: Segmentation fault
   27 |     }
      |     ^
0x21196c6 internal_error(char const*, ...)
        ???:0
```

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=242783c52c22ed96eae722b2fa9847711ac84699)
fixes another case of `x86`-specific instruction selector that
generates unexpected instruction parameters.

## one more vectored instruction `ICE`

[`target/103894`](https://gcc.gnu.org/PR103894): `gcc` `ICE`
on `valgrind`.

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=1096ab1775636f35de9c6661f8f71f03299af998)
fixes marking of available SSE instructions in 32-bit mode.

## precompiled headers

[`target/103910`](https://gcc.gnu.org/PR103910): `gcc` `ICE`
on `openjdk`. Minimized example:

```c++
#include "precompiled.hpp"
void *operator new(unsigned long, void *__p) { return __p; }
#define MEMORY_TYPES_DO(f) f(mtNone, )
enum MEMFLAGS {};
#define MEMORY_TYPE_SHORTNAME(type, human_readable) MEMFLAGS type;
MEMORY_TYPES_DO(MEMORY_TYPE_SHORTNAME) struct GrowableArrayView {
  GrowableArrayView(int *, int, int);
};
int *GrowableArrayWithAllocator_data;
struct GrowableArrayWithAllocator : GrowableArrayView {
  GrowableArrayWithAllocator(int initial_max)
      : GrowableArrayView(GrowableArrayWithAllocator_data, initial_max, 0) {
    for (int i = 0; i < initial_max; i++)
      new (&GrowableArrayWithAllocator_data[i]) int();
  }
};
struct GrowableArrayMetadata {
  GrowableArrayMetadata(MEMFLAGS);
};
struct GrowableArray : GrowableArrayWithAllocator {
  GrowableArrayMetadata _metadata;
  GrowableArray(int initial_max)
      : GrowableArrayWithAllocator(initial_max), _metadata(mtNone) {}
};
struct SourceObjList {
  SourceObjList();
};
SourceObjList::SourceObjList() { GrowableArray(128 * 1024); }
```

`ICE`:

```
$ rm -rf ph
$ mkdir -p ph
$ touch precompiled_.hpp # create empty file
$ ./xg++ -B. -O3 -march=opteron -fcheck-new -c precompiled_.hpp -o ph/precompiled.hpp.gch
$ ./xg++ -B. -O3 -march=opteron -fcheck-new -Iph -c archiveBuilder.cpp -o a.o

during GIMPLE pass: aprefetch
archiveBuilder.cpp: In constructor ‘SourceObjList::SourceObjList()’:
archiveBuilder.cpp:28:1: internal compiler error: in gimple_build_call, at gimple.c:267
   28 | SourceObjList::SourceObjList() { GrowableArray(128 * 1024); }
      | ^~~~~~~~~~~~~
0xd2845f gimple_build_call(tree_node*, unsigned int, ...)
        gcc/gimple.c:267
0x12880c8 emit_mfence_after_loop
        gcc/tree-ssa-loop-prefetch.c:1300
0x12880c8 mark_nontemporal_stores
        gcc/tree-ssa-loop-prefetch.c:1359
0x12880c8 loop_prefetch_arrays
        gcc/tree-ssa-loop-prefetch.c:1955
0x12880c8 tree_ssa_prefetch_arrays()
        gcc/tree-ssa-loop-prefetch.c:2031
0x1288be9 execute
        gcc/tree-ssa-loop-prefetch.c:2097
```

This is an unusual and scary case: precompiled headers are implemented
as a serialization of part of `gcc` heap. The serialization tries
hard to be very fast and cuts corners in various places. Up to the point
where building positional-independent `gcc` binary breaks `GCH`:
<https://gcc.gnu.org/PR71934>. It's very fragile. It does not have to be.
To make `GCH` somehow work `gcc`'s internals have garbage collector
implementation. It requires global variables to be annotated with
`gcc`-specific [`GTY` markings](https://gcc.gnu.org/onlinedocs//gccint/GTY-Options.html)
that describe heap layout to garbage collector.

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=d243f4009d8071b734df16cd70f4c5d09a373769)
hints at an `x86_mfence` global variable that did not have the marking.
Why did it break only with `GCH`?
Without `GCH`, `x86_mfence` is referenced via another global object which
has `GTY` marking. Both are constructed via normal means of `gcc`
parsing the source.

With `GCH`, that another global object is restored at a different location
and the `decl` that is referenced by `x86_mfence` is `GC`'ed out.

## `C++` frontend constructor `ICE`

[`c++/103910`](https://gcc.gnu.org/PR104583): `gcc` `ICE`
on `dolphin-emu`. Minimized example:

```c
struct TVec3 {
  TVec3();
  int data;
};
using Vec3 = TVec3;
struct {
  Vec3 position{};
} EmulatePoint_state;
void EmulatePoint() { EmulatePoint_state = {}; }
```

`ICE`:

```
$ /tmp/gcc/xg++ -B/tmp/gcc -c bug.cc -freport-bug
bug.cc: In function ‘void EmulatePoint()’:
bug.cc:9:42: internal compiler error: in cp_gimplify_expr, at cp/cp-gimplify.cc:746
    9 | void EmulatePoint() { EmulatePoint_state = {}; }
      |                       ~~~~~~~~~~~~~~~~~~~^~~~
0x7a0fb2 cp_gimplify_expr(tree_node**, gimple**, gimple**)
        gcc/cp/cp-gimplify.cc:746
0xd9fb80 gimplify_expr(tree_node**, gimple**, gimple**, bool (*)(tree_node*), int)
        gcc/gimplify.cc:14893
0xdab095 gimplify_init_ctor_preeval
        gcc/gimplify.cc:4678
```

In this case `gcc` was unable to generate `GIMPLE` out of
`EmulatePoint_state = {};` statement.

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=f8c1f29a0b47b4b4a3c1506678f7ca2ce4b7ffbb)
expands set of cases that look like a constructor call.

## fun option handling bug

[`middle-end/104705`](https://gcc.gnu.org/PR104705): `gcc` `ICE`s when
tried to build `ghc`.

Normally I would expect `ghc` to be the last project to crash `gcc`
as it does not do anything fancy in `C` land. And yet here we are.
Minimized example:

```c
#pragma GCC optimize "foo"
#pragma GCC push_options
#pragma GCC pop_options
```

`ICE`:

```
$ /tmp/bg/gcc/xgcc -B/tmp/bg/gcc -c a.c  -O2 -Wall
a.c:1:9: warning: bad option ‘-ffoo’ to pragma ‘optimize’ [-Wpragmas]
    1 | #pragma GCC optimize "foo"
      |         ^~~
a.c:3:9: internal compiler error: ‘global_options’ are modified in local context
    3 | #pragma GCC pop_options
      |         ^~~
```

A cryptic assert.
[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=111754595cf8d3a8ae7063a42ac4cea18a304711)
says that `-g*` option was not accounted for when option contexts are
created and destroyed with pragmas.

## unexpected analyzer `ICE`s

[`analyzer/105074`](https://gcc.gnu.org/PR105074): `gcc` `ICE` when
tried to build `gnutls`.

```c
void _gnutls_log(const char *);
static void _gnutls_ocsp_verify_mandatory_stapling(void) {
  _gnutls_log(__func__);
}
void check_ocsp_response_gnutls_x509_cert_verify_peers(void) {
  _gnutls_ocsp_verify_mandatory_stapling();
}
```

`ICE`:

```
$ /tmp/gb/gcc/xgcc -B/tmp/gb/gcc -O2 -fanalyzer -c cert-session.c

during IPA pass: analyzer
In function ‘_gnutls_ocsp_verify_mandatory_stapling’,
    inlined from ‘check_ocsp_response_gnutls_x509_cert_verify_peers’ at cert-session.c:7:3:
cert-session.c:4:3: internal compiler error: Segmentation fault
    4 |   _gnutls_log(__func__);
      |   ^~~~~~~~~~~~~~~~~~~~~
0xdd6bc3 crash_signal
        gcc/toplev.cc:322
0x8a8120 cgraph_node::get_edge(gimple*)
        gcc/cgraph.cc:744
0x121059c ipa_ref_requires_tracking
        gcc/analyzer/region.cc:1192
0x121059c symnode_requires_tracking_p
        gcc/analyzer/region.cc:1235
0x121059c ana::decl_region::calc_tracked_p(tree_node*)
        gcc/analyzer/region.cc:1254
0x1234786 ana::decl_region::decl_region(unsigned int, ana::region const*, tree_node*)
```

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=1203e8f7880c9751ece5f5302e413b20f4608a00)
adjusts the checks for expressions without explicit declaration.

On a surface side it's an obscure `-fanalyzer` crash on `__func__`
expression that should not pop up frequently. Perhaps `gnutls` is a
rare package that enables `-fanalyser`?
No. If we look at the source of `-fanalyzer` flag we'll find that it's
one of default warnings of ... [`gnulib`(!)](https://git.savannah.gnu.org/cgit/gnulib.git/commit/?id=3831e2e9f355c557b0c0ed9712548b62feaf694f).
We will probably see a lot more analyzer crashes as it's one of the
more complex pieces of `gcc`.

## `ia64` `RTL` mach `ICE`

[`target/105215`](https://gcc.gnu.org/PR105215): `gcc` `ICE`
on `openssl` (`ia64` target).

Not fixed yet.

## `ia64` `RTL` mach `ICE`

[`target/105247`](https://gcc.gnu.org/PR105247): `gcc` `ICE`
on `sqlite-3.38.2` (`ia64` target). Minimal example:

```c
int sqlite3CodeVerifySchemaAtToplevel_pToplevel_0;
void sqlite3CodeVerifySchema();
void sqlite3FindInIndex_pParse() {
  int i = -8;
  sqlite3CodeVerifySchema(sqlite3FindInIndex_pParse, i);
}
void sqlite3CodeVerifySchema(int, int iDb) {
  sqlite3CodeVerifySchemaAtToplevel_pToplevel_0 |= 1 << iDb;
}
```

`ICE`:

```
$ ia64-unknown-linux-gnu-gcc -O1 -c sqlite3-sqlite3.o.c -o a.o

during RTL pass: cse1
sqlite3-sqlite3.o.c: In function 'sqlite3FindInIndex_pParse':
sqlite3-sqlite3.o.c:7:1: internal compiler error: in decompose, at rtl.h:2288
    7 | }
      | ^
0xa02263 wi::int_traits<>::decompose()
        ../../gcc-12-20220410/gcc/rtl.h:2288
0xa02263 wide_int_ref_storage<>::wide_int_ref_storage<>()
        ../../gcc-12-20220410/gcc/wide-int.h:1024
0xa02263 generic_wide_int<>::generic_wide_int<>()
        ../../gcc-12-20220410/gcc/wide-int.h:782
0xa02263 wide_int_storage::wide_int_storage<>()
```

[The change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=ba2f60499dd4a3bc1bb4e99fa12dda3bc1548519)
has a great explanation of the details by Jakub: arithmetic shift
(to implement `1 << iDb`) on `ia64` is slightly different from
other targets: it's defined only for 64-bit type (`long` /
`unsigned ulong` types).

## `popcount` and `libgcc`

[`middle-end/105253`](https://gcc.gnu.org/PR105253): `gcc` fails
to build `linux`. Minimal example:

```c
int
foo (unsigned long long *p)
{
  int i, cnt = 0;
  unsigned long long elem;
  for (i = 0; i < (256 / 64); i++)
    {
      elem = p[i];
      for (; elem; cnt++)
        elem &= elem - 1;
    }
  return cnt;
}
```

Link error:

```
ERROR: modpost: "__popcountdi2" [drivers/net/ethernet/broadcom/bnx2x/bnx2x.ko] undefined!
ERROR: modpost: "__popcountdi2" [drivers/gpu/drm/amd/amdgpu/amdgpu.ko] undefined!
```

Long time ago `gcc` used to produce direct CPU instructions to
implement this code. Nowadays `gcc` recognizes this pattern
and converts it to a `__builtin_popcountl()` call.
If CPU supports `popcnt` instruction then `gcc` emits it as is.
But if CPU has no support for it `gcc` generates `__popcountdi2`
external function call. `__popcountdi2` is implemented in `libgcc`
for all targets.
Normally such a replacement just works. But `linux` kernel does not use
`libgcc` for
various reasons and prefers to re-implement such `builtin`s (division,
shifts, `memcpy`, `strlen` operations). But `popcout` ones did not
occur frequently enough to be re-implemented in kernel.

One of the fixes would be to extend `linux` kernel with `popcount`.
But so far `gcc`
[tweaked](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=29c46490de4616b911fccb34a9479f768fb51e94)
cost heuristics to avoid such conversion for targets that don't have
efficient `popcnt`.

Until next time perhaps :)

## Parting words

Most of these bugs are short-lived and would have no chance to slip
into a release. Most ICEs are also not hard to work around when
encountered.
The wrong-code bugs are the subtlest. These usually live for a while
until they get noticed.

Bug stats by subsystem:

- 6 `target`: 3 vectorization bugs, 2 `ia64` bugs, 1 `GCH` bug
- 5 `c++` frontend
- 3 `middle-end`: vectorization, option parsing and `popcount` instruction one bug each
- 2 `tree-opt`: both are `atomics` bugs
- 1 `ipa`
- 1 `analyzer`

If not for `ia64` bugs `c++` would be the primary source of bugs.

`gcc-12 is very close to a release cut date. I hope it to be smooth.

As usual here is a list if notable changes for upcoming release:
<https://gcc.gnu.org/gcc-12/changes.html>

If you feel like it do give the `gcc` snapshot a try.

Have fun!
