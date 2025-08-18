---
title: "highway debugging example"
date: May 18, 2024
root: "http://trofi.github.io"
---

[`highway`](https://github.com/google/highway) test suite is a great
stress test for `gcc` vectorization and SIMD intrinsics code
generators:

- at compile time `highway` instantiates all the vector extensions your CPU could support
- at runtime it runs the tests on all the supported extensions against
  various vector sizes

It cover many corner cases of what could possibly go wrong with vectored
forms of various operations. A few past examples are:
[`PR110274`](https://gcc.gnu.org/PR110274),
[`PR110880`](https://gcc.gnu.org/PR110880),
[`PR111048`](https://gcc.gnu.org/PR111048),
[`PR111051`](https://gcc.gnu.org/PR111051),
[`PR115115`](https://gcc.gnu.org/PR115115).

While `highway` tests are quite small they are somewhat tricky to
extract into self-contained examples. In this post I'll write down a few
hacks I usually use to simplify this task.

## An example

Today test suite failed at me on today's `gcc-15` build against
`nixpkgs` as:

```
 1146 - HwyReverseTestGroup/HwyReverseTest.TestAllReverseLaneBytes/EMU128  # GetParam() = 2305843009213693952 (Subprocess aborted)
 1151 - HwyReverseTestGroup/HwyReverseTest.TestAllReverseBits/EMU128  # GetParam() = 2305843009213693952 (Subprocess aborted)
 1186 - HwyShuffle4TestGroup/HwyShuffle4Test.TestAllPer4LaneBlockShuffle/EMU128  # GetParam() = 2305843009213693952 (Subprocess aborted)
```

`TestAllReverseLaneBytes` is the test function. `EMU128` is an
(emulated) CPU target: the code does not use compiler intrinsics and
uses loops over scalar operations to emulate `SIMD`.

## Check latest `git`

`highway` is actively maintained. In case the failure is caused by a
`highway` bug (and not by a faulty compiler) chances are that it's
already fixed in latest version. Worth trying it first:

```
# get the build time depends into the development shell
$ nix develop -f ~/n libhwy

$$ git clone https://github.com/google/highway.git
$$ cd highway

$$ mkdir build
$$ cd build
$$ cmake ..
$$ make -j $(nproc) && make test
...
1146 - HwyReverseTestGroup/HwyReverseTest.TestAllReverseLaneBytes/EMU128  # GetParam() = 2305843009213693952 (Subprocess aborted)
1151 - HwyReverseTestGroup/HwyReverseTest.TestAllReverseBits/EMU128  # GetParam() = 2305843009213693952 (Subprocess aborted)
1186 - HwyShuffle4TestGroup/HwyShuffle4Test.TestAllPer4LaneBlockShuffle/EMU128  # GetParam() = 2305843009213693952 (Subprocess aborted)

```

The bug was still there!

## Enable single (simplest) target

`highway` uses heavy `C++` template code and various iterator macros to
compile the library for each supported CPU extension `highway` knows
about. This increases build times and complicated debugging via code
tweaking as code has to compile for all active targets, not just one.

I disabled all targets except the problematic one. In our case the
problematic target is `EMU128`. Thus, the local change to leave `EMU128`
as the only available option is:

```diff
--- a/hwy/detect_targets.h
+++ b/hwy/detect_targets.h
@@ -29,7 +29,7 @@
 // #define HWY_BASELINE_TARGETS (HWY_SSE4 | HWY_SCALAR)

 // Uncomment to override the default blocklist:
-// #define HWY_BROKEN_TARGETS HWY_AVX3
+#define HWY_BROKEN_TARGETS (HWY_AVX2 | HWY_SSE4 | HWY_SSE2 | HWY_SSSE3 | HWY_SSE4 | HWY_AVX3_SPR | HWY_AVX3_ZEN4 | HWY_AVX3)

 // Uncomment to definitely avoid generating those target(s):
 // #define HWY_DISABLED_TARGETS HWY_SSE4
```

Here I disabled anything that build system reports as supported.
Before the change I had `Compiled HWY_TARGETS:   AVX3_SPR AVX3_ZEN4 AVX3 AVX2 SSE4 SSSE3 SSE2`
in this output:

```
Config: emu128:0 scalar:0 static:0 all_attain:0 is_test:0
Compiled HWY_TARGETS:   AVX3_SPR AVX3_ZEN4 AVX3 AVX2 SSE4 SSSE3 SSE2
HWY_ATTAINABLE_TARGETS: AVX3_SPR AVX3_ZEN4 AVX3 AVX2 SSE4 SSSE3 SSE2 EMU128
HWY_BASELINE_TARGETS:   SSE2 EMU128
HWY_STATIC_TARGET:      SSE2
HWY_BROKEN_TARGETS:     Unknown
HWY_DISABLED_TARGETS:
Current CPU supports:   AVX2 SSE4 SSSE3 SSE2 EMU128 SCALAR
```

After the change I get `Compiled HWY_TARGETS:   EMU128` in this output:

```
Config: emu128:0 scalar:0 static:0 all_attain:0 is_test:0
Compiled HWY_TARGETS:   EMU128
HWY_ATTAINABLE_TARGETS: EMU128
HWY_BASELINE_TARGETS:   SSE2 EMU128
HWY_STATIC_TARGET:      EMU128
HWY_BROKEN_TARGETS:     AVX3_SPR AVX3_ZEN4 AVX3 AVX2 SSE4 SSSE3 SSE2
HWY_DISABLED_TARGETS:
Current CPU supports:   AVX2 SSE4 SSSE3 SSE2 EMU128 SCALAR
```

Leaving a single compiled target speeds the builds a few times up.
Then I picked the specific binary that implements failing test. In this
case it was `tests/reverse_test`:

```
$ make -j$(nproc) && ./tests/reverse_test
...
[==========] Running 1 test from 1 test suite.
[----------] Global test environment set-up.
[----------] 1 test from HwyReverseTestGroup/HwyReverseTest
[ RUN      ] HwyReverseTestGroup/HwyReverseTest.TestAllReverseLaneBytes/EMU128


u16x4 expect [0+ ->]:
  0xD49E,0x049E,0x0137,0x69D3,
u16x4 actual [0+ ->]:
  0xFF9E,0xFF9E,0x0137,0xFFD3,
Abort at reverse_test.cc:162: EMU128, u16x4 lane 0 mismatch:
  expected '0xD49E', got '0xFF9E'.

Aborted (core dumped)
```

Here we can see that instead of expected `0xD49E,0x049E,0x0137,0x69D3`
output our library did `0xFF9E,0xFF9E,0x0137,0xFFD3`.

## Shrink the test

The rest reduction is usually test-specific, but some of the hacks can
be applied to many tests. In this case I kept only one failing test:

```diff
--- a/hwy/tests/reverse_test.cc
+++ b/hwy/tests/reverse_test.cc
@@ -293,13 +295,7 @@ HWY_AFTER_NAMESPACE();

 namespace hwy {
 HWY_BEFORE_TEST(HwyReverseTest);
-HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverse);
-HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverse2);
-HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverse4);
-HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverse8);
 HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverseLaneBytes);
-HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverseBits);
-HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverseBlocks);
 HWY_AFTER_TEST();
 }  // namespace hwy
```

The test was still failing (sometimes it's not the case when mere
presence of unrelated code changes the inlining and vectorization
decisions).
Then I shrunk the cases down to 16-bit element sizes by inlining a
`ForUI163264` definition:

```diff
--- a/hwy/tests/reverse_test.cc
+++ b/hwy/tests/reverse_test.cc
@@ -248,7 +249,7 @@ HWY_NOINLINE void TestAllReverse8() {
 }

 HWY_NOINLINE void TestAllReverseLaneBytes() {
-  ForUI163264(ForPartialVectors<TestReverseLaneBytes>());
+  ForPartialVectors<TestReverseLaneBytes>()(uint16_t());
 }
```

Then I removed all the other unrelated test asserts from the
`hwy/tests/reverse_test.cc` file.
Then I inlined obvious template parameters right into local test class.
Then I extracted random generated data used in the failing vectors (using
`printf()` statements) and inlined values into `.cc` file. Sometimes
I had to sprinkle `__attribute__((noipa))` attributes on local functions
to inhibit too eager constant folding.

## `gdb` hints

To explore generated code in `gdb` I explored `hwy::N_<target>::`
namespace as:

```
$ gdb tests/reverse_test
(gdb) disassemble hwy::N_EMU128::TestAllReverseLaneBytes
...
   <+113>:   mov    0x3d190(%rip),%rax        # 0x4dc68
   <+120>:   mov    $0xffffd49e,%esi
   <+125>:   xor    %edx,%edx
   <+127>:   mov    $0x8,%edi
   <+132>:   mov    %rax,0x0(%rbp)
   <+136>:   mov    %si,(%r12)

   <+141>:   movzwl 0x2(%rbp),%eax
   <+145>:   xor    %esi,%esi
   <+147>:   rol    $0x8,%ax
   <+151>:   mov    %ax,0x2(%r12)

   <+157>:   movzwl 0x4(%rbp),%eax
   <+161>:   rol    $0x8,%ax
   <+165>:   mov    %ax,0x4(%r12)

   <+171>:   movzwl 0x6(%rbp),%eax
   <+175>:   rol    $0x8,%ax
   <+179>:   mov    %ax,0x6(%r12)

   <+185>:   mov    0x0(%rbp),%rax
   <+189>:   movq   %rax,%xmm0
   <+194>:   movdqa %xmm0,%xmm1
   <+198>:   psllw  $0x8,%xmm0
   <+203>:   psraw  $0x8,%xmm1
   <+208>:   por    %xmm0,%xmm1
   <+212>:   movq   %xmm1,0x8(%rsp)
```

Here I was lucky! I immediately spotted the bug. We see both:

- 16-bit wide move/rotate/move: `movzwl / rol / mov` (looks correct)
- and 128-bit wide move/rotate/move: `movq / psllw / psraw / por / movq`

The rotate part is broken here: it should have been logical `psrlw`
shift, not arithmetic (sign-preserving) `psraw` shift.
At this point my test looked this way:

```c++
HWY_NOINLINE void TestAllReverseLaneBytes() {
    const CappedTag<uint16_t, 4, 0> d;

    const size_t N = Lanes(d);
    fprintf(stderr, "N = %zu\n", N);
    auto in = AllocateAligned<uint16_t>(N);
    auto expected = AllocateAligned<uint16_t>(N);

    fprintf(stderr, "iter\n");
        in[0] = 0x9ed4u;
        in[1] = 0x049eu;
        in[2] = 0x0137u;
        in[3] = 0x69d3u;
        expected[0] = ReverseBytesOfValue(in[0]);
        expected[1] = ReverseBytesOfValue(in[1]);
        expected[2] = ReverseBytesOfValue(in[2]);
        expected[3] = ReverseBytesOfValue(in[3]);

    const auto v = Load(d, in.get());
    HWY_ASSERT_VEC_EQ(d, expected.get(), ReverseLaneBytes(v));
}
````

I looked at the `ReverseLaneBytes()` implementation. It had two parts.
The first part was generic for all targets:

```c++
// from hwy/ops/generic_ops-inl.h
template <class V, HWY_IF_T_SIZE_V(V, 2)>
HWY_API V ReverseLaneBytes(V v) {
  const DFromV<V> d;
  const Repartition<uint8_t, decltype(d)> du8;
  return BitCast(d, Reverse2(du8, BitCast(du8, v)));
}
```

And the second part was `EMU128`-specific:

```c++
// from hwy/ops/emu128-inl.h
template <class D>
HWY_API VFromD<D> Reverse2(D d, VFromD<D> v) {
  VFromD<D> ret;
  for (size_t i = 0; i < MaxLanes(d); i += 2) {
    ret.raw[i + 0] = v.raw[i + 1];
    ret.raw[i + 1] = v.raw[i + 0];
  }
  return ret;
}
```

I inlined the above definitions into the test and got this:

```c++
// $ cat hwy/tests/reverse_test.cc
#include <stddef.h>

#undef HWY_TARGET_INCLUDE
#define HWY_TARGET_INCLUDE "tests/reverse_test.cc"
#include "hwy/foreach_target.h"  // IWYU pragma: keep
#include "hwy/highway.h"
#include "hwy/tests/test_util-inl.h"

HWY_BEFORE_NAMESPACE();
namespace hwy {
namespace HWY_NAMESPACE {

template <class D>
__attribute__((noipa))
static VFromD<D> Reverse2_(D d, VFromD<D> v) {
  VFromD<D> ret;
  for (size_t i = 0; i < MaxLanes(d); i += 2) {
    ret.raw[i + 0] = v.raw[i + 1];
    ret.raw[i + 1] = v.raw[i + 0];
  }
  return ret;
}

HWY_NOINLINE void TestAllReverseLaneBytes() {
    const CappedTag<uint16_t, 4, 0> d;

    const size_t N = Lanes(d); // 4
    auto in = AllocateAligned<uint16_t>(N);
    auto expected = AllocateAligned<uint16_t>(N);

    in[0] = 0x9ed4u;
    in[1] = 0x049eu;
    in[2] = 0x0137u;
    in[3] = 0x69d3u;
    expected[0] = 0xd49eu;
    expected[1] = 0x9e04u;
    expected[2] = 0x3701u;
    expected[3] = 0xd369u;

    const auto v = Load(d, in.get());
    const Repartition<uint8_t, decltype(d)> du8;
    const auto r = BitCast(d, Reverse2_(du8, BitCast(du8, v)));
    HWY_ASSERT_VEC_EQ(d, expected.get(), r);
}

// NOLINTNEXTLINE(google-readability-namespace-comments)
}  // namespace HWY_NAMESPACE
}  // namespace hwy
HWY_AFTER_NAMESPACE();

#if HWY_ONCE

namespace hwy {
HWY_BEFORE_TEST(HwyReverseTest);
HWY_EXPORT_AND_TEST_P(HwyReverseTest, TestAllReverseLaneBytes);
HWY_AFTER_TEST();
}  // namespace hwy

#endif
```

## Make sure it's an optimizer

Adding `#pragma GCC optimize(0)` to the beginning of the file makes the
bug to go away. It's a good hint that it's a compiler bug: the test looks
obviously correct (not much hidden code is left in the templates).
But the only way to make sure is to finish the reduction down to a
self-contained example. We will need it anyway to report upstream.

## Final result

After a few manual extra inlines and simplifications I got this
self-contained example:

```c
// $ cat bug.c
typedef unsigned char u8;

__attribute__((noipa))
static void fill_src(u8 * src) {
    src[0] = 0x00; src[1] = 0xff;
}

__attribute__((noipa))
static void assert_dst(const u8 * dst) {
    if (dst[0] != 0xff) __builtin_trap();
    if (dst[1] != 0x00) __builtin_trap();
}

int main() {
    u8 src[8] __attribute__((aligned(16))) = { 0 };
    u8 dst[8] __attribute__((aligned(16))) = { 0 };

    // place 0x00 into src[0] and 0xFF into src[1]
    fill_src(src);

    // swap bytes:
    // place 0xFF into dst[0], 0x00 into dst[1]
    for (unsigned long i = 0; i < 8; i += 2) {
        dst[i + 0] = src[i + 1];
        dst[i + 1] = src[i + 0];
    }

    // make sure bytes swapped
    assert_dst(dst);
}
```

Triggering:

```
$ gcc bug.c -o a -O1 && ./a
$ gcc bug.c -o a -O2 && ./a
Illegal instruction (core dumped)
```

## Upstream report

I reported the bug as [`PR115146`](https://gcc.gnu.org/PR115146).
Bisecting `gcc` pointed me to this
["vector shift" change](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=a71f90c5a7ae29).
This change looks very close to the culprit as the code explicitly picks
the "arithmetic" flavor of shift instruction (should be "logical"
instead).
By now the original change author already provided a test patch in the
report! So quick!

Have fun!

