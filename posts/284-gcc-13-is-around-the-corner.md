---
title: "gcc-13 is around the corner"
date: April 20, 2023
---

It's the end of April again and that means `gcc-13` will be released
very soon. Do check out [porting page](https://gcc.gnu.org/gcc-13/porting_to.html)
to see what will likely cause problems for you on the upgrade. Most of
the issues listed there bit me over past year.

But that's not the topic of this post. I kept using weekly `gcc`
snapshots of `master` branch for my desktop.

Since [October 2022](/posts/262-a-few-more-gcc-13-bugs.html) I
encountered a few new bugs. Let's have a look at some of them.

### gori ICE

[tree-optimization/109274](https://gcc.gnu.org/PR109274): `gcc` `ICE`d
on `afdko`.

Minimal `afdko` crash:

```c
float selfIsectPath_lo, selfIsectPath_a;
int selfIsectPath_isecta;
void splitBez(float *a, float t) {
  float p1 = p1;
  *a = t * t - p1;
}
int checkSelfIsectCurve(float *p2) {
  if (*p2 == *p2)
    return 0;
  return 1;
}
void selfIsectPath() {
  float hi;
  while (selfIsectPath_lo) {
    float t = hi / 2;
    splitBez(&selfIsectPath_a, t);
    checkSelfIsectCurve(&selfIsectPath_a);
    if (selfIsectPath_isecta)
      hi = t;
  }
}
```

Triggering:

```
$ gcc -O3 -c absfont_path.c.c -o a.o
during GIMPLE pass: evrp
absfont_path.c.c: In function 'selfIsectPath':
absfont_path.c.c:21:1: internal compiler error: Segmentation fault
   21 | }
      | ^
0xcb7c0f crash_signal
        gcc/toplev.cc:314
0x19682c4 range_def_chain::in_chain_p(tree_node*, tree_node*)
        gcc/gimple-range-gori.cc:126
0x19682c4 gori_compute::compute_operand_range(vrange&, gimple*, vrange const&, tree_node*, fur_source&, value_relation*)
        gcc/gimple-range-gori.cc:667
0x19690d7 gori_compute::compute_operand1_range(vrange&, gimple_range_op_handler&, vrange const&, tree_node*, fur_source&, value_relation*)
        gcc/gimple-range-gori.cc:1174
0x1968165 gori_compute::compute_operand_range(vrange&, gimple*, vrange const&, tree_node*, fur_source&, value_relation*)
        gcc/gimple-range-gori.cc:726
0x19698a7 gori_compute::compute_operand2_range(vrange&, gimple_range_op_handler&, vrange const&, tree_node*, fur_source&, value_relation*)
        gcc/gimple-range-gori.cc:1254
0x1969cf4 gori_compute::compute_operand1_and_operand2_range(vrange&, gimple_range_op_handler&, vrange const&, tree_node*, fur_source&, value_relation*)
        gcc/gimple-range-gori.cc:1274
```

This is a crash in value range propagation subsystem. It's expected to
derive various properties from comparisons and arithmetics. For example
`*p2 == *p2` is probably always true as long as you can prove that `*p2`
is not a `NaN`. Unfortunately `gcc` did not consider specifics of `NaN`
in some places and managed to `SIGSEGV` itself.

Andrew explained the failure in more detail in
[this comment](https://gcc.gnu.org/PR109274#c12).

## Miscompilation of byte swapping

[tree-optimization/108064](https://gcc.gnu.org/PR108064): `gcc`
miscompiled `apache-arrow-cpp`.

Minimal reproducer:

```c
typedef short int i16;

static inline i16 ByteSwap16(i16 value) {
  constexpr auto m = static_cast<i16>(0xff);
  return static_cast<i16>(((value >> 8) & m) | ((value & m) << 8));
}

__attribute__((noipa))
void swab16(i16 * d, const i16* s) {
  for (unsigned long i = 0; i < 4; i++) {
    d[i] = ByteSwap16(s[i]);
  }
}

__attribute__((noipa))
int main(void) {
  /* need to alogn inputs to make sure vectized part
     of the loop gets executed. */
  alignas(16) i16 a[4] = {0xff, 0, 0, 0};
  alignas(16) i16 b[4];
  alignas(16) i16 c[4];

  swab16(b, a);
  swab16(c, b);

  /* Contents of 'a' should be equivalent to 'c'.
     But gcc bug generates invalid vectored shifts.  */
  if (a[0] != c[0])
    __builtin_trap();
}
```

Triggering the bug:

```
$ ./gcc-git/bin/g++ -O3 a.cc -o a && ./a
Illegal instruction (core dumped)
$ ./gcc-git/bin/g++ -O0 a.cc -o a && ./a
```

This example takes an array of 4 16-bit integers and swaps bytes in it
twice. We expect to get the same result as original. But we get
something else.

It took me a while to extract it from `apache-arrow` test suite but I'm
glad I spent a bit of time on it. Note how I had to use `alignas(16)`
hints to make sure runtime address of arrays has a nice 16-byte aligned
boundary. Otherwise bug does not happen consistently. It's a good hint
that vectorization is involved here.

If you have some familiarity in the `x86_64` assembler this snippet
shows mechanics of the bug:

```asm
; swab16(short*, short const*):
movq   (%rsi),%xmm0
movdqa %xmm0,%xmm1
psllw  $0x8,%xmm0
psraw  $0x8,%xmm1 ; <<<- should be psrlw!
por    %xmm1,%xmm0
movq   %xmm0,(%rdi)
```

If the above code does not make sense it's explanation is:

- load 16 bytes of input (more than our `u16` array) into `xmm0` from
  `rsi` address
- do `xmm0 = ((xmm0 << 8) | (xmm0 >> 8))` equivalent to achieve byte
  swap
- write 8 bytes back (exactly our `u16` array) to `rdi` address

This is yet another hint at `gcc` vectorization bug where `swab16()`
loop over `u16` values was widened to loop over `u64` values.

The problem happens in `>>` where arithmetic (sign-extending) shift is
used instead of logical (zero-extending) shift.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=33be3ee36a7e2c0be383ec01b5fbc9aef39568fd)
is trivial: use logical shift vectorization templates of this kind.

## initializer list failure

[c++/108071](https://gcc.gnu.org/PR108071): `gcc` failed to build 
`clang`.

Minimal reproducer:

```c++
#include <initializer_list>

class OptSpecifier;
struct ArrayRef {
  ArrayRef(std::initializer_list<OptSpecifier>);
};
struct OptSpecifier {
  explicit OptSpecifier(bool);
  OptSpecifier(unsigned);
};
struct ArgList {
  void AddAllArgs(ArrayRef) const;
};
enum { OPT_u };
struct Linker {
  void ConstructJob(const ArgList &) const;
};
void Linker::ConstructJob(const ArgList &Args) const {
  Args.AddAllArgs({OPT_u});
}
```

And build failure:

```
$ g++ -c bug.cc.cc
bug.cc.cc: In member function 'void Linker::ConstructJob(const ArgList&) const':
bug.cc.cc:19:18: error: call of overloaded 'OptSpecifier(const<unnamed enum>)' is ambiguous
   19 |   Args.AddAllArgs({OPT_u});
      |   ~~~~~~~~~~~~~~~^~~~~~~~~
bug.cc.cc:9:3: note: candidate: 'OptSpecifier::OptSpecifier(unsigned int)'
    9 |   OptSpecifier(unsigned);
      |   ^~~~~~~~~~~~
bug.cc.cc:8:12: note: candidate: 'OptSpecifier::OptSpecifier(bool)'
    8 |   explicit OptSpecifier(bool);
      |            ^~~~~~~~~~~~
bug.cc.cc:7:8: note: candidate: 'constexpr OptSpecifier::OptSpecifier(const OptSpecifier&)'
    7 | struct OptSpecifier {
      |        ^~~~~~~~~~~~
bug.cc.cc:7:8: note: candidate: 'constexpr OptSpecifier::OptSpecifier(OptSpecifier&&)'
```

It's a `c++` frontend bug in handling of initializer lists. I don't
pretend to understand [the fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=4ef521bbc63f8a3883d507a8b6c1f95f442df3fe).
Looks like a kind of implicit conversion handling was missing there.

## More -Wdangling-reference false positives

[c++/107488](https://gcc.gnu.org/PR107488): `cppunit` exposed a false
positive in recently added `gcc` warning.

I [saw before](/posts/264-gcc-s-new-Wdangling-reference-warning.html) a
few other examples of false positives in this area. Here is another one:

```c++
#include <vector>

int attributesAsString(std::vector<int> & v)
{
  int attributes;

  std::vector<int>::const_iterator itAttribute = v.begin();
  while ( itAttribute != v.end() )
  {
    const int &attribute = *itAttribute++;
    attributes += attribute;
  }

  return attributes;
}
```

Triggering the warning:

```
$ g++ -Werror=dangling-reference -c a.cpp.cpp -o a.o
a.cpp.cpp: In function 'int attributesAsString(std::vector<int>&)':
a.cpp.cpp:12:16: error: possibly dangling reference to a temporary [-Werror=dangling-reference]
   12 |     const int &attribute = *itAttribute++;
      |                ^~~~~~~~~
a.cpp.cpp:12:40: note: the temporary was destroyed at the end of the full expression
  'itAttribute.__gnu_cxx::__normal_iterator<const int*, std::vector<int> >::operator++(0).__gnu_cxx::__normal_iterator<const int*, std::vector<int> >::operator*()'
   12 |     const int &attribute = *itAttribute++;
      |                                        ^~
```

It's a reasonable code without a chance to leak something unexpected.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=32a06ce38a38bf37db468f0e6c83520fcc221534)
special-cased `operator*()` as not creating short-lived temporaries.

[c++/109514](https://gcc.gnu.org/PR109514) was a similar case in
`fheroes2` codebase.

## -fanalyzer crash on bind() function

[analyzer/107783](https://gcc.gnu.org/PR107783): `gnutls` triggered
`ICE` in `-fanalyzer` mode.

This time the reproducer is tiny:

```c++
int
foo (void)
{
  return bind (0, 0, 0);
}
```

```
$ gcc -fanalyzer -c oerlsfmf.c
during IPA pass: analyzer
oerlsfmf.c: In function 'foo':
oerlsfmf.c:4:10: internal compiler error: in deref_rvalue, at analyzer/region-model.cc:3238
    4 |   return bind (0, 0, 0);
      |          ^~~~~~~~~~~~~~
```

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=12a4785c9120beeef42f1bded52cc2674e206f57)
corrected type annotation for `bind()` in analyzer's model of functions
working with file descriptors.

## ICE in implicit type conversions

[c++/108047](https://gcc.gnu.org/PR108047): `arrow-cpp` triggered `gcc`
`ICE`.

Small reproducer:

```c++
#include <string>
#include <vector>
void format_underline(std::vector<std::string>);

template <typename>
void parse_key_value_pair(void) { format_underline({""}); }
```

And the crash:

```
$ g++ -c bug.cc
...
bug.cc: In function 'void parse_key_value_pair()':
bug.cc:7:51: internal compiler error:
  unexpected expression '(std::__cxx11::basic_string<char>)""' of kind implicit_conv_expr
    7 | void parse_key_value_pair(void) { format_underline({""}); }
      |                                   ~~~~~~~~~~~~~~~~^~~~~~
  diagnostic_impl(rich_location*, diagnostic_metadata const*, int, char const*, __va_list_tag (*) [1], diagnostic_t)
  internal_error(char const*, ...)
  cxx_eval_constant_expression(constexpr_ctx const*, tree_node*, value_cat, bool*, bool*, tree_node**)
```

Here `gcc` could not figure out the type of constant expression in the
frontend and crashed.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=01ea66a6c56e53163d9430f4d87615d570848aa8)
amends it. I don't understand it either. It has something to do with
initializer lists and templates :)

This was a popular failure. `nix` also `ICE`d `gcc` the same way.

## float vectors and implicit conversions

[c++/107358](https://gcc.gnu.org/PR107358): `gcc` failed to compile
`libjxl`.

Minimized example:

```c++
// this works:
float approx_scal(float e) {
    return e - 124.225514990f;
}

typedef float __attribute__((vector_size(4*sizeof(float)))) F;

// this fails:
F approx_vec(F e) {
    return e - 124.225514990f;
}
```

Did you know you can do `operator-()` against `float` vectors? I did
not. The trigger looked this way:

```
$ g++ -fPIC -std=c++11 -o skcms.cc.o -c skcms.cc
skcms.cc: In function 'F approx_vec(F)':
   10 | F approx_vec(F e) {
      |                 ^
skcms.cc:11:14: error: conversion of scalar 'long double' to vector 'F' {aka '__vector(4) float'} involves truncation
   11 |     return e - 124.225514990f;
      |            ~~^~~~~~~~~~~~~~~~
```

Even though all the arguments are of `float` type `gcc` pulled out
`double` conversion and failed.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=65e3274e363cb2c6bfe6b5e648916eb7696f7e2f)
added expected precision to the typechecker frontend.

## implicit construction on assignment

[c++/109247](https://gcc.gnu.org/PR109307): `gcc` failed to compile
`webkitgtk`.

Minimal example:

```c++
#include <optional>
#include <variant>

class RefGradient {};
class RefPattern {};
class AffineTransform {};

class SourceBrush {
  public:
    struct Brush {
        struct LogicalGradient {
            RefGradient gradient;
            AffineTransform spaceTransform;
        };

        std::variant<LogicalGradient, RefPattern> brush;
    };

    void setGradient(RefGradient &&, const AffineTransform & spaceTransform = { });
    void setPattern(RefPattern &&);

  private:
    std::optional<Brush> m_brush;
};

void SourceBrush::setGradient(RefGradient&& gradient, const AffineTransform& spaceTransform)
{
    m_brush = { Brush::LogicalGradient { std::move(gradient), spaceTransform } };
}

void SourceBrush::setPattern(RefPattern&& pattern)
{
    m_brush = { std::move(pattern) };
}
```

The trigger:

```
$ g++ SourceBrush.cpp -c -std=c++20
SourceBrush.cpp: In member function 'void SourceBrush::setGradient(RefGradient&&, const AffineTransform&)':
SourceBrush.cpp:28:80: error: converting to 'std::optional<SourceBrush::Brush>' from initializer list would use explicit constructor 'constexpr std::optional<_Tp>::optional(_Up&&) [with _Up = SourceBrush::Brush::LogicalGradient; typename std::enable_if<__and_v<std::__not_<std::is_same<std::optional<_Tp>, typename std::remove_cv<typename std::remove_reference<_Iter>::type>::type> >, std::__not_<std::is_same<std::in_place_t, typename std::remove_cv<typename std::remove_reference<_Iter>::type>::type> >, std::is_constructible<_Tp, _Up>, std::__not_<std::is_convertible<_Up, _Tp> > >, bool>::type <anonymous> = false; _Tp = SourceBrush::Brush]'
   28 |     m_brush = { Brush::LogicalGradient { std::move(gradient), spaceTransform } };
      |                                                                                ^
```

It's a long but straightforward error: `gcc` stopped inferring `Brush`
outer constructor. So far the consensus that it's not a `gcc-13` bug
but a bug in previous versions of `gcc` to accept this code.

The fix should look like:

```diff
--- a/SourceBrush.cpp
+++ b/SourceBrush.cpp
@@ -50,10, +50,10
 void SourceBrush::setGradient(RefGradient&& gradient, const AffineTransform& spaceTransform)
 {
-     m_brush =       { Brush::LogicalGradient { std::move(gradient), spaceTransform } };
+     m_brush = Brush { Brush::LogicalGradient { std::move(gradient), spaceTransform } };
 }

 void SourceBrush::setPattern(RefPattern&& pattern)
 {
-     m_brush =       { std::move(pattern) };
+     m_brush = Brush { std::move(pattern) };
 }
```

## ICE in ipa clone

[ipa/108110](https://gcc.gnu.org/PR108110): `gcc` `ICE`d on `minetest`
code.

The minimal example I came up with was:

```c++
void __throw_out_of_range_fmt(...);
char *_M_p;
struct Trans_NS___cxx11_basic_string {
  long _M_string_length;
  long _M_check___pos;
  Trans_NS___cxx11_basic_string() {
    long __length = 0;
    _M_string_length = __length;
  }
  long size() { return _M_string_length; }
  long foo___pos;
  char foo() { return _M_p[foo___pos]; }
  int compare() { __throw_out_of_range_fmt(_M_check___pos, _M_string_length); __builtin_trap(); }
};
bool str_starts_with(Trans_NS___cxx11_basic_string &str,
                     Trans_NS___cxx11_basic_string prefix) {
  if (str.size() < prefix.size())
    str.compare();
  for (; prefix.size();) {
    char __trans_tmp_2 = prefix.foo();
    if (__trans_tmp_2)
      return false;
  }
  __builtin_trap();
}
void testStartsWith() {
  Trans_NS___cxx11_basic_string s1, s2;
  str_starts_with(s1, s2);
}
```

And the trigger is:

```
$ g++ -Wall -Wextra  -O3  -c bug.cc
during IPA pass: inline
bug.cc: In function 'void testStartsWith()':
bug.cc:28:18: internal compiler error: in modify_call, at ipa-param-manipulation.cc:700
   28 |   str_starts_with(s1, s2);
      |   ~~~~~~~~~~~~~~~^~~~~~~~
  diagnostic_impl(rich_location*, diagnostic_metadata const*, int, char const*, __va_list_tag (*) [1], diagnostic_t)
  internal_error(char const*, ...)
  fancy_abort(char const*, int, char const*)
  ipa_param_adjustments::modify_call(cgraph_edge*, bool) [clone .cold]
  cgraph_edge::redirect_call_stmt_to_callee(cgraph_edge*)
  redirect_all_calls(copy_body_data*, basic_block_def*)
  copy_body(copy_body_data*, basic_block_def*, basic_block_def*, basic_block_def*) [clone .isra.0]
  expand_call_inline(basic_block_def*, gimple*, copy_body_data*, bitmap_head*)
  optimize_inline_calls(tree_node*)
  inline_transform(cgraph_node*)
  execute_all_ipa_transforms(bool)
  cgraph_node::expand()
  symbol_table::compile() [clone .part.0]
  symbol_table::finalize_compilation_unit()
```

Here `gcc` backtrace is very clear: optimization inlined the function
call and tried to redirect the calls to resulting function after the
inline is performed. But something went wrong.

[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=c389991432da2bcc335a2b4fb7e502d28a6b3346)
enhances resolution of original function arguments to survive various
transformations so the replacement would be correct in all contexts.

# Parting words

Again, most of the `gcc` bugs were fixed within a week they were
introduced. It's amazing.

While I encountered most of the bugs I reported only minority of them as
someone else already discovered them first.

Partly it was because I was updating weekly snapshots on Sunday nights
(when the snapshots are cut). While the bugs are introduced during the
week. As I don't have much time to explore complicated `gcc` bugs on
weekdays it usually takes me the time until next weekend to look into
failures. That is almost 2 weeks of lag from introduction to report.
It's not ideal for everyone: devs might have moved on to another problem
and brave users started encountering the bug in the wild.

To think of it I found handling snapshots a bit clunky to manage
short-lived backports locally until next snapshot is cut. It's the very
same reason I never published snapshots as distribution packages myself
for other users: they are always slightly stale. Using `git` branches
is a bit easier.

I started using weekly `gcc` snapshots at the time (and not just `gcc`
from `git`) only because I did not know how to bootstrap my `NixOS`
system with `gcc` that needs `git` in its dependencies. A year has
passed and I know how to do it now! Let's see if I'll be able to catch
bugs faster as a result.

If you are feeling brave and you are ready to trace and report `gcc`
bugs like the above do consider giving unreleased versions of `gcc` a
try. You might learn a thing or two in the process.

The wrong-code bugs are the subtlest. These usually live for a while
until they get noticed. They take time to get extracted and understood.
But they are most rewarding to understand and to fix!

`gcc-13` development is almost done. It will require quite a bit of
`#include <cstdint>` header sprinkling. My local system still has about
30 packages fixed pending upstream inclusion. I hope that official
`gcc-13` release will help upstream developers to adapt faster.

Looking at the bug list above the histogram of most failing subsystems
is:

- `c++`: 5
- `tree-optimization`: 2
- `analyzer`: 1
- `ipa`: 1

As I don't get exposed to exotic arches nowadays it's natural I don't
see many bugs in their backends either. Thus `c++` frontend is by far
the most frequent to cause issues. And it certainly feels that way. That
is a good indicator that `C++` as a language still evolves substantially.

Overall `gcc-13` should be a smooth sailing similar to `gcc-12` (famous
last words).

As usual here is a list of notable changes for upcoming release:
<https://gcc.gnu.org/gcc-13/changes.html>

Have fun!
