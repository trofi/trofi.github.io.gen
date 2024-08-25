---
title: "gcc-15 bugs, pile 1"
date: August 25, 2024
---

About 4 months have passed since `gcc-14.1.0` release. Around the same
time `gcc-15` development has started and a few major changes were
merged into the `master` development branch.

## summary

This time I waited to collect about 20 bug reports I encountered:

- [c++/114933](https://gcc.gnu.org/PR114933): `mcfgthread-1.6.1`
  typecheck failure. Ended up being `mcfgthread` bug caused by stronger
  `gcc` checks.
- [tree-optimization/114872](https://gcc.gnu.org/PR114872): `sagemath`
  `SIGSEGV`ed due to broken assumptions around `setjmp()` / `longjmp()`.
  Not a `gcc `bug either.
- [target/115115](https://gcc.gnu.org/PR115115): `highway-1.0.7` test
  suite expected too specific `_mm_cvttps_epi32()` semantics. A `gcc-12`
  regression!
- [target/115146](https://gcc.gnu.org/PR115146): `highway-1.0.7` test
  suite exposed `gcc-15` bug in vectoring `bswap16()`-like code.
- [tree-optimization/115227](https://gcc.gnu.org/PR115227): `libepoxy`,
  `p11-kit` and `doxygen` can't fit in RAM of 32-bit `gcc` due to memory
  leak in value range propagation subsystem.
- [target/115397](https://gcc.gnu.org/PR115397): `numpy` ICE for `-m32`:
  `gcc` code generator generated a constant pool memory reference and
  crashed in instruction selection.
- [c++/115403](https://gcc.gnu.org/PR115403): `highway` build failure
  due to wrong scope handling of `#pragma GCC target` by `gcc`.
- [tree-optimization/115602](https://gcc.gnu.org/PR115602):
  `liblapack-3.12.0` ICE in `slp` pass. `gcc` generated a self-reference
  cycle after applying code subexpression elimination.
- [bootstrap/115655](https://gcc.gnu.org/PR115655): `gcc` bootstrap
  failure on `-Werror=unused-function`.
- [libstdc++/115797](https://gcc.gnu.org/PR115797): `gcc` failed to
  compile `extern "C" { #include <math.h> }` code. `<math.h>` was fixed
  to survive such imports.
- [middle-end/115863](https://gcc.gnu.org/PR115863): wrong code on
  `zlib` when handling saturated logic. A bug in truncation handling.
- [rtl-optimization/115916](https://gcc.gnu.org/PR115916): wrong code on
  `highway`. Bad arithmetic shift `ubsan`-related fix in `gcc`'s own code.
- [middle-end/115961](https://gcc.gnu.org/PR115961): wrong code on `llvm`,
  bad bitfield truncation handling for sub-byte bitfield sizes. Saturated
  truncation arithmetics handling was applied too broadly.
- [tree-optimization/115991](https://gcc.gnu.org/PR115991): ICE on
  `linux-6.10`. Caused by too broad acceptance of sub-register use in an
  instruction. ENded up selecting invalid instructions.
- [rtl-optimization/116037](https://gcc.gnu.org/PR116037): `python3`
  hangup due to an `-fext-dce` bug.
- [rtl-optimization/116200](https://gcc.gnu.org/PR116200): crash during
  `gcc` bootstrap, wrong code on `libgcrypt`. A bug in RTL constant pool
  handling.
- [rtl-optimization/116353](https://gcc.gnu.org/PR116353): ICE on
  `glibc-2.39`. Another RTL bug where `gcc` instruction selector was
  presented with invalid value reference.
- [middle-end/116411](https://gcc.gnu.org/PR116411): ICE on
  `readline-8.2p13`. Conditional operation was incorrectly optimized for
  some of builtin functions used in branches.
- [tree-optimization/116412](https://gcc.gnu.org/PR116412): ICE on
  `openblas-0.3.28`. Similar to the above: conditional operation was
  incorrectly optimized for complex types.

## fun bug

The [`zlib` bug](https://gcc.gnu.org/PR115863) is probably the most
unusual one. Due to a typo in newly introduced set of optimizations
`gcc` managed to convert ` a > b ? b : a` type of expressions into an
equivalent of ` b > a ? b : a`. But it only does it for `b = INT_MAX`
type of arguments (case of saturation).

As a result it only broke `zlib` test suite as it specifically tests for
out of range access to cause `SIGSEGV`s. For well-behaved inputs it never
caused any problems. The `gcc` fix
[was trivial](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=aae535f3a870659d1f002f82bd585de0bcec7905):

```diff
--- a/gcc/config/i386/i386.md
+++ b/gcc/config/i386/i386.md
@@ -9990,7 +9990,7 @@
   rtx sat = force_reg (DImode, GEN_INT (GET_MODE_MASK (<MODE>mode)));
   rtx dst;

-  emit_insn (gen_cmpdi_1 (op1, sat));
+  emit_insn (gen_cmpdi_1 (sat, op1));

   if (TARGET_CMOVE)
     {
@@ -10026,7 +10026,7 @@
   rtx sat = force_reg (SImode, GEN_INT (GET_MODE_MASK (<MODE>mode)));
   rtx dst;

-  emit_insn (gen_cmpsi_1 (op1, sat));
+  emit_insn (gen_cmpsi_1 (sat, op1));

   if (TARGET_CMOVE)
     {
@@ -10062,7 +10062,7 @@
   rtx sat = force_reg (HImode, GEN_INT (GET_MODE_MASK (QImode)));
   rtx dst;

-  emit_insn (gen_cmphi_1 (op1, sat));
+  emit_insn (gen_cmphi_1 (sat, op1));

   if (TARGET_CMOVE)
     {
```

We swap argument order to restore original intent.

## histograms

Where did most `gcc` bugs come from?

- `tree-optimization`: 4
- `rtl-optimization`: 4
- `middle-end`: 3
- `target`: 3
- `c++`: 1
- `bootstrap`: 1
- `libstdc++`: 1

As usual `tree-optimization` is at the top of subsystem causing troubles.
But this time `rtl-optimization` got close to it as well.

`highway` managed to yield us 4 new bugs while `llvm` got us just one
new bug.

## parting words

`gcc-15` got a few very nice optimizations (and bugs) related to
saturated truncation, zero/sign-extension elimination, constant folding
in RTL.

I saw at least 5 bugs related to wrong code generation (and
also slowly reducing another one in the background). `middl-end` ones
were easy to reduce and explore, `RTL` ones were very elusive.

The most disruptive change is probably a removal of `#include <cstdint>`
from one of `libstdc++` headers. That requires quite a few upstream
fixes to add missing headers ([cppdap](https://github.com/google/cppdap/pull/133),
[woff2](https://github.com/google/woff2/pull/176),
[graphite](https://github.com/silnrsi/graphite/pull/91),
[glslang](https://github.com/KhronosGroup/glslang/pull/3684),
[widelands](https://github.com/widelands/widelands/pull/6522),
[wesnoth](https://github.com/wesnoth/wesnoth/pull/9250) and many others).

Have fun!
