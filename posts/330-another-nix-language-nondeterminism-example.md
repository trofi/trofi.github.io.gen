---
title: "Another Nix Expression Language non-determinism example"
date: December 26, 2024
---

Today I found another source of non-determinism in `nix expression language`.
This time it's a
[`builtins.sort`](https://github.com/NixOS/nix/issues/12106) primitive!

How do you break `sort`?

Compared to the
[previous non-determinism instance](/posts/292-nix-language-nondeterminism-example.html)
this case of non-determinism breaking `sort` is not as arcane.

## a working `sort` example

Before triggering the problematic condition let's look at a working sort:

```
$ nix repl
nix-repl> builtins.sort builtins.lessThan [ 4 3 2 1 ]
[
  1
  2
  3
  4
]

nix-repl> builtins.sort (a: b: a < b) [ 4 3 2 1 ]
[
  1
  2
  3
  4
]
```

All nice and good: we pass the comparison predicate and get some result
back. In the first case we are passing a builtin comparator. In the
second case we write a lambda that implements `<`. Nothing fancy.

Normally `sort` function expects a bunch of properties from the passed
predicate, like
["strict weak ordering"](https://en.wikipedia.org/wiki/Weak_ordering#Strict_weak_orderings)
to return something that looks sorted.

## suspicious `sort` call

But what happens if we pass a predicate that does not satisfy that
property? On vanilla `nix` that would be:

```
nix-repl> builtins.sort (a: b: true) [ 4 3 1 2 ]
[ 2 1 3 4 ]
```

The result is not sensible, but at least it did not crash. All good?

**Quiz question:** Is this returned order guaranteed to be the same across `nix` implementations
on different platforms?

## triggering the non-determinism

Today I tried to build `nix` package with `gcc` `STL`
[debugging enabled](https://gcc.gnu.org/onlinedocs/libstdc++/manual/debug_mode_using.html#debug_mode.using.mode). In theory it's simple: you pass `-D_GLIBCXX_DEBUG` via
`CXXFLAGS` and you get your debugging for free.

I was chasing an unrelated `nix` memory corruption bug and did just that.
I hoped for a simple case like the past [PR#8825](https://github.com/NixOS/nix/pull/8825).

To my surprise `nixpkgs` evaluation started triggering `libstdc++`
assertions. For the above "suspicious sort" example the execution was:

```
$ nix eval --expr 'builtins.sort (a: b: true) [ 4 3 2 1 ]

/nix/store/L89IQC7AM6I60Y8VK507ZWRZXF0WCD3V-gcc-14-20241116/include/c++/14-20241116/bits/stl_algo.h:5027:
In function:
    void std::stable_sort(_RAIter, _RAIter, _Compare) [with _RAIter =
    nix::Value**; _Compare = nix::prim_sort(EvalState&, PosIdx, Value**,
    Value&)::<lambda(nix::Value*, nix::Value*)>]

Error: comparison doesn't meet irreflexive requirements, assert(!(a < a)).

Objects involved in the operation:
    instance "functor" @ 0x7ffd7d2fdb00 {
      type = nix::prim_sort(nix::EvalState&, nix::PosIdx, nix::Value**, nix::Value&)::{lambda(nix::Value*, nix::Value*)#1};
    }
    iterator::value_type "ordered type"  {
      type = nix::Value*;
    }
Aborted (core dumped)
```

Uh-oh. A crash where there was none before. Note how `libstdc++` tells us
that our comparator is not expected to return `true` for `a < a`.

## `builtins.sort` implementation

Looking at the `nix` implementation around the crash it reveals that
`nix` uses `std::stable_sort` to implement `builtins.sort`
([link](https://github.com/NixOS/nix/blob/bff9296ab997269d703c5222b7e17d67a107aeed/src/libexpr/primops.cc#L3642)) with no predicate validation:

```c++
static void prim_sort(EvalState & state, const PosIdx pos, Value * * args, Value & v)
{
    state.forceList(*args[1], pos, "while evaluating the second argument passed to builtins.sort");

    auto len = args[1]->listSize();
    if (len == 0) {
        v = *args[1];
        return;
    }

    state.forceFunction(*args[0], pos, "while evaluating the first argument passed to builtins.sort");

    auto list = state.buildList(len);
    for (const auto & [n, v] : enumerate(list))
        state.forceValue(*(v = args[1]->listElems()[n]), pos);

    auto comparator = [&](Value * a, Value * b) {
        /* Optimization: if the comparator is lessThan, bypass
           callFunction. */
        if (args[0]->isPrimOp()) {
            auto ptr = args[0]->primOp()->fun.target<decltype(&prim_lessThan)>();
            if (ptr && *ptr == prim_lessThan)
                return CompareValues(state, noPos, "while evaluating the ordering function passed to builtins.sort")(a, b);
        }

        Value * vs[] = {a, b};
        Value vBool;
        state.callFunction(*args[0], vs, vBool, noPos);
        return state.forceBool(vBool, pos, "while evaluating the return value of the sorting function passed to builtins.sort");
    };

    /* FIXME: std::sort can segfault if the comparator is not a strict
       weak ordering. What to do? std::stable_sort() seems more
       resilient, but no guarantees... */
    std::stable_sort(list.begin(), list.end(), comparator);

    v.mkList(list);
}
```

Here `comparator()` calls user-supplied function written in
`nix expression language` directly (if we ignore a performance special
case) into `std::stable_sort()`. The comment suggests that `std::sort()`
was already crashing here.

This means that today `builtins.sort` semantics are following `c++`'s
`std::stable_sort()` along with it's undefined behaviours and
instability for non-conformant `comparator()` predicate.

## tracking down bad predicates

`nixpkgs` is a vast code base. It's quite hard to figure out which part
of `nix expression language` code triggers this condition from a `C++`
stack trace. I added the following hack into local `nix` to convert
those violations into nix-level exceptions:

```diff
--- a/src/libexpr/primops.cc
+++ b/src/libexpr/primops.cc
@@ -3633,6 +3633,24 @@ static void prim_sort(EvalState & state, const PosIdx pos, Value * * args, Value
                 return CompareValues(state, noPos, "while evaluating the ordering function passed to builtins.sort")(a, b);
         }

+        /* Validate basic ordering requirements for comparator: */
+        {
+            Value * vs[] = {a, a};
+            Value vBool;
+            state.callFunction(*args[0], vs, vBool, noPos);
+            bool br = state.forceBool(vBool, pos, "while evaluating the return value of the sorting function passed to builtins.sort");
+            if (br)
+                state.error<EvalError>("!(a < a) assert failed").atPos(pos).debugThrow();
+        }
+        {
+            Value * vs[] = {b, b};
+            Value vBool;
+            state.callFunction(*args[0], vs, vBool, noPos);
+            bool br = state.forceBool(vBool, pos, "while evaluating the return value of the sorting function passed to builtins.sort");
+            if (br)
+                state.error<EvalError>("!(b < b) assert failed").atPos(pos).debugThrow();
+        }
+
         Value * vs[] = {a, b};
         Value vBool;
         state.callFunction(*args[0], vs, vBool, noPos);
```

Here before calling the `compare(a,b)` against two different list
elements we are making sure that `compare(a,a)` and `compare(b,b)` does
not return `true`.

And now the error is a bit less intimidating:

```
nix-repl> builtins.sort (a: b: true) [ 4 3 2 1 ]
error:
       … while calling the 'sort' builtin
         at «string»:1:1:
            1| builtins.sort (a: b: true) [ 4 3 2 1 ]
             | ^

       error: !(a < a) assert failed
```

On a `nixpkgs` input the evaluation now fails as:

```
$ nix-instantiate -A colmapWithCuda --show-trace
error:
       … while calling a functor (an attribute set with a '__functor' attribute)
         at pkgs/top-level/all-packages.nix:5843:20:
         5842|   colmap = libsForQt5.callPackage ../applications/science/misc/colmap { inherit (config) cudaSupport; };
         5843|   colmapWithCuda = colmap.override { cudaSupport = true; };
             |                    ^
         5844|
...
         at pkgs/development/cuda-modules/generic-builders/multiplex.nix:88:17:
           87|   # perSystemReleases :: List Package
           88|   allReleases = lib.pipe releaseSets [
             |                 ^
           89|     (lib.attrValues)
```

This points us at
[`cuda-modules/generic-builders/multiplex.nix`](https://github.com/NixOS/nixpkgs/blob/1557114798a3951db0794379f26b68a5fdf68b12/pkgs/development/cuda-modules/generic-builders/multiplex.nix#L83):

```nix
  preferable =
    p1: p2: (isSupported p2 -> isSupported p1) && (strings.versionAtLeast p1.version p2.version);

  # ...

  newest = builtins.head (builtins.sort preferable allReleases);
```

Can you quickly say if `preferable` satisfies `lessThan` requirements?

`left >= right` is generally problematic for sorts:

```
nix-repl> builtins.sort (a: b: a >= b) [ 4 3 3 1 ]
error:
       … while calling the 'sort' builtin
         at «string»:1:1:
            1| builtins.sort (a: b: a >= b) [ 4 3 3 1 ]
             | ^

       error: !(a < a) assert failed
```

To make the comparator stricter it should contain strict inequality,
like `b < a` or `!(a >= b)`:

```
nix-repl> builtins.sort (a: b: b < a) [ 4 3 3 1 ]
[
  4
  3
  3
  1
]

nix-repl> builtins.sort (a: b: !(b >= a)) [ 4 3 3 1 ]
[
  4
  3
  3
  1
]
```

I proposed a seemingly trivial change as [PR#368366](https://github.com/NixOS/nixpkgs/pull/368366):

```diff
--- a/pkgs/development/cuda-modules/generic-builders/multiplex.nix
+++ b/pkgs/development/cuda-modules/generic-builders/multiplex.nix
@@ -81,7 +81,7 @@ let
   redistArch = flags.getRedistArch hostPlatform.system;

   preferable =
-    p1: p2: (isSupported p2 -> isSupported p1) && (strings.versionAtLeast p1.version p2.version);
+    p1: p2: (isSupported p2 -> isSupported p1) && (strings.versionOlder p2.version p1.version);

   # All the supported packages we can build for our platform.
   # perSystemReleases :: List Package

```

I'm not sure it's correct.

`cuda-modules` is not the only `sort` `lessThan` property violation. Next
failure is a `stan` package:

```
$ nix build --no-link -f. cmdstan --show-trace
...
       … while calling the 'sort' builtin
         at pkgs/build-support/coq/meta-fetch/default.nix:115:55:
          114|     if (isString x && match "^/.*" x == null) then
          115|       findFirst (v: versions.majorMinor v == x) null (sort versionAtLeast (attrNames release))
             |                                                       ^
          116|     else

       error: !(a < a) assert failed
```

Here you can already see that the pattern is suspiciously similar:
`sort versionAtLeast` probably does not do what it's expected to do.

Proposed a similar fix as [PR#368429](https://github.com/NixOS/nixpkgs/pull/368429).

Other packages affected:

- `mathematica`: [PR#368433](https://github.com/NixOS/nixpkgs/pull/368433)

More stuff to fix!

## Parting words

`nix expression language` used in `nix` package manager is of
minimalistic kind: it does not have much syntax sugar
hoping to reach high performance and predictability of the evaluation.
And it it manages to surprise me time and time again where I have to
debug both `nix expression language` and it's underlying `c++`
implementation.

Sorting is tricky if you allow a user-supplied sorting predicate.

`nixpkgs` has a few more sorting predicate violations that needs to be
fixed. I found at least [`cuda`](https://github.com/NixOS/nixpkgs/pull/368366),
[`coq`](https://github.com/NixOS/nixpkgs/pull/368429) and
[`mathematica`](https://github.com/NixOS/nixpkgs/pull/368433).

Examples found after the first version of the post was published:
- [`coqPackages_8_20` used `sort (<=)`](https://github.com/NixOS/nixpkgs/pull/418946)

Have fun!
