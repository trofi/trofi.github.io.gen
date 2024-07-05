---
title: "gcc-14 bugs, pile 1"
date: July 12, 2023
root: "http://trofi.github.io"
---

Around beginning og the May `gcc-14` development got opened for major
changes to be merged into `master` branch. 2 months have passed since
and I collected a "round" number of bugs: 16. Today is also a day when I
managed to build my whole system by `gcc-master` without encountering
any obvious bugs.

## Summary

Here is the full list of bugs I encountered in chronological order
:

- [ipa/109643](https://gcc.gnu.org/PR109643): `ICE` on `pkg-config`
- [ipa/109652](https://gcc.gnu.org/PR109652): `ICE` on `valgrind`
- [tree-optimization/109732](https://gcc.gnu.org/PR109732): wrong code
  on `json` testsuite.
- [c++/109755](https://gcc.gnu.org/PR109755): imprecise warning location.
- [tree-optimization/110067](https://gcc.gnu.org/PR110067): wrong code
  on `pixman` test suite.
- [middle-end/110228](https://gcc.gnu.org/PR110228): wrong code on
  `llvm` JIT test suite.
- [tree-optimization/110271](https://gcc.gnu.org/PR110271): `ICE` on
  `pycryptodome`
- [target/110274](https://gcc.gnu.org/PR110274): wrong code on `highway`
  test suite.
- [debug/110308](https://gcc.gnu.org/PR110308): `ICE` on `audiofile`.
- [tree-optimization/110332](https://gcc.gnu.org/PR110332): `ICE` on
  `llvm`.
- [middle-end/110443](https://gcc.gnu.org/PR110443): `ICE` on `a52dec`.
- [middle-end/110515](https://gcc.gnu.org/PR110515): wrong code on
  `llvm` `DWARF` test suite.
- [c++/110523](https://gcc.gnu.org/PR110523): `ICE` on `json`.
- [c++/110580](https://gcc.gnu.org/PR110580): rejects valid on `nix`
  code.
- [c++/110598](https://gcc.gnu.org/PR110598): wrong code for `llvm`
  `AMDGCN` test suite.
- [tree-optimization/110601](https://gcc.gnu.org/PR110601): `ICE` on
  `systemd`.

Let's look at a few histograms.

Looking at the manifestation of the bug:

- `ICE`s: 8
- `wrong-code`: 6
- `other`: 2

Wrong code is almost as much as compiler crashes. Some of wrong codes
took me a while to extract from the real project. Once again robust test
suites make it so much easier to extract an example for upstream
reporting.

Looking at the subsystems:

- `tree-optimization`: 5
- `c++`: 4
- `middle-end`: 3
- `ipa`: 2
- `debug`: 1
- `target`: 1

Half the bugs happened in generic optimization phase (`tree-optimization`
and `middle-end`). A few bugs in `c++` frontend (mainly in tempalte
instantiation code). And only one failure was target-specific. It should
be obvious by now that I did no test `gcc` on anything besides `x86_64`
:).

## Tip of the day

When chasing wrong code generation down to a particular source file I
found the `#pragma GCC optimize(1)` injection hack to be useful:

```
# flip first halg to -O1
$ git grep -LF 'pragma GCC optimize' -- '**.cpp' | head -n $NUMBER |
    xargs sed '1i #pragma GCC optimize(1)' -i

# flip second half to -O1
$ git grep -LF 'pragma GCC optimize' -- '**.cpp' | tail -n +$(($NUMBER+1)) |
    xargs sed '1i #pragma GCC optimize(1)' -i
```

That way I flip `$NUMBER` files down to `-O1` optimization level
(assuming it is enough to inhibit the bug from happening) and bisect it
down to a single file without `-O1` injected.

`ccache` makes pragma addition and removal very cheap. You effectively
need to compile project only twice: with default options and with `-O1`
pragma override. The rest is a bit of linking.

## Parting words

Running `gcc-master` is still a lot of fun. I keep finding ~2 new bugs
per week on a regular basis. I did not get to fix any of them: in almost
all cases maintainers figured out the fix way before I got closer to the
culprit code in `gcc`. Maybe one day I'll get there first :)

`llvm` and `json` remain to be the hardest stress tests for `gcc`.

Have fun!
