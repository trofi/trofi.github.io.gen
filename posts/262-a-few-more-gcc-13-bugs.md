---
title: "A few more gcc-13 bugs"
date: October 21, 2022
---

Another 6 months have passed since I wrote anything about `gcc` bugs.
`gcc-13` should release around April next year (in about 6 months).
Being in the middle I'm dumping another set of bugs bugs I encountered:

- <https://gcc.gnu.org/PR106551>: `-fanalyzer` `ICE` on `libpipeline-1.5.4` code
- <https://gcc.gnu.org/PR105650>: `fontforge` wrong code in vectorizer
- <https://gcc.gnu.org/PR106142>: `pcre` wrong code in range analyzer in middle end
- <https://gcc.gnu.org/PR105492>: `proxmark3` `ICE` in `C` `typedef` handling of vectored types
- <https://gcc.gnu.org/PR105587>: `av1` `ICE` in `gcc` vectorizer
- <https://gcc.gnu.org/PR105956>: `boost` `ICE` in `C++` template expansion
- <https://gcc.gnu.org/PR106616>: `linux-4.19` `ICE` in `ia64` backend
- <https://gcc.gnu.org/PR107196>: `llvm` test suite hangup in range analyser
- <https://gcc.gnu.org/PR106334>: `LTO` `ICE` on `nix` when merging debug info
- <https://gcc.gnu.org/PR106540>: `LTO` `ICE` on `nix` when merging debug info (again)
- <https://gcc.gnu.org/PR106831>: decimal code wrong code on `mpfr` code
- <https://gcc.gnu.org/PR105852>: ICE in template instantiation on `nodejs` code
- <https://gcc.gnu.org/PR105608>: pre-compiled headers `ICE` in large macros
- <https://gcc.gnu.org/PR106905>: vectorizer ICE on `zstd` code

I won't focus on any of the bugs in detail here as they are quite boring
this time.

As usual `ICE` are simple to minimize and reduce. Runtime failures are
hardest to deal with. Especially tough one was the
[`llvm-14` hangup](https://gcc.gnu.org/PR107196). It took me a few attempts
at minimizing the example. I failed all of them. Luckily someone found a
smaller unrelated reproducer and the bug was fixed.

Bug pace makes it about 2 new bugs a week. Still barely manageable :)

As usual here is a set of upcoming changes in `gcc-13`:
<https://gcc.gnu.org/gcc-13/changes.html>. A few more `cstdint` headers
would have to be added to various upstreams.

Have fun!
