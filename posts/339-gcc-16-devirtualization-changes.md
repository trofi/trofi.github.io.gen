---
title: "gcc-16 devirtualization changes"
date: September 27, 2025
---

## A quiz

Let's start from a quiz: is this single file program a valid `c++`
program? Will it always build and run?

```c++
void d_impl(void); /* has no definition at all! */

struct B { virtual void f(void) {} };

struct D : public B { virtual void f(void) { d_impl(); } };

void do_f(struct B* o) { o->f(); }

int main(void) { return 0; }
```

## Running `gcc`

It feels like this whole program is just an obfuscated version of
`int main(){}` and thus should Just Work, right? And `gcc-15` would agree:

```
$ g++-15 a.cc -o a -fopt-info -O2
$ ./a
```

But with `gcc-16` it fails to link:

```
$ g++-16 a.cc -o a -fopt-info -O2
a.cc:11:30: optimized: speculatively devirtualizing call in void do_f(B*)/3 to virtual void B::f()/1
a.cc:11:30: optimized: speculatively devirtualizing call in void do_f(B*)/3 to virtual void D::f()/2
a.cc:11:30: optimized: devirtualized call in void do_f(B*)/3 to 2 targets
a.cc:11:30: optimized:  Inlined virtual void B::f()/10 into void do_f(B*)/3 which now has time 12.400000 and size 11, net change of -2.
a.cc:11:30: optimized:  Inlined virtual void D::f()/11 into void do_f(B*)/3 which now has time 12.160000 and size 10, net change of -1.

ld: /tmp/nix-shell.QW52Fh/ccNR2yWI.o: in function `do_f(B*)':
a.cc:(.text+0x29): undefined reference to `d_impl()'
ld: /tmp/nix-shell.QW52Fh/ccNR2yWI.o: in function `D::f()':
a.cc:(.text._ZN1D1fEv[_ZN1D1fEv]+0x1): undefined reference to `d_impl()'
collect2: error: ld returned 1 exit status
```

Note: it fails to find an implementation of `void d_impl(void);` function.

## devirtualization mechanics

Why did `gcc` not notice missing reference before?
`-fopt-info` gives us a hint that `gcc` "devirtualized" virtual call
of `void do_f(struct B* o) { o->f(); }` into non-virtual calls and got
extra references into the code.
`-fdump-tree-all` can show us the result after these transformations:

```
$ g++ a.cc -o a -fopt-info -O2 -fdump-tree-all
...

# I removed a bit of unrelated detail manually
$ cat a.cc.273t.optimized

void B::f (struct B * const this) { return; }

void D::f (struct D * const this) { d_impl (); }

void do_f (struct B * o)
{
  int (*) () * _1;
  int (*) () _2;
  void * PROF_6;
  void * PROF_8;

  _1 = o_4(D)->_vptr.B;
  _2 = *_1;
  PROF_6 = [obj_type_ref] OBJ_TYPE_REF(_2;(struct B)o_4(D)->0B);
  if (PROF_6 == D::f) {
    d_impl (); [tail call]
    return;
  }

  PROF_8 = [obj_type_ref] OBJ_TYPE_REF(_2;(struct B)o_4(D)->0B);
  if (PROF_8 == B::f)
    return;

  OBJ_TYPE_REF(_2;(struct B)o_4(D)->0B) (o_4(D)); [tail call]
}

int main () { return 0; }
```

Here `gcc-16` expanded `void do_f(struct B* o) { o->f(); }` against
unknown `o->f()` call into a few known types: `o->B::f()` and
`o->D::f()` calls by checking the function addresses via vtable. This
allowed `gcc` to inline `B::f()` and `D::f()`. Pseudocode of the result:

```c++
// before
void do_f(struct B* o) { o->f(); }

// after
void do_f(struct B* o) {

  if (o->f == D::f) {
    // inlined D::f()
    d_impl(); // our new reference!
    return;
  }

  if (o->f == B::f) {
    // inlined B::f()
    return;
  }

  // other types
  o->f();
}
```

`gcc-15` did not use to do this kind of transformations. It's a recent
change added in the [commit `Add --param max-devirt-targets`](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=9ee937b2f92a930eb5407260a56e5fe0fa137e85).
This extended existing devirtualization optimization to consider not
just one possible devirtualization target (before the patch), but at most
`3`.

## Fixes and workarounds

Now we can even work the original example around and build it:

```
$ g++ a.cc -o a -fopt-info -O2 --param=max-devirt-targets=1
$ ./a
```

Is the above a completely hypothetical scenario? Why would you have such
code lying around? Well, I initially noticed it on
[`cmake` build failure](https://gitlab.kitware.com/cmake/cmake/-/issues/27256).
There `./bootstrap` script failed to build initial `cmake` on
`gcc-16`. `./bootstrap` code uses only a subset of `cmake` source code,
but it had a few `#include` that do refer to the code that does not
get compiled/linked in `./bootstrap`. The devirtualization change exposed
it. The [fix](https://gitlab.kitware.com/cmake/cmake/-/merge_requests/11243/diffs?commit_id=ea04e19daf7010781d0df980b9683a642093e381)
was to `#ifdef` out the code that has no chance to execute on `./bootstrap`.
To transfer it back to our example the fix is similar to the following:

```c++
void d_impl(void); /* has no definition at all! */

struct B { virtual void f(void) {} };

#if 0
struct D : public B { virtual void f(void) { d_impl(); } };
#endif

void do_f(struct B* o) { o->f(); }

int main(void) { return 0; }
```

```
$ g++ a.cc -o a -fopt-info -O2
a.cc:9:30: optimized: speculatively devirtualizing call in void do_f(B*)/2 to virtual void B::f()/1
a.cc:9:30: optimized:  Inlined virtual void B::f()/6 into void do_f(B*)/2 which now has time 7.200000 and size 9, net change of -2.

$ ./a
```

All good now!

## Parting words

The initial example was not quite correct and caused link failures when
devirtualization kicked in. Including headers to the unlinked code does
not always work.

Devirtualization does sometimes bloat the code a bit with references that
have no chance to execute in real programs. Profile-guided optimizations
help a lot to avoid generation of completely dead code by getting better
estimates of observed behavior.

`cmake` is [fixed](https://gitlab.kitware.com/cmake/cmake/-/merge_requests/11243/diffs?commit_id=ea04e19daf7010781d0df980b9683a642093e381)
and can now be built with `gcc-16`!

Have fun!
