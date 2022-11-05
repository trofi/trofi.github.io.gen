---
title: "GCC's new -Wdangling-reference warning"
date: November 05, 2022
---

Tl;DR: `gcc` [just got]()
new `-Wdangling-reference` diagnostic to catch some use-after-free bugs
related to temporary object lifetimes in `c++` code.

Initially I encountered `-Wdangling-reference` at
<https://gcc.gnu.org/PR107488> where `cppunit-1.15.1` failed to build
due to `-Wall -Werror` flags in the build system. That particular case
was a false positive (`gcc` bug). I also found similar build failures in
`libcamera`, `cvise`, `spirv-tools`, `aws-sdk-cpp`.

Once false positive was fixed build was restored on `cppunit` and
`spirv-tools`. `libcamera` and `cvise` still fail and need more triage.

At least on `aws-sdk-cpp-1.9.294` the bug is real
([fixed since](https://github.com/aws/aws-sdk-cpp/commit/e1aceaeb211901d1985663d3de12c76913e41b90)).

Smaller `aws-sdk-cpp` example:

```c++
// cat a.cpp
class C {
    int i_;
  public:
    C();
    int & iRef(void); // returns i_ according to the legend
};

void g(int i);

void f(void) {
    const int & i = C().iRef();
    g(i);
}
```

Building:

```
$ g++-13.0.0 -c a.cpp -Wall -Werror
a.cpp: In function 'void f()':
a.cpp:11:17: error: possibly dangling reference to a temporary [-Werror=dangling-reference]
   11 |     const int & i = C().iRef();
      |                 ^
a.cpp:11:29: note: the temporary was destroyed at the end of the full expression 'C().C::iRef()'
   11 |     const int & i = C().iRef();
      |                     ~~~~~~~~^~
cc1plus: all warnings being treated as errors
```

Normally temporary objects don't outlive statement they are created (or
even expression in older c++ standards). The special case is a
[lifetime extension](https://en.cppreference.com/w/cpp/language/lifetime)
by taking a const lvalue reference (and a few other types of
references).

In case of our example reference returned by `iRef()` could be a
reference to part of `C()` object (and according to the legened it is).
This means that `g(i)` dereferences already destroyed object.

Unfortunately even this example is still prone to false positives: if
`iRef()` happened to return a reference to some global variable that
outlived `C()`. We dont see an `iRef()` definition here at all. Thus it
could have any lifetime. I don't think `gcc` should warn for such cases.

`libcamera` is one of such cases: <https://gcc.gnu.org/PR107532>

```c
struct Plane { unsigned int bytesused; };

// Passes a reference through. Does not change lifetime.
template <typename Inner>
struct Ref {
    const Inner & i_;
    Ref(const Inner & i) : i_(i) {}
    const Inner & inner() { return i_; }
};

struct FrameMetadata {
    Ref<const Plane> planes() const { return p_; }

    Plane p_;
};

void bar(const Plane & meta);
void foo(const FrameMetadata & fm)
{
    const Plane & meta = fm.planes().inner();
    bar(meta);
}
```

`gcc-13` complains about it as:

```
$ g++-13.0.0 -c -Wall -Werror=dangling-reference a.cpp
a.cpp: In function 'void foo(const FrameMetadata&)':
a.cpp:20:19: error: possibly dangling reference to a temporary [-Werror=dangling-reference]
   20 |     const Plane & meta = fm.planes().inner();
      |                   ^~~~
a.cpp:20:43: note: the temporary was destroyed at the end of the full expression '(& fm)->FrameMetadata::planes().Ref<const Plane>::inner()'
   20 |     const Plane & meta = fm.planes().inner();
      |                          ~~~~~~~~~~~~~~~~~^~
cc1plus: some warnings being treated as errors

This gcc version is this week's gcc-13 snapshot with https://gcc.gnu.org/PR107488 applied on top.
```

The idiom here is to wrap a non-owning reference into a tiny value-like
object. `gcc` does not see it and thinks that wrapper's lifetime matters
here.

## Parting words

New `-Wdangling-reference` option in `gcc` is exciting! It has a chance
to catch really nasty use-after-free cases. But it also seems to need
quite a bit more tuning to dial down false positives.

`-Werror` is a good way to make your program fail to build for no good
reason (i.e. a benign compiler bug). But it's also a great tool for
software developers to find bugs in code being modified (or in the
compiler :).

Have fun!
