---
title: "gcc-15 template checking improvements"
date: July 22, 2024
---

## Tl;DR

On 18 Jul `gcc`
[merged](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=313afcfdabeab3e6705ac0bd1273627075be0023)
extended correctness checks for template functions. This will cause some
incorrect unused code to fail to compile. Consider fixing or deleting
the code. I saw at least two projects affected by it:

- [`aspell`](https://github.com/GNUAspell/aspell/pull/650)
- [`mjpegtools`](https://sourceforge.net/p/mjpeg/patches/63/)

## more words

`c++` is a complex language with a type system that does static checking.
Most of the time checking the type correctness is easy by both human and
the compiler. But sometimes it's less trivial. Namespaces and function
arguments can bring various declarations into the scope. Template code
splits a single definition point into two: template definition point
and template instantiation.

Let's look at a simple example:

```c++
template <typename T> struct S {
    int foo(void) { return bar(); }
};

int bar() { return 42; }

int main() {
    S<int> v;
    return v.foo();
}
```

This fails to build on all recent `gcc` as:

```
$ g++ -c a.cc
a.cc: In member function 'int S<T>::foo()':
a.cc:2:28: error: there are no arguments to 'bar' that depend on a
  template parameter, so a declaration of 'bar' must be available [-fpermissive]
    2 |     int foo(void) { return bar(); }
      |                            ^~~
a.cc:2:28: note: (if you use '-fpermissive', G++ will accept your code,
  but allowing the use of an undeclared name is deprecated)
```

`gcc` really wants `bar` to be visible at the template instantiation
time. But what is we don't call `foo` at all?

```c++
template <typename T> struct S {
    int foo(void) { return bar(); }
};

int main() {}
```

Still fails the same:

```
$ g++ -c a.cc
a.cc: In member function 'int S<T>::foo()':
a.cc:2:28: error: there are no arguments to 'bar' that depend on a
  template parameter, so a declaration of 'bar' must be available [-fpermissive]
    2 |     int foo(void) { return bar(); }
      |                            ^~~
a.cc:2:28: note: (if you use '-fpermissive', G++ will accept your code,
  but allowing the use of an undeclared name is deprecated)
```

That is neat: even if you never try to instantiate a function `gcc`
still tries to do basic checks on it.

But what if we call `foo()` via `this` pointer explicitly?

```c++
template <typename T> struct S {
    int foo(void) { return this->bar(); }
};

int main() {}
```

Is it valid `c++`?

`gcc-14` says it's fine:

```
$ g++-14 -c a.cc
<ok>
```

Is there a way to somehow make `bar()` available via `this`? Maybe, via
inheritance? Apparently, no. `gcc-15` now flags the code above as
unconditionally invalid:

```
$ g++-15 -c a.cc
a.cc: In member function 'int S<T>::foo()':
a.cc:2:34: error: 'struct S<T>' has no member named 'bar'
    2 |     int foo(void) { return this->bar(); }
      |                                  ^~~
```

To get it to work you need something like a
[`CRTP`](https://en.wikipedia.org/wiki/Curiously_recurring_template_pattern#Static_polymorphism)
pattern:

```c++
// Assume Derived::bar() will be provided.
template <typename Derived> struct S {
    int foo(void) { return static_cast<Derived*>(this)->bar(); }
};

int main() {}
```

Interestingly the above problem pops up time to time in real projects in
template code that was not tried after refactors. One such example is an
[`aspell` bug](https://github.com/GNUAspell/aspell/pull/650):

```c++
  template<class Parms>
  void VectorHashTable<Parms>::recalc_size() {
    size_ = 0;
    for (iterator i = begin(); i != this->e; ++i, ++this->_size);
  }
```

`gcc-14` built it just fine. `gcc-15` started rejecting the build as:

```
In file included from modules/speller/default/readonly_ws.cpp:51:
modules/speller/default/vector_hash-t.hpp:
  In member function 'void aspeller::VectorHashTable<Parms>::recalc_size()':
modules/speller/default/vector_hash-t.hpp:186:43:
  error: 'class aspeller::VectorHashTable<Parms>' has no member named 'e'
  186 |     for (iterator i = begin(); i != this->e; ++i, ++this->_size);
      |                                           ^
modules/speller/default/vector_hash-t.hpp:186:59:
  error: 'class aspeller::VectorHashTable<Parms>' has no member named '_size'; did you mean 'size'?
  186 |     for (iterator i = begin(); i != this->e; ++i, ++this->_size);
      |                                                           ^~~~~
      |                                                           size
```

`VectorHashTable` does not contain `_size` field, but it does contain
`size_` (used just a line before). `e` field is not a thing either.

The change is simple:


```diff
--- a/modules/speller/default/vector_hash-t.hpp
+++ b/modules/speller/default/vector_hash-t.hpp
@@ -183,7 +183,7 @@ namespace aspeller {
   template<class Parms>
   void VectorHashTable<Parms>::recalc_size() {
     size_ = 0;
-    for (iterator i = begin(); i != this->e; ++i, ++this->_size);
+    for (iterator i = begin(), e = end(); i != e; ++i, ++size_);
   }

 }
```

Or you could also delete the function if it was broken like that for a
while.

Another example is [`mjpegtools` bug](https://sourceforge.net/p/mjpeg/patches/63/):

```c++
// The commented-out method prototypes are methods to be implemented by
// subclasses.  Not all methods have to be implemented, depending on
// whether it's appropriate for the subclass, but that may impact how
// widely the subclass may be used.
template <class INDEX, class SIZE>
class Region2D
{
  public:
    // ...

    template <class REGION, class REGION_O, class REGION_TEMP>
    void UnionDebug (Status_t &a_reStatus,
        REGION_O &a_rOther, REGION_TEMP &a_rTemp);

    // bool DoesContainPoint (INDEX a_tnY, INDEX a_tnX);

    // ...
}

template <class INDEX, class SIZE>
template <class REGION, class REGION_TEMP>
void
Region2D<INDEX,SIZE>::UnionDebug (Status_t &a_reStatus, INDEX a_tnY,
    INDEX a_tnXStart, INDEX a_tnXEnd, REGION_TEMP &a_rTemp)
{
    // ...
            if (!((rHere.m_tnY == a_tnY
                && (tnX >= a_tnXStart && tnX < a_tnXEnd))
            || this->DoesContainPoint (rHere.m_tnY, tnX)))
                goto error;
    // ...
}
```

Here `mjpegtools` assumes that `DoesContainPoint` should come from
derived type. But modern `c++` just does allow it to be defined like that:

```
In file included from SetRegion2D.hh:12,
                 from MotionSearcher.hh:15,
                 from newdenoise.cc:19:
Region2D.hh: In member function 'void Region2D<INDEX, SIZE>::UnionDebug(Status_t&, INDEX, INDEX, INDEX, REGION_TEMP&)':
Region2D.hh:439:34: error: 'class Region2D<INDEX, SIZE>' has no member named 'DoesContainPoint'
  439 |                         || this->DoesContainPoint (rHere.m_tnY, tnX)))
      |                                  ^~~~~~~~~~~~~~~~
```

The [fix](https://sourceforge.net/p/mjpeg/Code/3513/) just deleted these
unusable functions. An alternative fix would need to look closer to a
`CRTP` tweak in our contrived example. But it's a bit more invasive change.

## parting words

`gcc-15` will reject more invalid unusable `c++` code in uninstantiated
templates. The simplest code change might be to just delete broken code.
More involved fix would require some knowledge of the codebase to fix
the declaration lookups (or to fix obvious typos).

Have fun!
