---
title: "gcc-15 bugs, pile 2"
date: April 19, 2025
---

8 more months have passed since my previous
[pile report](/posts/323-gcc-15-bugs-pile-1.html). `gcc-15` was
[branched off](https://gcc.gnu.org/pipermail/gcc/2025-April/245943.html)
from `master` and will receive only regression fixes. `master` is called
`gcc-16` now.

It's a good time to look at the compiler bugs I encountered.

## summary

I got about 30 of those:

- [`rtl-optimization/116516`](https://gcc.gnu.org/PR116516): ICE on
  `linux-6.10` due to inability to handle some address calculation
  expressions.
- [`middle-end/116516`](https://gcc.gnu.org/PR116797): ICE on
  `libvpx-1.14.1` due to a vectorizer bug that tried to access outside
  array boundary.
- [`middle-end/116814`](https://gcc.gnu.org/PR116814): ICE on
  `libjack2-1.9.22` due to `gcc` inability to generate code for
  saturated subtraction
- [`tree-optimization/116817`](https://gcc.gnu.org/PR116817): ICE on
  `libajantv2-16.2` `gcc` vectorizer broke on a loop invariant
- [`libstdc++/116857`](https://gcc.gnu.org/PR116857): `mingw32` build
  failure, was exposed after re-enabling most warnings on `gcc` headers.
- [`c++/116880`](https://gcc.gnu.org/PR116880): `co_await` use-after-free
  on `nix-2.24.8` code. A `gcc` bug in coroutine lifetime management.
- [`c++/116911`](https://gcc.gnu.org/PR116911): `qt5.qtbase` build
  failure due to `gcc` regression in assigning external linkage to local
  variables.
- [`bootstrap/117039`](https://gcc.gnu.org/PR117039): `-Werror=` `libcpp`
  `gcc` build failure due to format string problems.
- [`c++/117114`](https://gcc.gnu.org/PR117114): `-Woverloaded-virtual`
  false positives due to a `gcc` in how it tracks methods in case of
  multiple inheritance.
- [`middle-end/117141`](https://gcc.gnu.org/PR117141): duplicate pattern
  definitions for subtraction-with-saturation primitive. A build warning.
- [`c/117177`](https://gcc.gnu.org/PR117177): wrong code on global arrays
  used by `python-3.12.7` and others. `gcc` generated invalid bytes that
  represent the array.
- [`c/117190`](https://gcc.gnu.org/PR117190): ICE on `linux-6.11.3`,
  another case of `gcc` inability to generate static const arrays
  similar to the previous entry.
- [`target/117194`](https://gcc.gnu.org/PR117194): wrong code on
  `highway-1.2.0` in vectorizer code. `gcc` used incorrect order of
  operands in `ANDN` primitive.
- [`libstdc++/117220`](https://gcc.gnu.org/PR117220): `stl_iterator` and
  `clang` incompatibility: `gcc` allows slightly different mix of
  `[[..]]` and `__attribute((..))` style of attributes ordering than
  `clang`.
- [`lto/117288`](https://gcc.gnu.org/PR117288): `lto` ICE on `wolfssl`,
  constant arrays are not handled by `gcc`. This time in `LTO` bytecode.
- [`tree-optimization/117306`](https://gcc.gnu.org/PR117306): `-O3`
  vectorizer ICE on `netpbm-11.8.0` of certain `bool` calculation patterns.
- [`middle-end/117378`](https://gcc.gnu.org/PR117378): `waybar` ICE on
  `c++` due to a `gcc` bug in expansion of ternary operators.
- [`rtl-optimization/117476`](https://gcc.gnu.org/PR117476): wrong code
  on `grep` and `libgcrypt` in a code that handles zero-extension.
- [`middle-end/117496`](https://gcc.gnu.org/PR117496): infinite recursion
  on `cdrkit` due to `a | b` pattern generating still foldable result.
- [`bootstrap/117843`](https://gcc.gnu.org/PR117843): `fortran` bootstrap
  build failure (`-Werror`). A missing enum entry handling.
- [`c++/117980`](https://gcc.gnu.org/PR117980): ICE on `nix-2.25.2` where
  `gcc` transformation broke the type of underlying expression.
- [`c++/118124`](https://gcc.gnu.org/PR118124): ICE on `nss`, `c++`
  constant arrays were not handled in `initializer_list<...>`.
- [`preprocessor/118168`](https://gcc.gnu.org/PR118168): slow `mypy`
  compilation on `-Wmisleading-indentation`. `gcc` parsed the whole file
  multiple times to resolve locations.
- [`tree-optimization/118205`](https://gcc.gnu.org/PR118205): `libdeflate`
  wrong code, fails `libtiff` tests due to a `gcc` bug in handling
  certain form of `PHI` modes.
- [`tree-optimization/118409`](https://gcc.gnu.org/PR118409): `gas` is
  compiled incorrectly due to `gcc` bug in handling `xor` on sub-byte
  bit fields.
- [`c++/118856`](https://gcc.gnu.org/PR118856): `mesonlsp-4.3.7` ICE
  and wrong code due to too early temporary destruction for arrays.
- [`c++/119138`](https://gcc.gnu.org/PR119138): `mingw32` bootstrap
  failure due to a `gcc` regression in attribute tracking for pointers.
- [`middle-end/119226`](https://gcc.gnu.org/PR119226): `vifm-0.14` ICE on
  `strcspn()` due to a bad folding recently added to `gcc` just for this
  function.
- [`analyzer/119278`](https://gcc.gnu.org/PR119278): `gnutls` `-fanalyzer`
  ICE due to lack of handling of a new type for static const arrays.
- [`target/119428`](https://gcc.gnu.org/PR119428): `e2fsprogs-1.47.2`
  wrong code on bit reset due to a wrong `btr` pattern.
- [`c++/119428`](https://gcc.gnu.org/PR119646): `lix` ICE on coroutine
  code where coroutine types and values cause `gcc` to fail to handle
  more complicated (but allowed by standard) cases.

## fun bugs

### `e2fsprogs` bug

The [`e2fsprogs bug`](https://gcc.gnu.org/PR119428) was an interesting
case of wrong code. This was enough to trigger it:

```c
// $ cat bug.c
__attribute__((noipa, optimize(1)))
void bug_o1(unsigned int nr, void * addr)
{
        unsigned char   *ADDR = (unsigned char *) addr;

        ADDR += nr >> 3;
        *ADDR &= (unsigned char) ~(1 << (nr & 0x07));
}

__attribute__((noipa, optimize(2)))
void bug_o2(unsigned int nr, void * addr)
{
        unsigned char   *ADDR = (unsigned char *) addr;

        ADDR += nr >> 3;
        *ADDR &= (unsigned char) ~(1 << (nr & 0x07));
}

int main() {
  void * bmo1 = __builtin_malloc(1024);
  void * bmo2 = __builtin_malloc(1024);
  for (unsigned bno = 0; bno < 1024 * 8; ++bno) {
    __builtin_memset(bmo1, 0xff, 1024);
    __builtin_memset(bmo2, 0xff, 1024);
    bug_o1(bno, bmo1);
    bug_o2(bno, bmo2);
    if (__builtin_memcmp(bmo1, bmo2, 1024) != 0)
      __builtin_trap();
  }
}
```

Crashing as:

```
$ gcc bug.c -o bug -O0 && ./bug
Illegal instruction (core dumped)
```

The `gcc` [fix](https://gcc.gnu.org/cgit/gcc/commit/?id=584b346a4c7a6e6e77da6dc80968401a3c08161d)
amends mask calculation as:

```diff
--- a/gcc/config/i386/i386.md
+++ b/gcc/config/i386/i386.md
@@ -18168,7 +18168,8 @@
  [(set (match_dup 4) (match_dup 1))
   (set (match_dup 0)
        (any_rotate:SWI (match_dup 4)
-		       (subreg:QI (match_dup 2) 0)))]
+		       (subreg:QI
+			 (and:SI (match_dup 2) (match_dup 3)) 0)))]
  "operands[4] = gen_reg_rtx (<MODE>mode);")
 
 (define_insn_and_split "*<insn><mode>3_mask_1"
@@ -18202,7 +18203,8 @@
   == GET_MODE_BITSIZE (<MODE>mode) - 1"
  [(set (match_dup 4) (match_dup 1))
   (set (match_dup 0)
-       (any_rotate:SWI (match_dup 4) (match_dup 2)))]
+       (any_rotate:SWI (match_dup 4)
+		       (and:QI (match_dup 2) (match_dup 3))))]
  "operands[4] = gen_reg_rtx (<MODE>mode);")
 
 (define_insn_and_split "*<insn><mode>3_add"
```

Here `gcc` incorrectly compiled `bug_o2()` into a single `btr`
instruction. `gcc` assumed `btr` performs a typical 8-bit mask on
register operand like other instructions do. But in case of `btr` it's
a 3/4/5-bit mask (for 8/16/32-bit offsets).

### `mesonlsp` bug

The [`mesonlsp` bug](https://gcc.gnu.org/PR118856) was also interesting.
There seemingly trivial code:

```c++
// $ cat bug.cpp
#include <string>
#include <vector>

int main(){
  for (const auto &vec : std::vector<std::vector<std::string>>{
           {"aaa"},
       }) {
  }
}
```

crashed at runtime:

```
# ok
$ g++ bug.cpp -o bug -fsanitize=address
$ ./bug

# bad:
$ g++ bug.cpp -o bug -fsanitize=address -std=c++23
$ ./bug

=================================================================
==3828042==ERROR: AddressSanitizer: heap-use-after-free on address 0x7ba90dbe0040 at pc 0x000000404279 bp 0x7ffd9db5c110 sp 0x7ffd9db5c108
READ of size 8 at 0x7ba90dbe0040 thread T0
    #0 0x000000404278 in std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_M_data() const (bug+0x404278)
...

0x7ba90dbe0040 is located 0 bytes inside of 32-byte region [0x7ba90dbe0040,0x7ba90dbe0060)
freed by thread T0 here:
    #0 0x7f790f1180c8 in operator delete(void*, unsigned long) (/<<NIX>>/gcc-15.0.1-lib/lib/libasan.so.8+0x1180c8)
    #1 0x000000406a4b in std::__new_allocator<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >::deallocate(std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >*, unsigned long) (bug+0x406a4b)
...

previously allocated by thread T0 here:
    #0 0x7f790f1171a8 in operator new(unsigned long) (/<<NIX>>/gcc-15.0.1-lib/lib/libasan.so.8+0x1171a8)
    #1 0x000000404c9f in std::__new_allocator<std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> > >::allocate(unsigned long, void const*) (bug+0x404c9f)
...

SUMMARY: AddressSanitizer: heap-use-after-free (bug+0x404278) in std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_M_data() const
Shadow bytes around the buggy address:
  0x7ba90dbdfd80: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x7ba90dbdfe00: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x7ba90dbdfe80: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x7ba90dbdff00: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
  0x7ba90dbdff80: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
=>0x7ba90dbe0000: fa fa 00 00 00 fa fa fa[fd]fd fd fd fa fa fd fd
  0x7ba90dbe0080: fd fa fa fa fd fd fd fd fa fa fa fa fa fa fa fa
  0x7ba90dbe0100: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x7ba90dbe0180: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x7ba90dbe0200: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
  0x7ba90dbe0280: fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa fa
Shadow byte legend (one shadow byte represents 8 application bytes):
  Addressable:           00
  Partially addressable: 01 02 03 04 05 06 07
  Heap left redzone:       fa
  Freed heap region:       fd
  Stack left redzone:      f1
  Stack mid redzone:       f2
  Stack right redzone:     f3
  Stack after return:      f5
  Stack use after scope:   f8
  Global redzone:          f9
  Global init order:       f6
  Poisoned by user:        f7
  Container overflow:      fc
  Array cookie:            ac
  Intra object redzone:    bb
  ASan internal:           fe
  Left alloca redzone:     ca
  Right alloca redzone:    cb
==3828042==ABORTING
```

It's a use-after-free bug. Caused by the `gcc` bugs in temporary
variables lifetime tracking. The `gcc` fixes
([one](https://gcc.gnu.org/cgit/gcc/commit/?id=e96e1bb69c7b46db18e747ee379a62681bc8c82d),
[two](https://gcc.gnu.org/cgit/gcc/commit/?id=720c8f685210af9fc9c31810e224751102f1481e))
are not very small, thus I'll not post them here.

## histograms

As usual what are the subsystems we found the bugs in?

- `c++`: 8
- `middle-end`: 6
- `tree-optimization`: 4
- `bootstrap`: 2
- `c`: 2
- `libstdc++`: 2
- `lto`: 2
- `rtl-optimization`: 2
- `target`: 2
- `analyzer`: 1
- `preprocessor`: 1

Surprisingly this time `c++` is at the top of the list. It feels like
coroutine related bugs pushed the needle. Otherwise, `middle-end` and
`tree-optimization` that follow are expected.

## parting words

Of the bugs above it looks like I reported only 18 of those while 13
were already reported by others.

Optimized handling of global constant arrays (`#embed`-style code) caused
numerous bugs in various subsystems from compiler crashes to wrong code.

The most disruptive change probably is the switch to
[`c23`](/posts/326-gcc-15-switched-to-c23.html).

Past month was very quiet from `gcc` bugs view. `gcc-15` is in a good
shape to be released.

Have fun!
