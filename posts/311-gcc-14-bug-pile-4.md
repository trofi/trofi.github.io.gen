---
title: "gcc-14 bugs, pile 4"
date: April 13, 2024
root: "http://trofi.github.io"
---

In a few weeks `gcc-14` branch should see a `14.1.0` release.

Since [November 2023](/posts/306-gcc-14-bug-pile-3.html) I encountered
only 8 more bugs. That makes it 2 bugs per month. 4 time lower rate than
I had last time.

## summary

Bugs I saw (in discovery order):

- [`tree-optimization/112711`](https://gcc.gnu.org/PR112711): wrong code
  on `llvm-16`, `bswap` and `assume(aligned)`.
- [`c++/112869`](https://gcc.gnu.org/PR112869): `ICE` on `libmpt-0.7.3`
  when built with `-std=c++20`.
- [`tree-optimization/112991`](https://gcc.gnu.org/PR112991): `ICE` on
  `p7zip` due to value numbering bugs.
- [`bootstrap/113132`](https://gcc.gnu.org/PR113132): bootstrap build
  failure due to `-Werror`.
- [`bootstrap/113445`](https://gcc.gnu.org/PR113445): bootstrap comparison
  failure due to instruction scheduler changes.
- [`tree-optimization/114249`](https://gcc.gnu.org/PR114249): `ICE` on
  `lvm2` (wrong type transform in `SLP`).
- [`c++/114439`](https://gcc.gnu.org/PR114439): `icu4c` build failure due
  to the initialization changes in `c++`.
- [`lto/114574`](https://gcc.gnu.org/PR114574): `unbound` ICE in `-flto`
  mode.

## fun bug

Only one of 8 bug was a runtime failure on `llvm-16` in
`__builtin_assume_aligned()` handling code. It's extracted from
`EndianTest.cpp`:

```c
// $ cat EndianTest.cpp
typedef          int i32;
typedef unsigned int u32;

static inline void write_i32(void *memory, i32 value) {
  // swap i32 bytes as if it was u32:
  u32 u_value = value;
  value = __builtin_bswap32(u_value);

  // llvm infers '1' alignment from destination type
  __builtin_memcpy(__builtin_assume_aligned(memory, 1), &value, sizeof(value));
}

__attribute__((noipa))
static void bug (void) {
  #define assert_eq(lhs, rhs) if (lhs != rhs) __builtin_trap()

  unsigned char data[5];
  write_i32(data, -1362446643);
  assert_eq(data[0], 0xAE);
  assert_eq(data[1], 0xCA);
  write_i32(data + 1, -1362446643);
  assert_eq(data[1], 0xAE);
}

int main() {
    bug();
}
```

The optimization breaks simple `store-32` / `load-8` / `compare`
sequence:

```
$ gcc/xg++ -Bgcc EndianTest.cpp -o bug -O0 && ./bug
$ gcc/xg++ -Bgcc EndianTest.cpp -o bug -O2 && ./bug
Illegal instruction (core dumped)
```

There `gcc` was too optimistic in assuming that
`__builtin_assume_aligned()` returns address not aliased to input
argument. As a result `gcc` erroneously removed dead-looking stores.
[The fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=302461ad9a04d82fee904bddac69811d13d5bb6a)
drops overly optimistic assumption.

## histograms

- `tree-optimization`: 3
- `c++`: 2
- `bootstrap`: 2
- `lto`: 1

We are back to the usual `tree-optimization` at the top of subsystems
where bugs lurked.

## parting words

This cycle felt easier than the previous three. It feels like `gcc-14`
is ready for release.

Since the time `gcc-13` was released about a year ago I found 53 bugs
(past piles
[1](/posts/291-gcc-14-bugs-pile-1.html),
[2](/posts/296-gcc-14-bugs-pile-2.html),
[3](/posts/306-gcc-14-bug-pile-3.html)).
This is about one bug a week.

I think the most impactful `gcc-14` change will probably be
`-Werror=implicit-int`, `-Werror=implicit-function-declaration` and
friends (see <https://gcc.gnu.org/gcc-14/porting_to.html> for more
details). This will break quite a few old unmaintained but used
everywhere `C` projects like `zip`, `opensp`, `jam`, `directfb`.

Have fun!
