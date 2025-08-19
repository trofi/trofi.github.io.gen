---
title: "gcc-15 switched to C23"
date: November 17, 2024
---

## Tl;DR

In November `gcc`
[merged](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=55e3bd376b2214e200fa76d12b67ff259b06c212)
the switch from `C17` (`-std=gnu17`) to `C23` (`-std=c23`) language
standard used by default for `C` code.
This will cause quite a few build failures in projects written in
`C`. A few example fixes:

- [`libffi`](https://github.com/libffi/libffi/pull/861/files): optional
  `va_start` parameter.
- [`ncompress`](https://github.com/vapier/ncompress/pull/40/files):
  `void foo()` changed the meaning to `void foo(void)`.
- [`ell`](https://lore.kernel.org/ell/20241117001814.2149181-1-slyich@gmail.com/T/#t)
  `bool`, `true` and `false` are new keywords. And specifically `false`
  is not equals to `0` or `NULL`.

## more words

`C23` has a few high-visibility breaking changes compared to `C17`.

### `bool`, `true`, and `false` are unconditionally defined now

`true` and `false` are now predefined constants (instead of being a
part of `<stdbool.h>` macros and `typedefs`). Thus, code like below does
not compile any more:

```c
enum { false = 0; }
typedef int bool;
```

Error messages:

```
$ printf 'enum { false = 0 };' | gcc -std=c17 -c -x c -
$ printf 'enum { false = 0 };' | gcc -c -x c -
<stdin>:1:8: error: expected identifier before 'false'

$ printf 'typedef int bool;' | gcc -std=c17 -c -x c -
$ printf 'typedef int bool;' | gcc -c -x c -
<stdin>:1:13: error: two or more data types in declaration specifiers
<stdin>:1:1: warning: useless type name in empty declaration
```

The fix is usually to use `<stdbool.h>` or avoid name collisions.
Example affected project is `linux`.

### partially defined `int (*)()` function prototypes are just `int (*)(void)` now

This one is trickier to fix when intentionally used. `C` happened to
allow the following code:

```c
// $ cat a.c
typedef void (*PF)();

static int f0(void)  { return 42; }
static int f1(int a) { return 42 + a; }

int main() {
    PF pf;

    // 0-argument function pointer
    pf = f0;
    pf();

    // 1-argument function pointer
    pf = f1;
    pf(42);

    // 3-argument function pointer: an odd one, but happens to work
    pf(42,42,42);
}
```

But not any more:

```
$ gcc -std=c17 -c a.c
$ gcc -c a.c
a.c: In function 'main':
a.c:15:8: error: assignment to 'PF' {aka 'int (*)(void)'} from incompatible pointer type 'int (*)(int)' [-Wincompatible-pointer-types]
   15 |     pf = f1;
      |        ^
a.c:16:5: error: too many arguments to function 'pf'
   16 |     pf(42);
      |     ^~
a.c:19:5: error: too many arguments to function 'pf'
   19 |     pf(42,42,42);
      |     ^~
```

This hack is used at least in `ski`, `ghc` and `ncompress`. But more
frequently it's use is an accident (`ell`, `iwd`, `bash` and a few others).

## parting words

Quick quiz: the above changes look like they tickle some very obscure
case. How many packages are affected on a typical desktop system? What
would be your guess? 1? 5? 100? 1000?

So far on my system (~2000 installed packages) I observed the failures
of the following projects:

- `linux`
- `speechd`
- `vde2`
- `sane-backends`
- `timidity`
- `neovim`
- `bluez`
- `samba`
- `weechat`
- `iwd`
- `protobuf`
- `netpbm`
- `mariadb-connector-c`
- `liblqr1`
- `sqlite-odbc-driver`
- `python:typed-ast`
- `python2`
- `perl:XS-Parse-Keyword`
- `pgpdump`
- `ell`
- `SDL-1`
- `ruby-3.1`
- `dnsmasq`
- `ghc`
- `gnupg`
- `ghostscript`
- `procmail`
- `jq`
- `libsndfile`
- `ppp`
- `time`
- `postfi`x
- `mcpp`
- `xmlrpc-c`
- `unifdef`
- `hotdoc`
- `mypy`
- `rustc`
- `xorg:libXt`
- `rsync`
- `oniguruma`
- `ltrace`
- `sudo`
- `lsof`
- `lv`
- `dbus-glib`
- `argyllcms`
- `valgrind`
- `postgresql-14`
- `gdb`
- `git`
- `ncompress`
- `w3m`
- `freeglut`
- `xcur2png`
- `vifm`
- `p11-kit`
- `cyrus-sasl`
- `xvidcore`
- `guile`
- `editline`
- `e2fsprogs`
- `gsm`
- `libconfig`
- `db`
- `libtirpc`
- `nghttp2`
- `libkrb5`
- `libgpg-error`
- `cpio`
- `sharutils`
- `gpm`
- `expect`
- `ncurses`
- `yasm`
- `texinfo-6.7`
- `gettext`
- `unzip`
- `gdbm`
- `m4`
- `binutils`
- `ed`
- `gmp`
- `bash`

That's more than 80 packages, or about 4% of all the packages I have
installed.

Looks like `gcc-15` will be a disruptive release (just like `gcc-14`)
that will require quite a few projects to adapt to new requirements
(either by fixing code or by slapping `-std=gnu17` as a requirement).

Most of the build failures above are not yet fixed upstream. These can be
good first contribution fixes if you are thinking of making one.

Have fun!
