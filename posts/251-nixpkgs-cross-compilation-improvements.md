---
title: "nixpkgs cross-compilation improvements"
date: July 30, 2022
---

## TL;DR

`gcc` cross-compilers are now stripped!
For example [nixpkgs/182513](https://github.com/NixOS/nixpkgs/pull/182513)
decreases `wine` closure (or any other `pkgsCross.*.stdenv`)
by a `~1GB`.

[nixpkgs/182513](https://github.com/NixOS/nixpkgs/pull/182513) also fixes
stripping of static libraries. You can now remove existing `dontStrip = true;`
workarounds in `nixpkgs` if you had to put them in to restore linkage.
For example [nixpkgs/183484](https://github.com/NixOS/nixpkgs/pull/183484)
decreases `mingw` closure by `200MB`.

With [nixpkgs/181943](https://github.com/NixOS/nixpkgs/pull/181943) `gcc`
cross-compilers and cross-built `gcc`s now enable expected features based
on target's libc headers. Previously libc headers were not passed correctly.
That caused cross-`gcc` and cross-build `gcc` to assume too conservative
assumptions about libc like use of `libssp` on targets or use of executable
stack support.

## Story mode

I like cross-compilation. It's a great way to peek at other
CPU architectures' properties without having to deal with
real hardware.
Cross-compilation is fundamentally just a compilation. The compiler
should emit code for that one CPU type. Should be a solved problem by
now, right? If you ever tried to cross-compile something large you
probably already know the complications that usually arise from it.

## The problem

Scrolling through open `nixpkgs` PRs I stopped on this one:
[`stdenv: lib{gmp,mpc,mpfr,isl}-stage3: isPower64 -> no -fstack-protector`](https://github.com/NixOS/nixpkgs/pull/181802).
It looked like something I could review. A few month ago I fiddled with
[`stdenv bootstrap`](/posts/240-nixpkgs-bootstrap-intro.html) when I dealt
with `glibc-2.35` update. If nothing else I knew `stdenv` is a bit hard
to reason about when it comes to figuring out bootstrap dependency tower.

In the PR Adam Joseph shared the problem he was trying to address. Somewhere
at bootstrap time on `powerpc64le-linux` platform one of the intermediate
`gcc` builds failed to link as:

```
/tmp/nix-build-gcc-10.3.0.drv-0/build/./prev-gcc/xg++ \
    -o cc1plus \
    cp/cp-lang.o ... main.o ... ../libdecnumber/libdecnumber.a ... -lz
/<<NIX>>/binutils-2.35.2/bin/ld: /<<NIX>>/mpfr-4.1.0/lib/libmpfr.a(mpfr-gmp.o):
  (.toc+0x8): undefined reference to `__stack_chk_guard'
/<<NIX>>/binutils-2.35.2/bin/ld: /<<NIX>>/mpfr-4.1.0/lib/libmpfr.a(mpfr-gmp.o):
  (.toc+0x8): undefined reference to `__stack_chk_guard'
...
```

Adam suggested disabling stack protector just for a few bootstrap
packages (`mpfr` and the similar) to get past the error. While it probably
gets the job done it also flags an assumption incompatibility between
compilers. It should not normally happen.

## What is `libssp`?

What is that `__stack_chk_guard` thing anyway? What is supposed to
provide it?
It has something to do with `-fstack-protector*` set of options in
`gcc`. Let's pick a trivial `void wr(char * p, char v){ *p = v; }`
function and build it with and without stack protector to get a
feel of it:

```asm
$ printf "void wr(long * p, long v){ *p = v; }" | gcc -S -x c - -o - -fno-stack-protector -O2

        .file   "<stdin>"
        .text
        .p2align 4
        .globl  wr
        .type   wr, @function
wr:
.LFB0:
        .cfi_startproc

        movq    %rsi, (%rdi)       ; Our `*p = v;` code
        ret

        .cfi_endproc
.LFE0:
        .size   wr, .-wr
        .ident  "GCC: (GNU) 12.1.0"
        .section        .note.GNU-stack,"",@progbits
```

The actual code takes 1 line here: `movq    %rsi, (%rdi)`. It stores
64-bit value at `%rsi` register (`long v` parameter) to memory pointed
by `%rdi` (`long * p` parameter). The rest is a bit of metadata to get
the code placed properly into the `ELF` file.

Now let's add stack protector code to it with `-fstack-protector-all`
option:

```asm
$ printf "void wr(long * p, long v){ *p = v; }" | gcc -S -x c - -o - -fstack-protector-all -O2

        .file   "<stdin>"
        .text
        .p2align 4
        .globl  wr
        .type   wr, @function
wr:
.LFB0:
        .cfi_startproc
        subq    $24, %rsp          ; allocated a bit of space for canary on stack
        .cfi_def_cfa_offset 32
        movq    %fs:40, %rax       ; canary = %fs:40
        movq    %rax, 8(%rsp)      ; store canary on stack
        xorl    %eax, %eax         ; clean registers up

        movq    %rsi, (%rdi)       ; initial `*p = v;` code

        movq    8(%rsp), %rax      ; load canary value back from stack
        subq    %fs:40, %rax       ; compare to the reference value
        jne     .L5                ; exit if canary comparison failed
        addq    $24, %rsp
        .cfi_remember_state
        .cfi_def_cfa_offset 8
        ret                        ; exit `wr()`
.L5:
        .cfi_restore_state
        call    __stack_chk_fail   ; handle failure
        .cfi_endproc
.LFE0:
        .size   wr, .-wr
        .ident  "GCC: (GNU) 12.1.0"
        .section        .note.GNU-stack,"",@progbits
```

Now our original code was diluted with 9(!) extra instructions related
to stack protector checks. To make the checking work the compiler uses
`%fs:40` thread-local memory location as a canary value. At start of
each function code places canary on stack (with `movq %rax, 8(%rsp)`)
and at the end of function code reads the canary value back from the
same location (with `movq 8(%rsp), %rax`) and checks if it was unchanged
(with `subq %fs:40, %rax` and `jne .L5`). If the canary check
failed then `__stack_chk_fail()` is called.

If we generalize the above to pseudo-code `gcc` turned our program to
something like:

```c
void wr(long * p, long v) {
    long canary = __stack_chk_guard;

    *p = v; // original code

    if (canary != __stack_chk_guard)
        __stack_chk_fail();
    return 0;
}
```

Something has to provide that `__stack_chk_fail()` function. In case
of `glibc` that function is provided by `libc.so.6` library starting
from `2.4` version:

```
$ nm -D <<NIX>>/glibc-2.34-210/lib/libc.so.6  | fgrep __stack
0000000000116de0 T __stack_chk_fail@@GLIBC_2.4
```

Something also has to arrange addressable `%fs:40` memory. In case of
`glibc` that value is placed by `glibc`
[itself](https://sourceware.org/git/?p=glibc.git;a=blob;f=csu/libc-start.c;h=543560f36c33b07a1fbe1b7e4578374fe8007b1f;hb=da3b9f445195bdbe77e66e4fc137a5732637b85f#l311)
in the early startup code. `%fs` is a `TLS` segment register for a
segment maintained by kernel: kernel changes the segment address
on thread switch.
Thus, the above assembly code generated by `gcc` implies presence of
operating system and supporting libc.

Not all architectures have a way to address thread-local data in that
fashion. For targets without `TLS` `glibc` emulates a bit of stack
protection with a global variable `uintptr_t __stack_chk_guard attribute_relro;`.
Turns out it's not the only implementation of stack protector prologue
and epilogue even on `x86_64`. What happens on `glibc-2.0`? Or on other
libcs or kernels?

The implementation we saw above was the default case of `--disable-libssp`
mode of `gcc`. We can also build `gcc` in `--enable-libssp`. In this
case we get a bit different code:

```asm
# Locally built `gcc` build with `./configure --enable-libssp`:
$ printf "void wr(long * p, long v){ *p = v; }" | gcc/xgcc -Bgcc -S -x c - -o - -fstack-protector-all -O2

        .file   "<stdin>"
        .text
        .p2align 4
        .globl  wr
        .type   wr, @function
wr:
.LFB0:
        .cfi_startproc
        subq    $24, %rsp                       ; allocated a bit of space for canary on stack
        .cfi_def_cfa_offset 32
        movq    __stack_chk_guard(%rip), %rax   ; canary = __stack_chk_guard
        movq    %rax, 8(%rsp)                   ; store canary on stack
        xorl    %eax, %eax                      ; clean registers up

        movq    %rsi, (%rdi)                    ; initial `*p = v;` code

        movq    8(%rsp), %rax                   ; load canary value back from stack
        subq    __stack_chk_guard(%rip), %rax   ; compare to the reference value
        jne     .L5                             ; exit if canary comparison failed
        addq    $24, %rsp
        .cfi_remember_state
        .cfi_def_cfa_offset 8
        ret                                     ; exit `wr()`
.L5:
        .cfi_restore_state
        call    __stack_chk_fail                ; handle failure
        .cfi_endproc
.LFE0:
        .size   wr, .-wr
        .ident  "GCC: (GNU) 13.0.0 20220724 (experimental)"
        .section        .note.GNU-stack,"",@progbits
```

The assembly code is very close to `--disable-libssp` case. The
difference is how canary is read:
instead of using thread-local `%fs:40` location `gcc` now resorts
to using a global `__stack_chk_guard` variable.
Note that `glibc` does not provide `__stack_chk_guard` symbol. In `gcc`
case expected to come from `libssp` library we just enabled. `gcc` spec
files add `-lssp` (or equivalent) to all link commands.
This means that binaries produced by `--enable-libssp` and by
`--disable-libssp` are slightly incompatible: the final result needs
to be linked by `--enable-libssp` `gcc`. Otherwise, we'll get linker
failures:

```
$ printf "void wr(long * p, long v){ *p = v; }" | gcc/xgcc -Bgcc -c -x c - -fPIC -o a.o -fstack-protector-all -O2
$ gcc -shared a.o -o liba.so -Wl,-no-undefined
<<NIX>>/binutils-2.38/bin/ld: a.o: in function `wr':
<stdin>:(.text+0x7): undefined reference to `__stack_chk_guard'
collect2: error: ld returned 1 exit status
```

Looks familiar? That's exactly the same failure we started with.

## So why do we get a mix of `gcc` flavors?

Not all libc versions provide stack protector infrastructure. `gcc`
tries to guess at `./configure` time by
[peeking](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/configure.ac;h=446747311a6aec3c810ad6aa4190f7bd383b94f7;hb=HEAD#l6734)
at target libc headers:

```m4
[if test -f $target_header_dir/features.h \
  && glibc_version_major_define=`$EGREP '^[     ]*#[    ]*define[       ]+__GLIBC__[
   ]+[0-9]' $target_header_dir/features.h` \
  && glibc_version_minor_define=`$EGREP '^[     ]*#[    ]*define[       ]+__GLIBC_MIN
OR__[   ]+[0-9]' $target_header_dir/features.h`; then
  glibc_version_major=`echo "$glibc_version_major_define" | sed -e 's/.*__GLIBC__[
     ]*//'`
  glibc_version_minor=`echo "$glibc_version_minor_define" | sed -e 's/.*__GLIBC_MINOR
__[     ]*//'`
fi]
...
# Test for stack protector support in target C library.
AC_CACHE_CHECK(__stack_chk_fail in target C library,
  gcc_cv_libc_provides_ssp,
  [gcc_cv_libc_provides_ssp=no
    ...
    case "$target" in
       *-*-musl*)
         # All versions of musl provide stack protector
         gcc_cv_libc_provides_ssp=yes;;
       *-*-linux* | *-*-kfreebsd*-gnu)
      # glibc 2.4 and later provides __stack_chk_fail and
      # either __stack_chk_guard, or TLS access to stack guard canary.
      GCC_GLIBC_VERSION_GTE_IFELSE([2], [4], [gcc_cv_libc_provides_ssp=yes], [
      ...
       *) gcc_cv_libc_provides_ssp=no ;;
    esac
  fi])
```

Here `configure.ac` just greps `glibc` `features.h` header for library
version. It does not do usual linking probing as bootstrap frequently
starts from `gcc` and `glibc` headers alone.
In `nixpkgs` case `gcc` build in `cross-compile` case
(`host != target`) was looking at a wrong directory location by
attempting to add `sysroot`
[prefix](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/configure.ac;h=446747311a6aec3c810ad6aa4190f7bd383b94f7;hb=HEAD#l2438):

```
if test x$host != x$target || test "x$TARGET_SYSTEM_ROOT" != x ||
   test x$build != x$host || test "x$with_build_sysroot" != x; then
  if test "x$with_build_sysroot" != x; then
    BUILD_SYSTEM_HEADER_DIR=$with_build_sysroot'$${sysroot_headers_suffix}$(NATIVE_SYSTEM_HEADER_DIR)'
  else
    BUILD_SYSTEM_HEADER_DIR='$(CROSS_SYSTEM_HEADER_DIR)'
  fi

  if test x$host != x$target
  then
    CROSS="-DCROSS_DIRECTORY_STRUCTURE"
    ALL=all.cross
    SYSTEM_HEADER_DIR=$BUILD_SYSTEM_HEADER_DIR
  elif test "x$TARGET_SYSTEM_ROOT" != x; then
    SYSTEM_HEADER_DIR='$(CROSS_SYSTEM_HEADER_DIR)'
  fi

  if test "x$with_build_sysroot" != "x"; then
    target_header_dir="${with_build_sysroot}${native_system_header_dir}"
  elif test "x$with_sysroot" = x; then
    target_header_dir="${test_exec_prefix}/${target_noncanonical}/sys-include"
  elif test "x$with_sysroot" = xyes; then
    target_header_dir="${test_exec_prefix}/${target_noncanonical}/sys-root${native_system_header_dir}"
  else
    target_header_dir="${with_sysroot}${native_system_header_dir}"
  fi
else
  target_header_dir=${native_system_header_dir}
fi
```

Note how hard `gcc` tries:

- `${buildsysroot}/${native_system_header_dir}`
- `${exec_prefix}/${target}/sys-include`
- `${exec_prefix}/${target}/sys-root${native_system_header_dir}`
- `${sysroot}${native_system_header_dir}`

`nixpkgs` provided none of these directories and build was falling back
to outdated `glibc-0.0` assumption.
Thus initial fix was simple: just add `--with-build-sysroot=/` option to
`gcc` `./configure` to trick it to use `/${native_system_header_dir}` path.
One-liner change! This allowed me to cross-build `gcc` for
`powerpc64le-linux` and make sure stack protector is using `glibc`
support code. Are we done?

## Pandora's box

The `--with-build-sysroot=/` now started enabling all sorts of
libc-specific features. That should be fine on it's own, but for
`nixpkgs` cross-build (`build != host == target`) case it was
like that for the first time.
Various `linux` targets just worked with the fix. Mostly because
we are compiling from `glibc` to `glibc`. Or from `glibc` to `musl`.
It's usually not that bad to miss a feature or two.

I was confident of the fix, but [`ofborg`](https://github.com/NixOS/ofborg)
build test told me that I broke `x86_64-darwin` `gcc` build:

```
impure path `//' used in link
collect2: error: ld returned 1 exit status
```

After a bit of debugging I found it to be just a false positive check
failure in `nixpkgs`-specific `ld` wrapper script. Wrapper complained
that `-syslibroot //` refers outside `/nix/store` path and thus breaks
the sandboxing. But in reality it's a no-op flag. Thus, I just skipped
this specific path in the wrapper.

I tried to cross-build `gcc`. It failed again. This time `ofborg` was
still unhappy and complained about missing `sys/sdt.h` header.
That was surprising: `darwin's` libc does provide `sys/sdt.h`,
while `glibc` does not. Why does it even try to use that header?
Normally `gcc` `configure.ac` [probes it](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/configure.ac;h=446747311a6aec3c810ad6aa4190f7bd383b94f7;hb=HEAD#l6807)
as a target header as well:

```shell
# Test for <sys/sdt.h> on the target.
GCC_TARGET_TEMPLATE([HAVE_SYS_SDT_H])
AC_MSG_CHECKING(sys/sdt.h in the target C library)
have_sys_sdt_h=no
if test -f $target_header_dir/sys/sdt.h; then
  have_sys_sdt_h=yes
  AC_DEFINE(HAVE_SYS_SDT_H, 1,
            [Define if your target C library provides sys/sdt.h])
fi
AC_MSG_RESULT($have_sys_sdt_h)
```

The answer was straightforward: `nixpkgs` incorrectly used host's
headers as target headers!

After I sorted this failure yet another failure came up: `pkgsLLVM`
bootstrap was broken because `gcc` enables cross-compilation mode
for `build != host || host != target` case. But `nixpkgs` uses
`x86_64-unknown-linux-gnu` for both `gcc` (host) and `llvm`
(target) toolchains and bootstraps it as a proper cross-compiler.
That was easy to fix with [`nixpkgs/182666`](https://github.com/NixOS/nixpkgs/pull/182666).

Is that it? I'm not sure. I think we have a few more workarounds
buried in `nixpkgs` that stemmed from the fact that we used wrong
headers. One bug at a time.

## Parting words

`nixpkgs` makes it trivial to try various cross-compilers with a
single command. `darwin` port was very useful to expose two bugs
in generic include layout scheme `nixpkgs` was using.

Reproducible environment made it possible to debug early stage of
`gcc` bootstrap when libc is not yet present for target. When I did
a similar work on Gentoo's `crossdev` I was frequently tricked by
the fact that building initial cross-toolchain results
in a different result than after a `crossdev` rerun.

`gcc` `./configure` is surprisingly resilient to all the invalid
configurations you throw at it. It always manages to produce something
that mostly works and gets you going as an initial porting effort.
I think it's a good thing in the toolchain world as target environments
are so diverse. But it takes some time to debug it efficiently.

Have fun!
