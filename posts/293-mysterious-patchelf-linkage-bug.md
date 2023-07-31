---
title: "Mysterious patchelf linkage bug"
date: July 31, 2023
---

Today's mystery started from [this build failure](https://hydra.nixos.org/build/229663092)
noticed by Vladimir on `staging-next` branch of `nixpkgs`.

There `aarch64-linux` build
[failed](https://hydra.nixos.org/log/k9mdaj5a9rbpkc8x9hj9mw3papraamcn-patchelf-0.15.0.drv)
to compile `patchelf` as part of `stdenv` with this mysterious error:

```
Making all in src
make[1]: Entering directory '/build/patchelf-0.15.0/src'
g++ -DPACKAGE_NAME=\"patchelf\" -DPACKAGE_TARNAME=\"patchelf\" -DPACKAGE_VERSION=\"0.15.0\" -DPACKAGE_STRING=\"patchelf\ 0.15.0\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -DPACKAGE=\"patchelf\" -DVERSION=\"0.15.0\" -I.    -Wall -std=c++17 -D_FILE_OFFSET_BITS=64     -g -O2 -c -o patchelf.o patchelf.cc
g++ -Wall -std=c++17 -D_FILE_OFFSET_BITS=64     -g -O2   -o patchelf patchelf.o  
ld: patchelf.o: in function `__gnu_cxx::__exchange_and_add(int volatile*, int)':
...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:66:
  undefined reference to `__aarch64_ldadd4_acq_rel'
ld: ...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:66:
  undefined reference to `__aarch64_ldadd4_acq_rel'
ld: ...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:66:
  undefined reference to `__aarch64_ldadd4_acq_rel'
ld: ...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:66:
  undefined reference to `__aarch64_ldadd4_acq_rel'
ld: patchelf.o: in function `__gnu_cxx::__atomic_add(int volatile*, int)':
...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:71:
  undefined reference to `__aarch64_ldadd4_acq_rel'
ld: patchelf.o:...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:71:
  more undefined references to `__aarch64_ldadd4_acq_rel' follow
collect2: error: ld returned 1 exit status
```

It is the `build.log` in almost all of it's entirety. I had a small
change in `staging-next` branch which should absolutely not cause that
failure. But I was not sure (one can never be sure when it come to the
toolchain bugs).

**Quick quiz**: why does it happen? A `gcc` bug? A `binutils` bug? Wrong
library lookup paths in `cc-wrapper` or `ld-wrapper`? Or a heisenbug?

## First hyporthesis

I had some past experience with error like that in recent the past
[here](https://github.com/NixOS/nixpkgs/pull/158047) and
[here](https://github.com/NixOS/nixpkgs/issues/201254).

In both cases it was a `nixpkgs`-specific problem of mixing multiple
toolchain versions in a single `stdenv`. I expect this kind of problem
to come back from time to time until bootstrap process is changed to
avoid any mixing of toolchain versions. Thus I was looking forward to
debug yet another one of those.

I ran the bisect and got the
[Merge pull request #245550 from trofi/gcc-restore-sys-include](https://github.com/NixOS/nixpkgs/commit/8470989a96eb1c0e0c12c30c1b7bd7174ed9349b).
as the culprit.

My own commit! Uh-on. One of the problems is that it's a merge commit of
the change, not the change itself. Why did bisect skip the change
itself? Why the change caused this change at all? It did not make sense.
Reverting the commit on top of `staging-next` did fix the `patchelf`
linkage. Should be the culprit then? I was about to submit the revert
and move on to less cryptic things.

But for some reason just before giving up I tried to run `--rebuild` on
a random "good" commit and got the `patchelf` linkage failure! That
meant the error was non-deterministic. It did not make sense.

On one hand `sys-include` commit above is quite relevant to the way
`gcc` bootstraps. On the other hand it is not supposed to lead to
non-deterministic failures.

I had to start from the first principles to see where and how linkage
process breasks.

## What is this error about?

Let's look at the specifics of code using atomics in `gcc`.

`libstdc++` (`gcc`s `c++` template library) uses atomic operations in
various containers. For example `<string>` uses atomics to implement
copy-on-write semantics. Naturally `patchelf` uses a bit of
`std::String` as well. Thus it's expected to use a bit of atomics like
`__atomic_fetch_add()` builtin.

Each architecture implements atomics in slightly different ways: some
an get away with a single instruction. Some require quite a bit fo code.
Let's have a look at `x86_64` and `aarch64` to see how close they are.

I'll use the very `/nix/store/c7qmp1dgqf3hh4fjw74y2k662nmaslcy-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:66`
source code bit from the error we saw above.

```c
// atomic.c
int f(int * p, int v) {
    return __atomic_fetch_add(p, v, __ATOMIC_ACQ_REL);
}
```

The function `f()` here atomically adds a `v` value stored at `p` and
returns some result.

`x86_64` generates the following code:

```
$ gcc -O2 -S a.c -o -
    f:
        movl    %esi, %eax
        lock xaddl      %eax, (%rdi)
        ret
```

It's literally one `lock xaddl` instruction. How about `aarch64`? Is it
as simple?

```
$ aarch64-unknown-linux-gnu-gcc -O2 -S a.c -o -
        .arch armv8-a
    f:
        mov     x2, x0
        stp     x29, x30, [sp, -16]!
        mov     w0, w1
        mov     x29, sp
        mov     x1, x2
        bl      __aarch64_ldadd4_acq_rel
        ldp     x29, x30, [sp], 16
        ret
```

We see a big difference here: `gcc` emits call into outside
`__aarch64_ldadd4_acq_rel` function. It's code hides in `libgcc.a`:

```
__aarch64_ldadd4_acq_rel:
   0:           bti     c
   4:           adrp    x16, 0 <__aarch64_have_lse_atomics>
                4: R_AARCH64_ADR_PREL_PG_HI21   __aarch64_have_lse_atomics
   8:           ldrb    w16, [x16]
                8: R_AARCH64_LDST8_ABS_LO12_NC  __aarch64_have_lse_atomics
   c:           cbz     w16, 18 <__aarch64_ldadd4_acq_rel+0x18>
  10:           ldaddal w0, w0, [x1]
  14:           ret
  18:           mov     w16, w0
  1c:           ldaxr   w0, [x1]
  20:           add     w17, w0, w16
  24:           stlxr   w15, w17, [x1]
  28:           cbnz    w15, 1c <__aarch64_ldadd4_acq_rel+0x1c>
  2c:           ret
```

Quite a bit of code here as well: if CPU supports `ldaddal` then
`libgcc.a` uses that. Otherwise it falls back to
`ldaxr / add / stlxr / cbnz`.

This amount of code is probably the reason why the code is not inlined
by `gcc`. For comparison `clang` does something a bit different:

```
$ clang -O2 -S a.c -o - -target aarch64-unknown-linux
    f:
.LBB0_1:
        ldaxr   w8, [x0]
        add     w9, w8, w1
        stlxr   w10, w9, [x0]
        cbnz    w10, .LBB0_1
        mov     w0, w8
        ret
```

Here `clang` inlined `ldaxr / add / stlxr / cbnz` and did not consider
`ldaddal` at all. If we nudge `gcc` a bit we can force it to inline
`ldaddal` with `-march=` flag:

```
$ aarch64-unknown-linux-gnu-gcc -O2 -S a.c -o - -Wall -march=armv8.1-a
    f:
        ldaddal w1, w0, [x0]
        ret
```

And the same for `clang`:

```
$ clang -O2 -S a.c -o - -target aarch64-unknown-linux -march=armv8.1-a
    f:
        ldaddal w1, w0, [x0]
        ret
```

All of the above tells us that sometimes `gcc` generates calls into
"external" libraries like `libgcc.a` (or `libgcc_s.so`) to implement
larger primitives. And this decision might depend on the backend
architecture and compiler flags.

## Looking at the linkage command

I filed [the bug](https://github.com/NixOS/nixpkgs/issues/246147) to
start capturing important details of the bug to see how far into it I
can get.

First I needed to extract exact attribute failing to build on `aarch64`.

I added `boot.binfmt.emulatedSystems = [ "aarch64-linux" ];` to
`/etc/nixos/configuration.nix` to get `binfmt-misc` wrapper and ran a
few build commands like:

```
$ nix build --no-link -f. --argstr system aarch64-linux patchelf -L
```

to point at the exact failing attribute. It ended up being
`stdenv.__bootPackages.stdenv.__bootPackages.stdenv.__bootPackages.patchelf`.
The attribute points right in the middle of `stdenv` bootstrap.

I dropped into interactive shell to poke at the exact details of build
failure against `good` and `bad` states with `nix develop`:

```
$ nix develop -f. --argstr system aarch64-linux patchelf
error (ignored): error: '--arg' and '--argstr' are incompatible with flakes
$$ genericBuild
...
$$ cd src
$$ g++ -Wall -std=c++17 -D_FILE_OFFSET_BITS=64     -g -O2   -o patchelf patchelf.o
ld: patchelf.o: in function `__gnu_cxx::__exchange_and_add(int volatile*, int)':
...-xgcc-12.3.0/include/c++/12.3.0/ext/atomicity.h:66:
  undefined reference to `__aarch64_ldadd4_acq_rel'
...
```

Now we can check where exactly linker looks the libraries up by adding
`-Wl,--verbose`:

```
$$ g++ -Wall -std=c++17 -D_FILE_OFFSET_BITS=64 -g -O2 -o patchelf patchelf.o -Wl,--verbose
...
==================================================
ld: mode aarch64linux
attempt to open ...-glibc-2.37-8/lib/crt1.o succeeded
...-glibc-2.37-8/lib/crt1.o
attempt to open ...-glibc-2.37-8/lib/crti.o succeeded
...
```

Here is the diff against `bad` and `good` environments:

```diff
$ diff -u <(cat /tmp/bad | unnix) <(cat /tmp/good | unnix)
--- /dev/fd/63  2023-07-30 08:26:51.561118824 +0100
+++ /dev/fd/62  2023-07-30 08:26:51.561118824 +0100
@@ -273,6 +273,8 @@
 attempt to open /<<NIX>>/xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.so failed
 attempt to open /<<NIX>>/xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a succeeded
 /<<NIX>>/xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
+(/<<NIX>>/xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a)ldadd_4_4.o
+(/<<NIX>>/xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a)lse-init.o
 /<<NIX>>/xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
 attempt to open /<<NIX>>/bootstrap-stage0-glibc-bootstrapFiles/lib/libgcc.so failed
 attempt to open /<<NIX>>/bootstrap-stage0-glibc-bootstrapFiles/lib/libgcc.a failed
```

I expected some difference in library version outputs, some order
difference in path lookups. But we see nothing like that here. The only
change is the difference in access to individual object files in
`libgcc.a`: `ldadd_4_4.o` and `lse-init.o` which we expect in both cases.

And on inspection of `libgcc.a` I noticed that in `bad` case it was
corrupted:

```
$ nm ...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
nm: ...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a:
  malformed archive
```

How did we manage to corrupt `libgcc.a`? A bit of strip log spills the
clue:

```
stripping (with command strip and flags -S -p) in
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
...-xgcc-12.3.0/
lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc_eh.a
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcov.a
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/crtbegin.o
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/crtbeginS.o
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/crtbeginT.o
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/crtend.o
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/crtendS.o
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/crtfastmath.o
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc_eh.a
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcov.a
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/crtbegin.o
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/crtbeginS.o
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/crtbeginT.o
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/crtend.o
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/crtendS.o
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/crtfastmath.o
```

It's somewhat hard to read but we have `libgcc.a` twice in the list:

```
...-xgcc-12.3.0/lib/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
...-xgcc-12.3.0/lib64/gcc/aarch64-unknown-linux-gnu/12.3.0/libgcc.a
```

Both are the same file as `lib64` is a symlink to `lib`:

```
$ ls -ld ...-xgcc-12.3.0/lib64
lrwxrwxrwx 1419 root root 3 Jan  1  1970 ...-xgcc-12.3.0/lib64 -> lib
```

## Race condition

So even if the file was stripped twice, why would it be a problem? Isn't
`strip` idempotent in this regard?

It would as long as `strip` is ran sequentially. And it used to be the
case for a while. But a few weeks ago the `strip` hook was made parallel
in [PR #207101](https://github.com/NixOS/nixpkgs/pull/207101)!

As a result two `strip` commands had a chance to open `libgcc.a`, strip
it and wripte the result back. Sometimes you are lucky and you get
something that works. But sometimes you are not so lucky and one of the
`stip` commands reads incompletely written `libgcc.a` by the previous
`strip`.

## The fix

The fix (or the workaround) is not to attemt to process the same file
twice. [PR #246164](https://github.com/NixOS/nixpkgs/pull/246164)
implements naive for of the symlink resolution via `realpath` and
double-strip avoidance via `sort -u`:

```diff
--- a/pkgs/build-support/setup-hooks/strip.sh
+++ b/pkgs/build-support/setup-hooks/strip.sh
@@ -68,6 +68,11 @@ stripDirs() {
         striperr="$(mktemp 'striperr.XXXXXX')"
         # Do not strip lib/debug. This is a directory used by setup-hooks/separate-debug-info.sh.
         find $paths -type f -a '!' -path "$prefix/lib/debug/*" -print0 |
+            # Make sure we process files under symlinks only once. Otherwise
+            # 'strip` can corrupt files when writes to them in parallel:
+            #   https://github.com/NixOS/nixpkgs/issues/246147#issuecomment-1657072039
+            xargs -r -0 -n1 -- realpath -z | sort -u -z |
+
             xargs -r -0 -n1 -P "$NIX_BUILD_CORES" -- $cmd $stripFlags 2>"$striperr" || exit_code=$?
         # xargs exits with status code 123 if some but not all of the
         # processes fail. We don't care if some of the files couldn't
```

Now we should generally try to strip slightly less from now on by
skipping identical files.

## Parting words

My initial guess of wrong library lookup paths was completely off. It
was another case of non-deterministic build causing major toolchain
breakage. I'm glad it was discovered so quickly after introduction.
[PR #246164](https://github.com/NixOS/nixpkgs/pull/246164) should fix it
for good.

Turns out `clang` and `gcc` generate a bit different code by default
around atomics.

Have fun!
