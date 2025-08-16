---
title: "nix and guix for Gentoo in 2023"
date: May 19, 2023
---

Another year has passed since I shared anything related to
[`::nix-guix`](https://github.com/trofi/nix-guix-gentoo/) Gentoo overlay.

As usual the overlay still ships latest `nix-2.15.0` and `guix-1.4.0`
packages.

## Overlay location changes

The overlay installation procedure
[changed slightly](https://github.com/trofi/nix-guix-gentoo/#enabling-the-overlay)
and does not rely on `layman` or `eselect repository` anymore.

If you are an existing user of `layman` setup then please consider
removing and re-syncing already created overlay. It should be a matter
of running something like:

```
# edit /etc/portage/repos.conf/nix-guix.conf
# rm -rfv /var/db/repos/nix-guix
# emerge --sync
```

Otherwise, you are risking using stale overlay mirrored by Gentoo
infrastructure at <https://github.com/gentoo-mirror/nix-guix>. It has
a few build problems like missing `gcc-13` fixes. They typical symptom
of using stale overlay is `rapidcheck`
[build failure](https://github.com/trofi/nix-guix-gentoo/issues/35):

```
/var/tmp/portage/dev-cpp/rapidcheck-0_pre20230113/work/rapidcheck-0_pre20230113/include/rapidcheck/detail/Utility.h:87:8: error: 'uint64_t' does not name a type
   87 | inline uint64_t avalanche(uint64_t x) {
      |        ^~~~~~~~
/var/tmp/portage/dev-cpp/rapidcheck-0_pre20230113/work/rapidcheck-0_pre20230113/include/rapidcheck/detail/Utility.h:6:1: note: 'uint64_t' is defined in header '<cstdint>'; did you forget to '#include <cstdint>'?
    5 | #include <limits>
  +++ |+#include <cstdint>
    6 |
```

It was [fixed a while ago](https://github.com/trofi/nix-guix-gentoo/commit/64edf8fe00c3fb3bcfc53aeb9287a4d3fb7cc882)
in the overlay. Make sure you have the correct sync URL that points:
<https://github.com/trofi/nix-guix-gentoo/>.

I hope that the fact of use of stale URL by `emerge --sync` will be
fixed in <https://bugs.gentoo.org/905869>. Or at least sync errors will
be exposed to users in a more intuitive form. Removing stale broken
mirrors would be another option.

## Other changes

We had 5 overlay contributors over the past year:

```
Artemis Everfree
Huang Rui
Maciej Barć
ns
Sergei Trofimovich
```

People delivered various fixes and improvements:

- add basic support for `clang`-based system profiles
- improve compatibility with systems where `/bin/sh` is `dash`
- `README.md` installation improvements
- dependency improvements on non-`amd64` systems
- CI workflows to catch simple syntax errors in ebuilds
- numerous `nix` version updates to trail latest upstream releases
- switch from `user.eclass` to `acct-user.eclass` to allow users to
  tweak default groups for builders.
- drop `USE=s3` support on `nix` to sidestep problems dealing with stale
  `aws-sdk-cpp` package
- restore lost by accident user's `CXXFLAGS=` support for `nix`
- document `ENV_UNSET` workaround for cases when global variables
  introduce build impurity

One of the items above had unusually large source of bugs.

## clang profiles related bugs

Gentoo recently added `clang`-based set of experimental profiles:

```
$ eselect profile list | grep clang
  [26]  default/linux/amd64/17.1/clang (exp)
  [27]  default/linux/amd64/17.1/systemd/clang (exp)
  [28]  default/linux/amd64/17.1/systemd/clang/merged-usr (exp) *
  [33]  default/linux/amd64/17.0/musl/clang (exp)
```

Of course being experimental does not stop users from trying to use the
profiles for all sorts of use cases. I found out about the profiles from
a bug report where `nix`
[failed to build](https://github.com/trofi/nix-guix-gentoo/issues/28)
on one of them.

### missing 'long double' helpers in runtime libraries

The symptom was a link failure around obscure functions like
`__unordtf2`:

```
ld.lld: error: undefined symbol: __unordtf2
>>> referenced by printf_fphex.o:(__printf_fphex) in archive /usr/lib/gcc/x86_64-pc-linux-gnu/12/../../../../lib64/libc.a
>>> referenced by printf_fphex.o:(__printf_fphex) in archive /usr/lib/gcc/x86_64-pc-linux-gnu/12/../../../../lib64/libc.a
>>> referenced by printf_fp.o:(__printf_fp_l) in archive /usr/lib/gcc/x86_64-pc-linux-gnu/12/../../../../lib64/libc.a
```

This kind of errors only ever happens when you mix the code compiled with
`gcc` and `clang`. `gcc` sometimes emits helper functions that it
expects to satisfy with `libgcc.a`. Those symbols are usually related to
handling of non-standard types like `long double`. But otherwise could
be anything that is worth sharing by the code generator.

It would not normally be a problem. But `glibc` provides `printf()`
function that supports any type that compiler tries to throw at
it. And even that is not a problem: as long as building and linking is
done by the same toolchain (like for `libc.so.6`) then compiler driver
provides all the needed libraries.

The problem happens when compiler and linker are different
implementations. One of such cases is static linking: currently `glibc`
is built with `gcc` even on `clang` profiles in Gentoo. But the vast
majority of `::gentoo` is built with `clang` including static `busybox`.

As a result static linking is effectively broken on `clang` profiles
today.

One of the fixes would be to switch `glibc` to be built with `clang`
just like the rest of packages (I believe `glibc` upstream does support
`clang` nowadays). Another workaround would be to disable support of
those non-standard types at least in static `glibc`. Yet another one
would be to implement `gcc`-specific library calls in `clang` runtime so
both could be interchangeable. That would be the best fix if `clang`
strives to generate output binary-compatible with `gcc` output.

So far none of the above is implemented in Gentoo.
<https://bugs.gentoo.org/899582> tracks the problem.

Thus, to make `nix` work I had to work around in statically built
`busybox` instance used by `nix-daemon` for initial build environments.

I took the path of supplying (unusable) stubs for missing symbols as:

```c
#include <stdio.h>
#include <stdlib.h>

/* Not a real implementation.
 * Just a few stubs for llvm-libunwind to be complete enough for
 * busybox to be able to link against libc.a's printf().
 */

__attribute__((noreturn))
static void die(const char * func)
{
    fprintf(stderr, "%s not implemented.", func);
    fprintf(stderr, "Please report at https://github.com/trofi/nix-guix-gentoo/issues\n");
    exit(1);
}

int __unordtf2 (long double a, long double b) { die(__func__); }
int __letf2 (long double a, long double b) { die(__func__); }
long double __multf3 (long double a, long double b) { die(__func__); }
long double __addtf3 (long double a, long double b) { die(__func__); }
```

Using this file as part of linkage process converts missing symbols from
link-time to run-time failures. `busybox` does not really use these
symbols thus it should be a safe hack.

That fixed `nix` build on `clang`-based profiles.

### `guix` and `llvm-strip`

Having set up `clang`-based Gentoo chroot to explore the problem above
I ran the test for all the `::nix-guix` packages. And sure enough `guix`
failed to build as well. Except that this time the failure was a lot
more obscure:

```
# emerge -1 guix
...
bytevector-u8-ref: Argument 2 out of range: 185581
```

Something very low-level broke in `guile` or it's libraries.

I ignored the failure for a few months until I looked up if others
encountered the same error. And indeed someone did! They solved it by
disabling stripping of `guile`. It was a great hint: rebuilding all of
`dev-scheme/*` with `FEATURES="-splitdebug nostrip"` repaired `guix` for
me as well.

`guile` uses `ELF` format to store it's compiled bytecode in
`/usr/lib/guile/`:

```
$ file /usr/lib64/guile/3.0/ccache/rnrs.go
/usr/lib64/guile/3.0/ccache/rnrs.go:
  ELF 64-bit LSB shared object,
  no machine,
  version 1 (embedded),
  dynamically linked,
  with debug_info,
  not stripped
```

The caveat is that it's a "no machine" ELF file as it has no native
code. As a result an attempt to strip these files can break their
internal structure if not done right. `binutils-strip` does not break
the files (probably by luck?). `llvm-strip` is not as lucky.

The workaround was to sprinkle around the change below all over the
`dev-scheme*/*` and `guix`:

```bash
# add to dev-scheme/guile-zlib
src_install() {
       default

       # Workaround llvm-strip problem of mangling guile ELF debug
       # sections: https://bugs.gentoo.org/905898
       dostrip -x "/usr/$(get_libdir)/guile"
}
```

It inhibits stripping of `*.go` files. Ideally one should not run
`strip` on object files not compatible to the default `strip` target.

Or `guile` could have encoded enough information for `llvm-strip` to
keep files working. Something to explore in future.

Disabling `*.go` stripping was enough to get `guix` running on
`clang`-based profiles.

## Parting words

`nix` and `guix` can still be used in Gentoo to play with these fancy
package managers. With static linkage and stripping workarounds those
can be used in more environments now.

Have fun!
