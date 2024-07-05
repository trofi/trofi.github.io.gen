---
title: "Zero Hydra Failures towards 23.11 NixOS release"
date: November 8, 2023
root: "http://trofi.github.io"
---

## `ZHF`

The next `NixOS-23.11` will be released around the end of November.
Current development phase is called `Zero Hydra Failures` (`ZHF`): at
this time the main focus is to fix as many build failures in
`nixpkgs/master` repository as possible before the final release.

[Issue #265948](https://github.com/NixOS/nixpkgs/issues/265948) (and
[the discourse topic](https://discourse.nixos.org/t/zero-hydra-failure-23-11-edition/35103))
is the tracker where you can get hints on how to help fixing known
broken packages and review already proposed fixes.

It is a great time to contribute to `nixpkgs`!

To follow the tradition let's fix one package here.

## `newlib` example

[`trunk jobset`](https://hydra.nixos.org/jobset/nixpkgs/trunk) shows us
about ~3800 build failure. I picked [`newlib` failure](https://hydra.nixos.org/log/nv0q296sc06achvd7ljlrsn8x3qh8fg1-newlib-4.3.0.20230120.drv)
and will try to fix it. The install part fails there as:

```
...
installing
install flags: SHELL=/nix/store/lf0wpjrj8yx4gsmw2s3xfl58ixmqk8qa-bash-5.2-p15/bin/bash install
make[1]: Entering directory '/build/newlib-4.3.0.20230120'
/nix/store/lf0wpjrj8yx4gsmw2s3xfl58ixmqk8qa-bash-5.2-p15/bin/bash ./mkinstalldirs /nix/store/1wxhiz8jkyff6chkwp89vy85qlgvi7ij-newlib-4.3.0.20230120 /nix/store/1wxhiz8jkyff6chkwp89vy85qlgvi7ij-newlib-4.3.0.20230120
make[2]: Entering directory '/build/newlib-4.3.0.20230120/etc'
make[3]: Entering directory '/build/newlib-4.3.0.20230120/etc'
make[3]: Nothing to be done for 'install-exec-am'.
make[3]: Nothing to be done for 'install-data-am'.
make[3]: Leaving directory '/build/newlib-4.3.0.20230120/etc'
make[2]: Leaving directory '/build/newlib-4.3.0.20230120/etc'
make[1]: Nothing to be done for 'install-target'.
make[1]: Leaving directory '/build/newlib-4.3.0.20230120'
$out is empty
```

I have no idea why the build fails. Let's find out the hard way.

[The build tab](https://hydra.nixos.org/build/239066832) tells us
that last successful build of `newlib` was around `2023-06-18`
on `d9895270b775226e0fdabd7937af2d236abe4eb2` `nixpkgs` input. And first
failed commit was `8277b539d371bf4308fc5097911aa58bfac1794f` around
`2023-07-01`.

Running bisect:

```
$ git bisect start 8277b539d371bf4308fc5097911aa58bfac1794f d9895270b775226e0fdabd7937af2d236abe4eb2
$ git bisect run nix build -f. newlib

commit cf1b7c4d5c027837e71d284a838fbeb05b3fcb7f
Date:   Sat Jun 24 01:13:17 2023 +0200

    newlib: fix build of nano variant on non-ARM architectures
...
```

The full diff of this
[commit](https://github.com/NixOS/nixpkgs/commit/cf1b7c4d5c027837e71d284a838fbeb05b3fcb7f)
is small an readable:

```diff
--- a/pkgs/development/misc/newlib/default.nix
+++ b/pkgs/development/misc/newlib/default.nix
@@ -73,10 +73,12 @@ stdenv.mkDerivation (finalAttrs: {
       cd $out${finalAttrs.passthru.libdir}

       for f in librdimon.a libc.a libg.a; do
-        cp "$f" "''${f%%\.a}_nano.a"
+        # Some libraries are only available for specific architectures.
+        # For example, librdimon.a is only available on ARM.
+        [ -f "$f" ] && cp "$f" "''${f%%\.a}_nano.a"
       done
     )
-  '';
+  '' + ''[ "$(find $out -type f | wc -l)" -gt 0 ] || (echo '$out is empty' 1>&2 && exit 1)'';

   passthru = {
     incdir = "/${stdenv.targetPlatform.config}/include";
```

In our case of `newlib` (not `newlib-nano`) the only change is the
addition of `[ "$(find $out -type f | wc -l)" -gt 0 ] || (echo '$out is empty' 1>&2 && exit 1)`
line. It causes build to fail if `$out` is empty. The `$out` was always
empty for `newlib.x86_64-linux`. Normally the `newlib` output contains
something only for bare-metal targets like
`pkgsCross.x86_64-embedded.newlib`.

Thus the fix is to constrain `newlib` to only those targets:

```diff
--- a/pkgs/development/misc/newlib/default.nix
+++ b/pkgs/development/misc/newlib/default.nix
@@ -96,5 +96,9 @@ stdenv.mkDerivation (finalAttrs: {
     # COPYING.NEWLIB
     # COPYING3
     license = licenses.gpl2Plus;
+    # newlib frequently does ont supply any files on hosted targets like
+    # x86_64-unknown-linux-gnu: https://hydra.nixos.org/log/nv0q296sc06achvd7ljlrsn8x3qh8fg1-newlib-4.3.0.20230120.drv
+    # Let's constrain `newlib` package to bare-metal alone.
+    broken = !stdenv.hostPlatform.isNone;
   };
 })
```

This change was proposed as [PR #266268](https://github.com/NixOS/nixpkgs/pull/266268).

## Parting words

Fixing package breakages are usually easier if the package used to work
at some point before. Otherwise we can always mark packages broken and
schedule them for removal.

Have fun!
