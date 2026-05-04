---
title: "Zero Hydra Failures towards 26.05 NixOS release"
date: May 4, 2026
---

`NixOS-26.05` is around the corner!
[The schedule](https://github.com/NixOS/nixpkgs/issues/503391) says it
should happen around 25 May.

Today the release process entered `ZHF` phase. There most packages with
build failures are expected to be addresses by the release date.
<https://github.com/NixOS/nixpkgs/issues/516381> contains
detailed step-by-step to identify interesting packages.

This time `nixpkgs` is full of build failures related to `gcc-15`
changes (`-std=gnu23` switch).

## An example package fix

I usually try to fix at least one package during `ZHF`. Let's do it
again.

I picked `ior` package. [`hydra` says](https://hydra.nixos.org/job/nixpkgs/unstable/ior.x86_64-linux)
it is broken since January 2026. Most recent <https://hydra.nixos.org/build/327774029>
build failure has the build log:

```
  CC       libaiori_a-option.o
option.c: In function 'option_parse_token':
option.c:325:19: error: too many arguments to function 'fp'; expected 0, have 1
  325 |                   fp(arg);
      |                   ^~ ~~~
make[3]: *** [Makefile:1281: libaiori_a-option.o] Error 1
```

Looks like a typical `gcc-15` failure. Trying to reproduce it locally:

```
$ git clone --depth 1 https://github.com/NixOS/nixpkgs
$ cd nixpkgs
$ nix build --no-link -f. ior -L
...
ior> option.c:325:19: error: too many arguments to function 'fp'; expected 0, have 1
ior>   325 |                   fp(arg);
ior>       |                   ^~ ~~~
ior> make[3]: *** [Makefile:1281: libaiori_a-option.o] Error 1
```

Fails just the same! Before trying to fix it ourselves let's see if
upstream already happens to have a fix. I looked at the history of our
failed `option.c` file at <https://github.com/hpc/ior/commits/main/src/option.c>
and I was lucky! <https://github.com/hpc/ior/pull/525> is the committed
fix for our issue!

I applied it locally as:

```diff
--- a/pkgs/by-name/io/ior/package.nix
+++ b/pkgs/by-name/io/ior/package.nix
@@ -2,6 +2,7 @@
   lib,
   stdenv,
   fetchFromGitHub,
+  fetchpatch,
   mpi,
   perl,
   autoreconfHook,
@@ -19,6 +20,16 @@ stdenv.mkDerivation (finalAttrs: {
     hash = "sha256-WsfJWHHfkiHZ+rPk6ck6mDErTXwt6Dhgm+yqOtw4Fvo=";
   };
 
+  patches = [
+    # Fix gcc-15 build:
+    #   https://github.com/hpc/ior/pull/525
+    (fetchpatch {
+      name = "gcc-15.patch";
+      url = "https://github.com/hpc/ior/commit/526c5ad06695a91a27163c520ce3305109f50bef.patch";
+      hash = "sha256-HvbRMt2EcuO7kxLL9qKpozpNKEOmWuHkKQTSUhfU7/w=";
+    })
+  ];
+
   nativeBuildInputs = [
     autoreconfHook
     pkg-config
```

Tested the build:

```
$ nix build --no-link -f. ior -L
...
<all good>
```

And proposed the fix as <https://github.com/NixOS/nixpkgs/pull/516449>.

## Parting Words

If you never contributed to `nixpkgs` and are wondering how hard it is
do give `ZHF` a try! Chances are you can quickly grab a broken package
and make it work.

Have fun!
