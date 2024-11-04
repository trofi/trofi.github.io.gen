---
title: "Zero Hydra Failures towards 24.11 NixOS release"
date: November 4, 2024
---

`ZHF` (or Zero Hydra Failures) is the time when most build failures are
squashed before final `NixOS-24.11` release
(see [full release schedule](https://github.com/NixOS/nixpkgs/issues/352882)).

To follow the tradition let's fix one bug for `ZHF`.

I picked [`xorg.libAppleWM`](https://hydra.nixos.org/build/276690936) build
failure. It's not a very popular package.

The failure looks trivial:

```
make[2]: Entering directory '/build/libapplewm-be972ebc3a97292e7d2b2350eff55ae12df99a42/src'
  CC       applewm.lo
gcc: error: unrecognized command-line option '-iframeworkwithsysroot'
```

The build was happening for `x86_64-linux` target. While this package
is `MacOS`-specific: it uses Darwin APIs and links to it's libraries
directly. No reason to try to build it on `x86_64-linux`.

The fix is to constrain the package to `darwin` targets (the default
platforms for `xorg` packages is `unix`):

```diff
--- a/pkgs/servers/x11/xorg/overrides.nix
+++ b/pkgs/servers/x11/xorg/overrides.nix
@@ -171,6 +171,9 @@ self: super:
   libAppleWM = super.libAppleWM.overrideAttrs (attrs: {
     nativeBuildInputs = attrs.nativeBuildInputs ++ [ autoreconfHook ];
     buildInputs =  attrs.buildInputs ++ [ xorg.utilmacros ];
+    meta = attrs.meta // {
+      platforms = lib.platforms.darwin;
+    };
   });

   libXau = super.libXau.overrideAttrs (attrs: {
```

This fix is now known as
[PR#353618](https://github.com/NixOS/nixpkgs/pull/353618).

## Parting words

I picked very lazy example of a broken package.
<https://github.com/NixOS/nixpkgs/issues/352882> contains more links and
hints on how to find and fix known breakages.

As usual contributing towards `ZHF` is very easy. Give it a try!

Have fun!
