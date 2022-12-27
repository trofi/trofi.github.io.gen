---
title: "xlibsWrapper yak"
date: December 27, 2022
---

During one of the raids on [stale nixpkgs packages](/posts/272-peeking-at-stale-nixpkgs-packages.html)
I found `Xaw3d` package which was present in two forms in `nixpkgs`:
`Xaw3d` and `xorg.libXaw3d`. Both were based on `libXaw3d` `Xorg`
package of the same version and used different definitions of the
package.

`xorg.libXaw3d` was used by a single broken `timedoctor` package.
Thus it ws easy to just delete both:

- `timedoctor` removal: [PR 195251](https://github.com/NixOS/nixpkgs/pull/195251)
- `xorg.libXaw3d` removal: [PR 195254](https://github.com/NixOS/nixpkgs/pull/195254)

Duplicate packages are very confusing. New namespaces like `xorg.*` make
it very easy to introduce subtly different packages.

A while later I attempted to patch `xorg.libXfont2` package via an
override locally:

```nix
final: prev: {
  xorg = prev.xorg.overrideScope' (f: p: {
    libXfont2 = p.libXfont2.overrideAttrs (oa: {
      patches = (oa.patches or []) ++ [
        ../libXfont2/nofollow.patch
      ];
    });
  });
}
```

That was supposed to work. It didn't: `pulseaudio` expression became
invalid and complained about missing `xorg.xlibsWrapper` attribute.
Despite the fact I seemingly did not touch it in my expression.

Here is the `xorg` attribute definition:

```nix
   # somewhere in pkgs/top-level/all-packages.nix
   # ...
   xorg = recurseIntoAttrs ((lib.callPackageWith __splicedPackages ../servers/x11/xorg {
   }).overrideScope' (lib.callPackageWith __splicedPackages ../servers/x11/xorg/overrides.nix {
     inherit (darwin.apple_sdk.frameworks) ApplicationServices Carbon Cocoa;
     inherit (darwin.apple_sdk.libs) Xplugin;
     inherit (buildPackages.darwin) bootstrap_cmds;
     udev = if stdenv.isLinux then udev else null;
     libdrm = if stdenv.isLinux then libdrm else null;
     abiCompat = config.xorg.abiCompat # `config` because we have no `xorg.override`
       or (if stdenv.isDarwin then "1.18" else null); # 1.19 needs fixing on Darwin
   }) // { inherit xlibsWrapper; } );
```

Note that unusual `// { inherit xlibsWrapper; }` last minute attribute
addition. It injects `xlibsWrapper` attribute after `xorg` was fully
constructed with all it's overrides. Attempt to redefine `xorg`
attribute (as I did in an overlay above) throws `xlibsWrapper` attribute
away. It has to be added back explicitly. I would say it's unexpected.

More interestingly not every package used `xorg.xlibsWrapper` form to
refer to the thing! Most packages used `xlibsWrapper` directly. Thus
I removed `xorg.xlibsWrapper` in favour of a global one in
[PR 194044](https://github.com/NixOS/nixpkgs/pull/194044).
The fix had to adjust only 5 packages (`gnome2.libgnomeui`, `pulseaudio`,
`ted`,`vlc`,`xine-ui`) and turned `xorg` into a proper overridable
scope:

```diff
--- a/pkgs/top-level/all-packages.nix
+++ b/pkgs/top-level/all-packages.nix
@@ -24104,21 +24104,21 @@ with pkgs;
   # have created a cycle.
   xorg = recurseIntoAttrs ((lib.callPackageWith __splicedPackages ../servers/x11/xorg {
   }).overrideScope' (lib.callPackageWith __splicedPackages ../servers/x11/xorg/overrides.nix {
     inherit (darwin.apple_sdk.frameworks) ApplicationServices Carbon Cocoa;
     inherit (darwin.apple_sdk.libs) Xplugin;
     inherit (buildPackages.darwin) bootstrap_cmds;
     udev = if stdenv.isLinux then udev else null;
     libdrm = if stdenv.isLinux then libdrm else null;
     abiCompat = config.xorg.abiCompat # `config` because we have no `xorg.override`
       or (if stdenv.isDarwin then "1.18" else null); # 1.19 needs fixing on Darwin
-  }) // { inherit xlibsWrapper; } );
+  }));
```

So much better now.

And then I looked at `xlibsWrapper` definition out of curiosity:

```nix
  # somewhere in pkgs/top-level/all-packages.nix
  # ...
  # Avoid using this. It isn't really a wrapper anymore, but we keep the name.
  xlibsWrapper = callPackage ../development/libraries/xlibs-wrapper {
    packages = [
      freetype fontconfig xorg.xorgproto xorg.libX11 xorg.libXt
      xorg.libXft xorg.libXext xorg.libSM xorg.libICE
    ];
  };
```

```
# somewhere in pkgs/development/libraries/xlibs-wrapper/default.nix
{lib, stdenv, packages}:

stdenv.mkDerivation {
  name = "xlibs-wrapper";

  dontBuild = true;

  installPhase = "mkdir -p $out";
  unpackPhase = "sourceRoot=.";

  propagatedBuildInputs = packages;

  preferLocalBuild = true;

  # For compatability with XFree86.
  passthru.buildClientLibs = true;

  meta = {
    platforms = lib.platforms.unix;
  };
}
```

The above says that:

- it's a deprecated package (it got "deprecated" comment in 2015)
- it's not a wrapper. Normally `wrapper` implied binary shell wrapper
  script in `nixpkgs`. This one is nothing like that.
- it's just an alias for build-time dependencies for some of Xorg libraries.

`xlibsWrapper` is a remnant of the far past when Xorg became modular
(around 7.0 version in [2005](https://github.com/NixOS/nixpkgs/commit/ea95a0509ecde0208d26cea272bd8fc5136dd4be))
where it's single tarball release was split into ~200 smaller packages.
`nixpkgs` kept this transitional package to ease porting to the new set
of packages.

To delete `xlibsWrapper` completely I filed
[PR 194054](https://github.com/NixOS/nixpkgs/issues/194054) to track and
update packages to set of actually used libraries one by one. 3 months
(and 80 PRs) later I can finaly say that all users were moved to finer
grained set of libraries!

`diffoscope` helped me a lot while working on the transition: if binaries
before and after the change look the the same then chances are high I did
not break any users.

Another yak became a bit less hairy.

There is still a lot to do: I think it's time to remove `xorg.*`
attribute set and merge individual packages back into the top-level
attribute set. This will turn `xorg.*` into normal packages and will
allow updating them just like anything else. I filed
[Issue 207978](https://github.com/NixOS/nixpkgs/issues/207978) to track
the effort.

## Parting words

`nixpkgs` is full of bits with accidental complexity like `xlibsWrapper`
or autogenerated set of `xorg` packages. Or long deprecated packages
like `llvm-5` (it still has a few reverse dependencies believe it or
not). They are usually easy to fix, just take some effport to sort out
all the numerous users.

Fun fact: it takes about 3 months of slow pace to sort out 80 packages.
Something like 1 package a day. Sounds like a good estimate of large-scale
changes.

Have fun!
