---
title: "Zero Hydra Failures towards 22.11 NixOS release"
date: November 08, 2022
---

The end of November is near and that means `NixOS` will get a release
soon. One of the late stages is Zero Hydra Failures phase: there everyone
squashes most of known build failures in `nixpkgs/master` before branching
off new `22.11` release.

<https://github.com/NixOS/nixpkgs/issues/199919> tracks the effort and
has a few hints on how to locate all known build failures in various
places. So far there is a few thousands build failures in `nixpkgs/master`.

I had a chance to fix [zsnes build failure](https://github.com/NixOS/nixpkgs/pull/199932).
As you can see it's a trivial fix. And we need a lot of fixes like these.
If you would like to help `nixpkgs` and upstream projects then please
join the effort of fixing those! Thank you in advance :)

Below I'll run through an example build fix in more detail.

I usually start from <https://hydra.nixos.org/jobset/nixpkgs/trunk>
periodic builder against `nixpkgs/master`. There I pick most recent
evaluation and navigate to `Still failing jobs` tab. From there I pick
failing package I like and check the build log. Apart from the build
log there is also a commit at which the package was built successully
last time. That usually helps in bisection of exact commit that broke
the package. Useful when you have no idea why the package is unhappy.

Let's pick `synfigstudio-1.5.1` as a broken example. It's build failure
is hiding in <https://hydra.nixos.org/build/197644988>. It's last
successful build is <https://hydra.nixos.org/build/194567109> (`Inputs`
tab says it's 667199140080a656d0be0d9c91b4bbac8828959f upstream commit
around end of September 2022).

[Build log](https://hydra.nixos.org/log/fd916xc57cfiwa3b7nm9x1i0f80czszx-synfigstudio-1.5.1.drv)
complains about missing `intltoolize`:

```
Makefile.am: installing './INSTALL'
Makefile.am: installing './COPYING' using GNU General Public License v3 file
Makefile.am:     Consider adding the COPYING file to the version control system
Makefile.am:     for your code, to avoid questions about which license your project uses
src/gui/Makefile.am: installing 'config/depcomp'
parallel-tests: installing 'config/test-driver'
autoreconf: Leaving directory '.'
configuring
*** No intltoolize found, please install the intltool package ***
```

Trying a trivial fix:

```diff
--- a/pkgs/applications/graphics/synfigstudio/default.nix
+++ b/pkgs/applications/graphics/synfigstudio/default.nix
@@ -103,6 +103,7 @@ stdenv.mkDerivation {
     pkg-config
     autoreconfHook
     gettext
+    intltool
     wrapGAppsHook
   ];
   buildInputs = [
```

```
$ nix build -f . synfigstudio
...
ok!
```

Proposed fix against `nixpkgs` as <https://github.com/NixOS/nixpkgs/pull/200095>.
That was easy.

Now out of curiosity let's see what previous change did break
`synfigstudio`:

```
$ git clone https://github.com/NixOS/nixpkgs
$ cd nixpkgs

$ git bisect start origin/master 667199140080a656d0be0d9c91b4bbac8828959f
Bisecting: 4429 revisions left to test after this (roughly 12 steps)
[25b3b9b1b2af029d1a6db918d8fdb5560724681b] Merge staging-next into staging

$ git bisect run nix build -f. synfigstudio
running  'nix' 'build' '-f.' 'synfigstudio'
...

commit 0734f54ef262ad642eec1166a416bae86779ed9f
Date:   Sun Sep 25 05:49:25 2022 +0300

    treewide: move pkg-config, autoreconfHook, intltool to nativeBuildInputs

    found with nixpkgs-lint
```

If we look at that patch it removed `intltool` from `buildInputs` (`HOST`
depends) instead of moving it to `nativeBuildInputs` (`BUILD` depends):

```diff
--- a/pkgs/applications/graphics/synfigstudio/default.nix
+++ b/pkgs/applications/graphics/synfigstudio/default.nix
@@ -107,21 +107,20 @@ stdenv.mkDerivation {
   ];
   buildInputs = [
     ETL
     synfig
     boost
     cairo
     glibmm
     gtk3
     gtkmm3
     imagemagick
-    intltool
     libjack2
     libsigcxx
     libxmlxx
     mlt
     gnome.adwaita-icon-theme
     openexr
     fftw
   ];

   enableParallelBuilding = true;
```

Simple bug downstream-only bug. Nothing to report upstream.

Have fun and happy fixing!
