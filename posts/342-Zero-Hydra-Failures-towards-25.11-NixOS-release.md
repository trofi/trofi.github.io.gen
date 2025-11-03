---
title: "Zero Hydra Failures towards 25.11 NixOS release"
date: November 3, 2025
---

It is November again! The usual plan is to have a `NixOS-.25.11` release
on 30th ([full schedule](https://github.com/NixOS/nixpkgs/issues/443568)).

Yesterday the schedule got to
[`ZHF phase`](https://github.com/NixOS/nixpkgs/issues/457852) where no
major changes are accepted to `master` branch and the focus is on fixing
build failures

It's a good time to fix easy build failures or remove long broken
packages. <https://github.com/NixOS/nixpkgs/issues/457852> contains
detailed step-by-step to identify interesting packages.

This year `nixpkgs` has especially large list of failures to sort out.
It feels like most build failures are either `cmake-4` or `qt-6.10`
related.

## an example package fix

Let's try to fix a single package for `ZHF`. I'll pick the
[`diskscan`](https://hydra.nixos.org/build/310538459). It's build log
is typical of `cmake-4` failure:

```
...
CMake Error at CMakeLists.txt:1 (cmake_minimum_required):
  Compatibility with CMake < 3.5 has been removed from CMake.

  Update the VERSION argument <min> value.  Or, use the <min>...<max> syntax
  to tell CMake that the project requires at least <min> but has been updated
  to work with policies introduced by <max> or earlier.

  Or, add -DCMAKE_POLICY_VERSION_MINIMUM=3.5 to try configuring anyway.
```

I know little to nothing about `cmake`. But this failure is the result
of `cmake` dropping support for pre-`cmake-3.5` behavior. Chances are
upstream already fixed the problem and we can use the patch as is.

Let's find the source repository by inspecting the package's definition:

```
$ EDITOR=cat nix edit -f '<nixpkgs>' diskscan
```

```nix
{
  lib,
  stdenv,
  fetchFromGitHub,
  cmake,
  ncurses,
  zlib,
}:

stdenv.mkDerivation rec {
  pname = "diskscan";
  version = "0.21";

  src = fetchFromGitHub {
    owner = "baruch";
    repo = "diskscan";
    rev = version;
    sha256 = "sha256-2y1ncPg9OKxqImBN5O5kXrTsuwZ/Cg/8exS7lWyZY1c=";
  };

  buildInputs = [
    ncurses
    zlib
  ];

  nativeBuildInputs = [ cmake ];

  meta = with lib; {
    homepage = "https://github.com/baruch/diskscan";
    description = "Scan HDD/SSD for failed and near failed sectors";
    platforms = with platforms; linux;
    maintainers = with maintainers; [ peterhoeg ];
    license = licenses.gpl3;
    mainProgram = "diskscan";
  };
}
```

Easy! <https://github.com/baruch/diskscan> displayed nothing related to
`cmake-4` fix. Let's write one! Trying to reproduce the failure locally
against upstream `master` branch:

```
$ git clone https://github.com/baruch/diskscan
$ cd diskscan

$ nix build --impure --expr 'with import <nixpkgs> {}; diskscan.overrideAttrs (oa: { src = builtins.fetchGit ./.; })' -L
...
diskscan> CMake Error at CMakeLists.txt:1 (cmake_minimum_required):
diskscan>   Compatibility with CMake < 3.5 has been removed from CMake.
```

Yay! Same failure! For this particular case the fix is trivial:

```diff
--- a/CMakeLists.txt
+++ b/CMakeLists.txt
@@ -1,4 +1,4 @@
-cmake_minimum_required(VERSION 3.0.2)
+cmake_minimum_required(VERSION 3.10)
 project(diskscan
         VERSION 0.19)

```

Testing the fix:

```
$ nix build --impure --expr 'with import <nixpkgs> {}; diskscan.overrideAttrs (oa: { src = builtins.fetchGit ./.; })' -L
warning: Git tree '/tmp/diskscan' is dirty
# done!

$ find result/
result/
result/bin
result/bin/diskscan
result/share
result/share/man
result/share/man/man1
result/share/man/man1/diskscan.1.gz
```

You can run the result and see if it does what's expected. I proposed
this trivial fix upstream as [`PR#77`](https://github.com/baruch/diskscan/pull/77).

Now we can use that to craft the `nixpkgs` fix! Let's check if the bug
is still there:

```
$ git clone https://github.com/NixOS/nixpkgs
$ cd nixpkgs

$ nix build -f. diskscan -L
...
diskscan> CMake Error at CMakeLists.txt:1 (cmake_minimum_required):
diskscan>   Compatibility with CMake < 3.5 has been removed from CMake.
```

Still there. Crafting the patch against `nixpkgs`:

```diff
--- a/pkgs/by-name/di/diskscan/package.nix
+++ b/pkgs/by-name/di/diskscan/package.nix
@@ -2,6 +2,7 @@
   lib,
   stdenv,
   fetchFromGitHub,
+  fetchpatch,
   cmake,
   ncurses,
   zlib,
@@ -18,6 +19,16 @@ stdenv.mkDerivation rec {
     sha256 = "sha256-2y1ncPg9OKxqImBN5O5kXrTsuwZ/Cg/8exS7lWyZY1c=";
   };

+  patches = [
+    # cmake-4 support:
+    #   https://github.com/baruch/diskscan/pull/77
+    (fetchpatch {
+      name = "cmake-4.patch";
+      url = "https://github.com/baruch/diskscan/commit/6e342469dcab32be7a33109a4d394141d5c905b5.patch?full_index=1";
+      hash = "sha256-05ctYPmGWTJRUc4aN35fvb0ITwIZlQdIweH7tSQ0RjA=";
+    })
+  ];
+
   buildInputs = [
     ncurses
     zlib
```

And testing the build:

```
$ nix build -f. diskscan -L
...

$ find result/
result/
result/bin
result/bin/diskscan
result/share
result/share/man
result/share/man/man1
result/share/man/man1/diskscan.1.gz
```

All good! Proposed the fix as
[`PR#458258`](https://github.com/NixOS/nixpkgs/pull/458258).

## parting words

If you are thinking to contribute to `nixpkgs` and never did `ZHF` is a
good time to start!

`cmake-4` set of failures has it's own seemingly infinite list of
[failures](https://github.com/NixOS/nixpkgs/issues/445447) waiting to be
fixed just like the example above.

Have fun!
