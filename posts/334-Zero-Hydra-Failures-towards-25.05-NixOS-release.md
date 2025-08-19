---
title: "Zero Hydra Failures towards 25.05 NixOS release"
date: May 1, 2025
---

It's May 1 and that means `NixOS-25.05` is almost
[there](https://github.com/NixOS/nixpkgs/issues/390768). Today the
release entered [`ZHF` phase](https://github.com/NixOS/nixpkgs/issues/390768)
(`Zero Hydra Failures`) where the main focus
is to squash as many build failures as possible before the release.

It's a good time to fix easy build failures or remove long broken
packages. <https://github.com/NixOS/nixpkgs/issues/390768> contains
detailed step-by-step to identify interesting packages.

## an example package fix

I usually try to fix at least one package during `ZHF`. This time I
picked [`hheretic`](https://hydra.nixos.org/build/294989234). The
failure does not look too cryptic:

```
...
checking for OpenGL support... no
configure: error: *** OpenGL not found!
```

To get a bit more detail I usually use `nix develop`:

```
$ nix develop -f. hheretic
$$ genericBuild
checking for OpenGL support... no
configure: error: *** OpenGL not found!
...
Running phase: buildPhase
no Makefile or custom buildPhase, doing nothing
...
```

Here I ran `genericBuild` to start a build process similar to what a
`nix build -f. hheretic` would do.
I got expected error (and a bit of extra stuff). Now I can peek at
`config.log` to check why `OpenGL` was not detected:

```
$ cat config.log
...
configure:5413: checking for OpenGL support
configure:5429: gcc -o conftest  -Wall -O2 -ffast-math -fomit-frame-pointer   conftest.c -lm  -Lno -lGL -lGLU >&5
conftest.c:30:10: fatal error: GL/gl.h: No such file or directory
   30 | #include <GL/gl.h>
      |          ^~~~~~~~~
compilation terminated.
```

The compiler does not see `GL/gl.h` header: a missing dependency. The
first thing I tried was this patch:

```diff
--- a/pkgs/by-name/hh/hheretic/package.nix
+++ b/pkgs/by-name/hh/hheretic/package.nix
@@ -4,6 +4,8 @@
   fetchFromGitHub,
   SDL,
   SDL_mixer,
+  libGL,
+  libGLU,
   autoreconfHook,
   gitUpdater,
 }:
@@ -27,6 +29,8 @@ stdenv.mkDerivation (finalAttrs: {
   buildInputs = [
     SDL
     SDL_mixer
+    libGL
+    libGLU
   ];

   strictDeps = true;
```

Running `nix build -f. hheretic` against it makes the package build
successfully. The change is proposed as a
[`PR#403458`](https://github.com/NixOS/nixpkgs/pull/403458) now.
As a bonus let's figure out when the package broke. In the
[history tab](https://hydra.nixos.org/job/nixos/trunk-combined/nixpkgs.hheretic.x86_64-linux)
we can see that:

- <https://hydra.nixos.org/build/292311010> was the last successful build
- <https://hydra.nixos.org/build/293013734> was the first failing build

Both links have `Inputs` tab where we can extract `nixpkgs` commits that
correspond to the build. That is enough for bisection:

```
$ git clone https://github.com/NixOS/nixpkgs
$ cd nixpkgs/
$ git bisect start 81b934af6399c868c693a945415bd59771f41718 316f79657ec153b51bee287fb1fb016b104af9ef
    Bisecting: 2949 revisions left to test after this (roughly 12 steps)
    [8490862820028f5c371ac0a7fde471990ff6ad80] evcc: 0.200.9 -> 0.201.0 (#390530)
$ git bisect run nix build -f. hheretic
running 'nix' 'build' '-f.' 'hheretic'
Bisecting: 1476 revisions left to test after this (roughly 11 steps)
...
Bisecting: 0 revisions left to test after this (roughly 1 step)
[e24f567a68111784e81cdda85e3784dd977f2ef8] Merge master into staging-next
running 'nix' 'build' '-f.' 'hheretic'
e47403cf2a2c76ae218bbf519c538b0ed419fa5f is the first bad commit
commit e47403cf2a2c76ae218bbf519c538b0ed419fa5f
Date:   Tue Mar 11 09:41:21 2025 +0100

    SDL: point alias to SDL_compat

 pkgs/top-level/all-packages.nix | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)
bisect found first bad commit
```

Looking at <https://github.com/NixOS/nixpkgs/commit/e47403cf2a2c76ae218bbf519c538b0ed419fa5f>
the `GitHub` UI says it corresponds to
[`PR#389106`](https://github.com/NixOS/nixpkgs/pull/389106).
Added
[the comment](https://github.com/NixOS/nixpkgs/pull/389106#issuecomment-2845845704)
there to get attention of relevant authors.

## parting words

`ZHF` event is a good way to contribute to `nixpkgs`. If you never did
but were waiting for an occasion it's a good one to try!

Have fun!
