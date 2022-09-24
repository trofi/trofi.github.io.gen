---
title: "Shrinking closure example"
date: September 24, 2022
---

Sometimes I check `nixpkgs` packages I use for unexpected
development-only runtime dependencies. I do it mostly to shrink download
sizes for things I update frequently.

I developed a few hacks to find things quickly. The primary hack is to
grep dependency graph of an executable-only package (say, a game) for
`-dev` packages in it's runtime closure. Here is an example for `fheroes2`
package:

```
$ nix-store -q --graph $(nix-build -A fheroes2) | grep -P -- '-dev.*->' | grep -vP -- '->.*-dev'
"4bdanp07rax5mazgjzgdwx61sf6p01qc-SDL2-2.0.22-dev" -> "nj09vl0pzc41sn4wh7q2vlppmkv3dhiy-SDL2_mixer-2.0.4" [color = "burlywood"];
...
```

Here we see that `SDL2.dev` package is pulled into `SDL2_mixer.out`
runtime closure. It's a bug.

More interesting hack is to grep full runtime closure for files that
are cleary development-only: `C` ehader files, `pkg-config` files and so
on. Here is a `grep` example again for `fheroes2`:

```
$ find $(nix path-info -r $(nix-build -A fheroes2)) | grep -P [.]h$ | shuf | unnix | nl | tail -n 2
  1301  /<<NIX>>/libnfnetlink-1.0.2/include/libnfnetlink/libnfnetlink.h
  1302  /<<NIX>>/xorgproto-2021.5/include/X11/extensions/dpmsproto.h
```

Here we see that `xorgproto` (header-only package) and `libnfnetlink`
(package without a separate `.dev` output) pull in development headers
into our previous game. Both are probably unintended and worth a fix.

To get rid of the dependencies I usually add `dev` outputs to libraries
without `dev` output like a recent
[libfido2 example](https://github.com/NixOS/nixpkgs/pull/191775/commits/d04acb8a96c2ae37dd4ff58db65dedfab8d3d79f):

```diff
--- a/pkgs/development/libraries/libfido2/default.nix
+++ b/pkgs/development/libraries/libfido2/default.nix
@@ -29,6 +29,8 @@ stdenv.mkDerivation rec {

   propagatedBuildInputs = [ openssl ];

+  outputs = [ "out" "dev" "man" ];
+
   cmakeFlags = [
     "-DUDEV_RULES_DIR=${placeholder "out"}/etc/udev/rules.d"
     "-DCMAKE_INSTALL_LIBDIR=lib"
```

Sometime I have to explicitly change the package to not retain
build-only dependencies. Here is a recent
[freedroidrpg example](https://github.com/NixOS/nixpkgs/pull/191810/commits/02ba9a3d60c6c45e1df45714a2a3db714eed9c18):

```
Do not embed paths to build-only depends (-I...SDL2-dev and friends)
into savefile lua comments.
--- a/src/savestruct_internal.c
+++ b/src/savestruct_internal.c
@@ -486,8 +486,8 @@ void save_game_data(struct auto_string *strout)
        autostr_append(strout,
                "SAVEGAME: %s %s %s;sizeof(tux_t)=%d;sizeof(enemy)=%d;sizeof(bullet)=%d;MAXBULLETS=%d\n",
                SAVEGAME_VERSION, SAVEGAME_REVISION, VERSION, (int)sizeof(tux_t), (int)sizeof(enemy), (int)sizeof(bullet), (int)MAXBULLETS);
-       autostr_append(strout, "BUILD_CFLAGS: %s\n", BUILD_CFLAGS);
-       autostr_append(strout, "BUILD_LDFLAGS: %s\n", BUILD_LDFLAGS);
+       autostr_append(strout, "BUILD_CFLAGS: %s\n", "<hidden>");
+       autostr_append(strout, "BUILD_LDFLAGS: %s\n", "<hidden>");
        autostr_append(strout, "VERSION: %s\n", freedroid_version);
        autostr_append(strout, "--]]\n");
```

Sometimes you might also need to add `propagatedBuildInputs = ...` to make
headers-only `dev` output self-contained.

Is it worth the hassle? If feels like development headers don't take
that much space anyway. It's true that some packages have tiny overhead.
But things add up quickly. For example
[freedroidrpg PR](https://github.com/NixOS/nixpkgs/pull/191810) shrinks
runtime closure from `808MB` down to `450MB` (44% reduction). While
[fheroes2 RPs](https://github.com/NixOS/nixpkgs/issues/191770#issuecomment-1250247308)
shrunk runtime closure from `622MB` down to `557MB` (11% reduction).

These are just two examples I found in 5 minutes. There are many more
packages you can fix in `nixpkgs`! Give it a try!

Have fun!
