---
title: "listing all nixpkgs packages"
date: December 30, 2023
---

## Intro

`nixpkgs` provides [a lot of packages](https://repology.org/repository/nix_unstable).
Today `repology.org` says it's `106937` packages for `89083` projects.

As I understand it `repology`'s `project` means upstream project name.
If we pick `python:networkx` `repology` name then `nixpkgs` provides a
few versions of `networkx` for each python version:

```
$ nix-env -qa | grep networkx
python3.10-networkx-3.1
python3.11-networkx-3.1
```

But what if I tell you the above number is only a minor subset of package
definitions hiding in `nixpkgs`? You could easily access packages like
`python3.12-networkx-3.1`, `python3.10-networkx-3.1` or even
`python3.11-networkx-3.1-riscv64-unknown-linux-gnu`. None of them are
listed on `repology`.

Abundance of various package flavours like this one is a well-known fact 
or a seasoned user of `nixpkgs`.

A few days ago I attempted to update `autoconf` from `2.71` to `2.72`
version. It's supposed to be a minor maintenance release without
many breaking changes. To make sure I don't break too much I attempted
to validate that all the packages that somehow use `autoconf` are still
building correctly.

## On attributes and package names

`NixOS` and `nixpkgs` users almost never deal with exact package names:
resolving a package name to package definition is slow and ambiguous.

Instead `nixpkgs` encourages users to use "attribute names" using
`nix`-language level constructs.

For example `python3.11-networkx-3.1` would have a name of
`python3Packages.networkx` on my system. The same package also has quite
a few aliases:

- `python311Packages.networkx`
- `python3.pkgs.networkx`
- `python311.pkgs.networkx`

Each of them evaluates to the same package definition. An example
`nix repl` session to make sure it's still true:

```
$ nix repl -f '<nixpkgs>'

nix-repl> python3Packages.networkx
«derivation /nix/store/659allxmdwqxr4zmg03z8wqyizlsdmgh-python3.11-networkx-3.1.drv»

nix-repl> python311Packages.networkx
«derivation /nix/store/659allxmdwqxr4zmg03z8wqyizlsdmgh-python3.11-networkx-3.1.drv»

nix-repl> python311.pkgs.networkx
«derivation /nix/store/659allxmdwqxr4zmg03z8wqyizlsdmgh-python3.11-networkx-3.1.drv
```

The `.drv` files have identical hash part which means all the names are
equivalent when used as is.

Simpler examples of attributes are `re2c` and `gnugrep`. More complex
ones are `python3Packages.ninja`, `linuxPackages_latest.kernel.configfile` and
`pkgsCross.riscv64.re2c`.

Thus to answer a question of what packages should I test after
`autoconf` upgrade I would prefer to get attribute names instead of
package names.

## Poor man's reverse dependency lookup

I routinely do package updates that touch many packages indirectly.
To get a list of impacted packages `nixpkgs` provides a
[`maintainers/scripts/rebuild-amount.sh` script](https://github.com/NixOS/nixpkgs/blob/master/maintainers/scripts/rebuild-amount.sh).

It instantiates all the known to hydra attributes into `.drv` files and
checks changed hashes before and after the change. This diff is our
impact. Script's typical output looks like that:

```
$ time ./maintainers/scripts/rebuild-amount.sh --print HEAD^
Estimating rebuild amount by counting changed Hydra jobs (parallel=unset).
     32 x86_64-darwin
     63 x86_64-linux

          asciidoc-full-with-plugins.x86_64-darwin  dist=/nix/store/...-asciidoc-full-with-plugins-10.2.0-dist;/nix/store/...-asciidoc-full-with-plugins-10.2.0
          asciidoc-full-with-plugins.x86_64-linux   dist=/nix/store/...-asciidoc-full-with-plugins-10.2.0-dist;/nix/store/...-asciidoc-full-with-plugins-10.2.0
          asciidoc-full.x86_64-darwin               dist=/nix/store/...-asciidoc-full-10.2.0-dist;/nix/store/...-asciidoc-full-10.2.0
          asciidoc-full.x86_64-linux                dist=/nix/store/...-asciidoc-full-10.2.0-dist;/nix/store/...-asciidoc-full-10.2.0
          auto-multiple-choice.x86_64-darwin        /nix/store/...-auto-multiple-choice-1.6.0
          auto-multiple-choice.x86_64-linux         /nix/store/...-auto-multiple-choice-1.6.0
          bicgl.x86_64-darwin                       /nix/store/...-bicgl-unstable-2018-04-06
          bicgl.x86_64-linux                        /nix/store/...-bicgl-unstable-2018-04-06
          bicpl.x86_64-darwin                       /nix/store/...-bicpl-unstable-2020-10-15
          bicpl.x86_64-linux                        /nix/store/...-bicpl-unstable-2020-10-15
          cantor.x86_64-linux                       /nix/store/...-cantor-23.08.4
          clevis.x86_64-linux                       man=/nix/store/...-clevis-19-man;/nix/store/...-clevis-19
          conglomerate.x86_64-darwin                /nix/store/...-conglomerate-unstable-2017-09-10
          ...
          netpbm.x86_64-darwin                                                                     bin=/nix/store/...-netpbm-11.4.4-bin;dev=/nix/store/...-netpbm-11.4.4-dev;/nix/store/...-netpbm-11.4.4
          netpbm.x86_64-linux                                                                      bin=/nix/store/...-netpbm-11.4.4-bin;dev=/nix/store/...-netpbm-11.4.4-dev;/nix/store/...-netpbm-11.4.4

          ...
...
real    6m38,854s
user    6m19,604s
sys     0m17,102s
```

Here I changed `netpbm` package in `HEAD` commit and that caused the
rebuild of `63` `x86_64-linux` packages (and `32` `x86_64-darwin` ones).
In this case rebuilding all `63` of them is not a big deal.

But even here some of the packages are probably not worth testing.
`netpbm` has something to do with image formats and `cleavis` is about
the encryption. I would guess `cleavis` would not be impacted by the
update at all.

It would be nice to find all **direct** users of `netpbm` instead and
rebuild those.

The very first topic I created on `NixOS discourse` was about
[reverse dependencies lookup](https://discourse.nixos.org/t/how-do-you-find-reverse-dependencies/15057).

I was a bit surprised there was no standard tool like that and wrote a
hack to do it:

```nix
# use as:
#    import ./arevdeps.nix linuxHeaders pkgs lib
revdepAttr: pkgs: lib:
let isDrv = v: (builtins.tryEval v).success && lib.isDerivation v;
    # skip broken and unsupported packages on this system in a very crude way:
    safeReadFile = df: let c = builtins.tryEval (builtins.readFile df); in if c.success then c.value else "";
    fastHasEntry = i: s: s != builtins.replaceStrings [i] ["<FOUND-HERE>"] s;
    sInDrv = s: d: fastHasEntry s (safeReadFile d.drvPath);
    rdepInDrv = rdep: d: builtins.any (s: sInDrv s d)
                                      (builtins.map (o: rdep.${o}.outPath) rdep.outputs);
    matchedPackages = lib.filterAttrs (n: d: isDrv d && rdepInDrv revdepAttr d)
                                      pkgs;
in builtins.attrNames matchedPackages
```

It's a bit wordy (not much `nix` experience by then) but it's idea is
simple: find references to a searched package via it's `.drv` path by
looking at `.drv` files created from attributes in `<nixpkgs>` object.
Let's try to look up `netpbm` against it:

```
nix-repl> import ./arevdeps.nix netpbm pkgs lib
[ "auto-multiple-choice" "bicpl" "fbcat" "foomatic-db-ppds" "fped"
  "img2pdf" "latex2html" "lilypond" "lilypond-unstable" "mup" "netpbm"
  "pcb" "pnglatex" "sng" "xplanet" "yad" ]
```

Only 16 packages!

The major caveat is that the hack does not try to descend from the top
level down to other attributes like `python3Packages.*` or
`haskellPackages.*`.

In theory you could direct the hack to specific attributes and expand
the output:

```
nix-repl> import ./arevdeps.nix netpbm pkgs.python3Packages lib
[ "img2pdf" "pnglatex" ]
```

In practice it's not very convenient and I never did it. The command
already takes a while to run and running it multiple times is no fun.

I decided to extend initial script to handle nested attributes.

## Naive attempt to extend the hack

In theory it's one small extension: add a tiny amount of code to descend
into child attributes and you are done.

Sounds good, did not work.

The result started crashing on various syntax errors in various
`nixpkgs` files. When I worked error around `nix` ate `100GB` of `RAM`
and crashed without producing the result.

I'll spare you the implementation details of a modified script.

Unbounded `RAM` usage is very unfortunate as the script in theory could
run in constant space. It's not very simple in practice as `nix` uses
[`boehm-gc`](https://en.wikipedia.org/wiki/Boehm_garbage_collector) to
control it's heap usage. I'm not sure a single loaded `<nixpkgs>` tree
allows for any garbage collection of `.drv` files.

I filed <https://github.com/NixOS/nix/issues/9671> issue to see if there
are any obvious references `nix` could remove to make garbage collection
more efficient.

But in the shorter term I had to try something else.

## A step back: just list all the attributes

I realized there are multiple problems with my hack and I attempted to
solve a simple problem. I wanted to just list all the available
attributes in `<nixpkgs>`. Ideally not just those known to `hydra` CI
builder but the ones hiding in `pkgsCross` in other places.

Quiz question: how hard is it to get a list of such attributes to
explore?

Getting an attribute list of a single set is trivial via single call of
`lib.attrNames`:

```
$ nix repl -f '<nixpkgs>'

nix-repl> lib.take 4 (lib.attrNames pkgs)
[ "AAAAAASomeThingsFailToEvaluate" "AMB-plugins" "ArchiSteamFarm" "AusweisApp2" ]

nix-repl> lib.length (lib.attrNames pkgs)
20059
```

The problem is that some of the attributes are neither derivations not
attribute sets:

```
nix-repl> pathsFromGraph
/nix/store/jp811zl7njhg1g59x95dgqs4rddgr7xz-source/pkgs/build-support/kernel/paths-from-graph.pl
```

Luckily it's easy to introspect value type via predefined predicates:

```
nix-repl> lib.isPath pkgs.pathsFromGraph
true

nix-repl> lib.isDerivation re2c
true

nix-repl> lib.isAttrs pkgsCross
true
```

Another problem is that some of attribute values don't evaluate
successfully. Sometimes intentionally:

```
nix-repl> pkgs.AAAAAASomeThingsFailToEvaluate
error:
       … while calling the 'throw' builtin

         at /nix/store/jp811zl7njhg1g59x95dgqs4rddgr7xz-source/pkgs/top-level/all-packages.nix:106:36:

          105|   ### Evaluating the entire Nixpkgs naively will fail, make failure fast
          106|   AAAAAASomeThingsFailToEvaluate = throw ''
             |                                    ^
          107|     Please be informed that this pseudo-package is not the only part

       error: Please be informed that this pseudo-package is not the only part
       of Nixpkgs that fails to evaluate. You should not evaluate
       entire Nixpkgs without some special measures to handle failing
       packages, like using pkgs/top-level/release-attrpaths.nix.

nix-repl> pkgs.gccWithoutTargetLibc
error:
       … while evaluating the attribute 'gccWithoutTargetLibc'
        15967|   gccWithoutTargetLibc = assert stdenv.targetPlatform != stdenv.hostPlatform; let
       error: assertion '((stdenv).targetPlatform != (stdenv).hostPlatform)' failed
```

And sometimes entirely by accident:

```
nix-repl> pkgsLLVM.clang_6
error:
       … while evaluating the attribute 'clang_6'
       error: attribute 'clangUseLLVM' missing
```

I just need to filter out all the problematic attributes and leave only
evaluatable ones. `nix` even provides a `builtins.tryEval` just for
this case:

```
nix-repl> builtins.tryEval pkgs.AAAAAASomeThingsFailToEvaluate
{ success = false; value = false; }

nix-repl> builtins.tryEval pkgs.gccWithoutTargetLibc
{ success = false; value = false; }

nix-repl> builtins.tryEval pkgs.gcc
{ success = true; value = «derivation /nix/store/y5vq20420rg2g6h03c8x7sxzjcxphg9w-gcc-wrapper-12.3.0.drv»; }
```

Sounds easy, right? As always there is a catch:

```
nix-repl> builtins.tryEval pkgsLLVM.clang_6
error:
       error: attribute 'clangUseLLVM' missing

nix-repl> builtins.tryEval pkgsMusl.adobe-reader
       error: evaluation aborted with the following error message: 'unsupported platform for the pure Linux stdenv'
```

Not all error types can be caught by `builtins.tryEval`: only `throw`
and `aasert` calls (these are explicitly present in the call) are
catchable. The rest is considered a bug in `nix` expression and can't be
caught. I guess it's the way to signal invalid `.nix` programs.

Lack of error recovery means that I can't do attribute filtering like
in a single `nix` expression! I had 2 options:

1. Write an external script that probes for problematic attributes and
   somehow skips them.

2. Fix all the evaluation errors  in `nixpkgs` to make the naive
   filtering work.

`[1.]` would require use of `nix` as a library in one form or another.
I was lazy and tried `[2.]` first. My assumption that it was a small
list of easy to fix errors.

Here is my first version of a simple attribute lister:

```nix
# Usage example:
# $ nix-instantiate --eval --strict ~/.config/nixpkgs/lib/all-attrs.nix -I nixpkgs=$PWD

{ nixpkgs ? import <nixpkgs> {
    config = {
    };
  }
, rootAttr ? "pkgs"
, verbose ? 1 # warn

# How to pick, resource usage for me as of 2023-12-28:
# 1 - 10 seconds, ~2GB of RAM
# 2 - 2 minutes, ~25GB of RAM (unfiltered attrs)
# 3 - 5+ minutes, ~70GB+ or RAM Fails on attributes like `pkgsCross.iphone32.ammonite`
# anything else: at your risk
, maxDepth
}:

let
  # simple variables:
  lib = nixpkgs.lib;

  # logging:
  err   = s: e: lib.trace "ERROR: ${s}" e;
  warn  = s: e: if verbose >= 1 then lib.trace "WARN: ${s}" e else e;
  info  = s: e: if verbose >= 2 then lib.trace "INFO: ${s}" e else e;
  debug = s: e: if verbose >= 3 then lib.trace "DEBUG: ${s}" e else e;

  # root to start at
  root = lib.attrByPath (lib.splitString "." rootAttr)
                        (warn "did not find ${rootAttr}" {})
                        nixpkgs;
  # other helpers:
  isPrimitive = v: lib.isFunction v
                || lib.isString v
                || lib.isBool v
                || lib.isList v
                || lib.isInt v
                || lib.isPath v
                || v == null;

  go = depth: ap: v:
    let
      a = lib.showAttrPath ap;
      e = builtins.tryEval v;
      maybe_go_deeper =
        if depth >= maxDepth
        then info "too deep (depth=${toString depth}) nesting of a=${a}, stop" []
        else map (nv: go (depth + 1) (ap ++ [nv.name]) nv.value)
                 (lib.attrsToList v);
    in debug "inspecting ${a}" (
    if !e.success then info "${a} fails to evaluate" []
    else if lib.isDerivation v
    then [a]
    else if lib.isAttrs v then maybe_go_deeper
    else if isPrimitive v then []
    # should not get here
    else warn "unhandled type of ${a}" []);
in lib.flatten (go 0 [] root)
```

It's more than a page of code. It explores at most `maxDepth` attributes
deep.

Usage example:

```
$ time nix-instantiate --eval --strict ~/.config/nixpkgs/lib/all-attrs.nix -I nixpkgs=$PWD --arg maxDepth 1
...
[ "AMB-plugins" "ArchiSteamFarm" ... "zulu8" "zuo" "zwave-js-server"
  "zx" "zxcvbn-c" "zxfer" "zxing" "zxing-cpp" "zxpy" "zxtune" "zydis"
  "zyn-fusion" "zynaddsubfx" "zynaddsubfx-fltk" "zynaddsubfx-ntk" "zz"
  "zziplib" "zzuf" ]

real    0m6,587s
user    0m5,963s
sys     0m0,608s
```

Yay! It works! Note: this is just one depth level of attributes, like
`re2c`. It does not contain packages like `python3Packages.ninja`. The
run takes 2GB and 6 seconds to complete.

If we want one level deeper we can specify `--maxDepth 2`:

```
$ time nix-instantiate --eval --strict ~/.config/nixpkgs/lib/all-attrs.nix -I nixpkgs=$PWD --arg maxDepth 2
...
[ "AMB-plugins" "ArchiSteamFarm" "AusweisApp2" "BeatSaberModManager"
  ...
  "CuboCore.coreaction" "CuboCore.corearchiver" "CuboCore.corefm"
  "CuboCore.coregarage"
  ...
  "__splicedPackages.AMB-plugins" "__splicedPackages.ArchiSteamFarm"
  ...
  "pkgsLLVM.aaaaxy" "pkgsLLVM.aacgain"
  ...
  "pkgsHostTarget.zig_0_11" "pkgsHostTarget.zig_0_9"
  ...
  "zyn-fusion" "zynaddsubfx" "zynaddsubfx-fltk" "zynaddsubfx-ntk" "zz"
  "zziplib" "zzuf" ]

real    1m4,845s
user    0m58,368s
sys     0m5,910s
```

Second level also works! This time it took 25GB and a bit more than 1
minute to print the result. There are a few issues with it: some
attribute trees like `__splicedPackages` and `pkgsHostTarget` are
redundant. We already get attributes like `python3Packages.ninja`.
But are not quite at `pkgsCross.riscv64.re2c` yet.

## Running the naive lister on larger depths

I ran the naive script above and derived the following fixes:

- [PR 277117](https://github.com/NixOS/nixpkgs/pull/277117):
  `netbsd.libcurses` constructed invalid type of `NIX_CFLAGS_COMPILE`.
- [PR 276984](https://github.com/NixOS/nixpkgs/pull/276984):
  `beam.packages.erlangR23` referred to non-existent `erlang_23`
  attribute.
- [PR 276985](https://github.com/NixOS/nixpkgs/pull/276985):
  `coq-kernel.launcher` used an alias instead of package name.
- [PR 276995](https://github.com/NixOS/nixpkgs/pull/276995):
  `haskell.packages.ghc810.mod` used non-existent `mod_0_1_2_2` attribute
  in it's definition.
- [PR 276986](https://github.com/NixOS/nixpkgs/pull/276986):
  `dockerTools.tests.docker-tools` used an alias instead of actual name.
- [PR 277211](https://github.com/NixOS/nixpkgs/pull/277211):
  `nixosTests.nixops` had an unsatisfied function argument.
- [PR 277355](https://github.com/NixOS/nixpkgs/pull/277355):
  `stdenv` used `abort`.
- [PR 277364](https://github.com/NixOS/nixpkgs/pull/277364):
  `python312Packages.array-record` accessed non-existent attribute.

Getting 8 bugs just like that impressed me. I optimized lister a bit to
be able to descend into larger attribute depths and found a few more
bugs.

If you did not notice my lister script never attempted to explore any
attributes **within** derivations. If we pick a `re2c` example the
script would never get to `re2c.passthru.updateScript`.

And `passthru` are probably least tested attributes as they rarely used
by `hydra` CI. The `passthry.tests` in particular are **not** used by
`hydra` but are used by `ofborg` `GitHub` actions. And the caveat of
`ofborg` is that it rarely shows test failures as a red cross. Usually
it renders a failure as inconclusive gray. The assumption is that the
reviewer look at the underlying failure and makes a decision.

Thus I added a knob to descend into attributes of derivations
[this way](https://github.com/trofi/nixpkgs-overlays/commit/50ed200dc06ee1b6ec8ad8ca879a9948cc85135e):

```diff
--- a/lib/all-attrs.nix
+++ b/lib/all-attrs.nix
@@ -35,6 +35,11 @@
 , maxDepth

 , ignoreCross ? true
+
+# Whether to validate every attribute within derivations themselves.
+# Most intereting fields are `passthru.tests`, but sometimes there are
+# very unusual bugs lurking. Risky but very fun!
+, ignoreDrvAttrs ? true
 }:

 let
@@ -76,11 +81,9 @@ let
     in debug "inspecting ${a}" (
     if !e.success then info "${a} fails to evaluate" []
     else if lib.isDerivation v
-    # TODO: add an option to traverse into derivations as well.
-    # Mainly to test validity of `passthru.tests`, `metadata` and
-    # similar.
-    then [a] # TODO: "++ maybe_go_deeper"
+    then [a] ++ lib.optionals (!ignoreDrvAttrs) maybe_go_deeper
     # Skip "foo = self;" attributes like `pythonPackages.pythonPackages`
+    # TODO: might skip too much.
     else if lib.isAttrs v && depth > 0 && lib.hasAttr (lib.last ap) v then info "${a} is a repeated attribute, skipping" []
     else if lib.isAttrs v then maybe_go_deeper
     else if isPrimitive v then []
```

I also had to add a few ignored paths like `nixosTests` as they require
around `1GB` of extra `RAM` per test(!) and I had to skip `pkgsCross`
as it derives too many attributes for (still!) naive script to handle.

But even with such a limited lister I managed to get to these bugs:

- [PR 277399](https://github.com/NixOS/nixpkgs/pull/277399):
  `bazel-watcher.bazel.tests` had a `optionalSttrs` typo instead of
  `optionalAttrs`.
- [PR 277400](https://github.com/NixOS/nixpkgs/pull/277400):
  `bitcoind-knots` referred to non-existent test.
- [PR 277402](https://github.com/NixOS/nixpkgs/pull/277402):
  `cargo` tried to pull tests for a package that does not define it.
- [PR 277404)](https://github.com/NixOS/nixpkgs/pull/277404):
  `corosync` did not specify a test input argument that it used.
- [PR 277408](https://github.com/NixOS/nixpkgs/pull/277408):
  `lua-wrapper` uses non-existent attributes to define paths.
- [PR 277420](https://github.com/NixOS/nixpkgs/pull/277420):
  `displaylink` referred to non-existent test.
- [PR 277434](https://github.com/NixOS/nixpkgs/pull/277434):
  `gnupg22` incorrectly refers to the test suite.
- [PR 277435](https://github.com/NixOS/nixpkgs/pull/277435):
  `pisocsope.rules` looked `writeTextDir` in `lib` instead of `pkgs`.
- [PR 277473](https://github.com/NixOS/nixpkgs/pull/277473):
  `guacamole-client` was referring to deleted test.
- [PR 277474](https://github.com/NixOS/nixpkgs/pull/277474):
  `mutmut` used `testers` attribute without use.
= [PR 277494](https://github.com/NixOS/nixpkgs/pull/277494):
  `buildFHSEnv` did not fully handle `multiPaths = null`.
- [PR 277512](https://github.com/NixOS/nixpkgs/pull/277512):
  `owncast` referred to non-existent test.
- [PR 277517](https://github.com/NixOS/nixpkgs/pull/277517):
  `python3Packages.pypaBuildHook.tests` test referred non-existent `.nix`
  file.
- [PR 277543](https://github.com/NixOS/nixpkgs/pull/277543):
  `pythonInterpreters.pypy39_prebuilt` referred to deleted `pypy38`
  attribute, not `pypy39`.
- [PR 277580](https://github.com/NixOS/nixpkgs/pull/277580):
  `tigervnc.tests` referred to non-existent test.
- [PR 277581](https://github.com/NixOS/nixpkgs/pull/277581):
  `wezterm.tests` referred to commented out tests.
- [PR 277590](https://github.com/NixOS/nixpkgs/pull/277590):
  `devpod.tests` passed incorrect parameter to a test function.
- [PR 277593](https://github.com/NixOS/nixpkgs/pull/277593):
  `fakeroot.tests` passed incorrect parameter to a test function.
- [PR 277595](https://github.com/NixOS/nixpkgs/pull/277595):
  `findup.tests` passed incorrect parameter to a test function.
- [PR 277600](https://github.com/NixOS/nixpkgs/pull/277600):
  `jellyfin-ffmpeg.tests` is missing `pkg-config` annotation.
- [PR 277617](https://github.com/NixOS/nixpkgs/pull/277617):
  `build-support/go` code constructed inaccessible `vendorSha256`
  attribute.
- [PR 277715](https://github.com/NixOS/nixpkgs/pull/277715):
  `octoprint` referred to non-existent attribute in `tests`.
- [PR 277741](https://github.com/NixOS/nixpkgs/pull/277741):
  `pypy2Packages.attrs` refers non-existent `.nix` file.
- [PR 277751](https://github.com/NixOS/nixpkgs/pull/277751):
  `python3Packages.openllm`: fix `passthru` dependency references and
  fix variable shadowing.
- [PR 277777](https://github.com/NixOS/nixpkgs/pull/277777):
  `python3Packages.openllm-client`: fix `passthru` dependency references.
- [PR 277788](https://github.com/NixOS/nixpkgs/pull/277788):
  `python3Packages.openllm-core`: fix `passthru` dependency references.
- [PR 277880](https://github.com/NixOS/nixpkgs/pull/277880):
  `valhalla` was missing `pkgConfigModules` definition.
- [PR 277899](https://github.com/NixOS/nixpkgs/pull/277899):
  `zammad.src.meta` failed to evaluate due to incorrect position
  assumption: no metadata attributes were defined in the `.nix` files.
- [PR 277973](https://github.com/NixOS/nixpkgs/pull/277973):
  `ruff.tests` referred `ruff-lsp` alias instead of direct name.
- [PR 277982](https://github.com/NixOS/nixpkgs/pull/277982):
  `spark.tests`: referred to `nixosTest` alias.
- [PR 278034](https://github.com/NixOS/nixpkgs/pull/278034):
  `nixosTests.kernel-generic` attempted to use `bool` value as a kernel
  derivation.
- [PR 278044](https://github.com/NixOS/nixpkgs/pull/278044):
  `aaxtomp3`: fix invalid reference to `glibc` for non-`glibc` targets.
- [PR 278069](https://github.com/NixOS/nixpkgs/pull/278069):
  `haskell.packages.ghc810` refer to non-existent packages.
- [PR 278074](https://github.com/NixOS/nixpkgs/pull/278074):
  `haskell.packages.ghc865Binary` refer to non-existent packages.
- [PR 278076](https://github.com/NixOS/nixpkgs/pull/278076):
  `haskell.packages.ghc98` refer to non-existent packages.
- [PR 278224](https://github.com/NixOS/nixpkgs/pull/278224):
  `haskell.packages.ghcjs` lacks `llvmPackages` attribute implied by
  `ghc-8.10` packages.
- [PR 278528](https://github.com/NixOS/nixpkgs/pull/278528):
  `python3Packages.paddlepaddle`: unhandled error in `src` attribute
  dereference.
- [PR 278915](https://github.com/NixOS/nixpkgs/pull/278915):
  `nvidia-x11` unconditionally refers to `/share/` even if libraries are
  the only enabled bit.
- [PR 278950](https://github.com/NixOS/nixpkgs/pull/278950):
  `pythonInterpreters.pypy39_prebuilt` failed the `test` evaluation as
  it exposed unhandled `pythonAttr = null` value. The test expected a
  real object.
- [PR 279018](https://github.com/NixOS/nixpkgs/pull/279018):
  `systemd.tests.systemd-journal-upload` has invalid maintainer
  specified.

Note: It's not the full list of required fixes. For more complex cases I
filed a few bugs to get maintainers' help:

- [Issue 277285](https://github.com/NixOS/nixpkgs/issues/277285):
  `pkgsStatic.php` enters infinite loop and exhausts all available
  memory.
- [Issue 277628](https://github.com/NixOS/nixpkgs/issues/277628):
  `godot3-mono.nugetSource.meta` detects infinite recursion on
  evaluation.
- [Issue 277698](https://github.com/NixOS/nixpkgs/issues/277698):
  `ocamlPackages.janeStreet_0_15` has unsatisfied attributes.

## Did I get the list package for `autoconf`?

Sort of: I managed to write the hack to get a list of packages using
`autoconf` in a few layers deep below top level. It's good enough for
testing close to exhaustive.

But I did not get exhaustive at all. There are two main problems still:

1. The attribute sets are infinite in `nixpkgs`. An example a bit silly
   but still valid attribute is:

   ```
   nix-repl> pkgs.pkgs.pkgs.pkgsCross.riscv64.pkgsMusl.pkgsCross.riscv64.pythonPackages.pythonPackages.pythonPackages.ninja
   «derivation /nix/store/4vnprl12q706s3ilb1g1c2v4bf9pjpc9-ninja-1.11.1.drv»`
   ```

   `nix` the language does not provide the mechanism to compare
   references to shortcut things like `pythonPackages.pythonPackages`
   And each scope has those self-referential package structures.

2. Even if the attribute set was finite in `<nixpkgs>` the mere act of
   listing them takes 100s of GB. It looks like it's because `nix` does
   not collect already evaluated garbage expressions that still have
   references from other parts of the tree. The packages loops in
   `nixpkgs` from `[1.]` do not help in that at all.

I am still hopeful that I can get something decent soon. I can
workaround `[2.]` `RAM` exhaustion by declaring defeat on a single
`.nix` script and run it in incremental mode. Say, to process 100
packages at a time to avoid infinite memory growth.

Another option would be to write a separate tool using `nix` as a
library to parse and evaluate `.nix` code that does this job
specifically. But I'd prefer to try to fix `nix` `GC` behaviour first. I
think it's tractable.

## Parting words

Traversing package attribute set in `nixpkgs` is surprisingly
challenging. I think it is fixable and should be fixed (at least for
non-`pkgsCross.*` part of the tree). Fetching metadata about the
packages is a frequent operation for many types of tree-wide changes.

I had a lot of fun writing debuggable `.nix` code to list available
`nixpkgs` attributes. So far my result is hiding at
<https://github.com/trofi/nixpkgs-overlays/blob/main/lib/all-attrs.nix>.

So far I managed to get to 4 levels of attribute depth using `60GB` of
`RAM`. This uncovered at least 27 bugs.

Some of the bugs are very scary:

- `cargo` did not have tests: <https://github.com/NixOS/nixpkgs/pull/277402>
- `lua-wrapper` did not expose correct paths: <https://github.com/NixOS/nixpkgs/pull/277408>

`builtins.tryEval` does not catch all the failure types in attribute
evaluation: `throw` / `assert` are fine, but reference to non-existent
attribute (or `assert`) are not.

`pkgs.nixosTests` attribute set is very slow and RAM hungry to evaluate:
<https://github.com/NixOS/nix/issues/9671>

You can also fix a few `nixpkgs` bugs! Just run `all-attrs.nix` as:

```
$ nix-instantiate --eval --strict ./all-attrs.nix \
    -I nixpkgs=~/path/to/nicpkgs \
    --arg maxDepth 2 --arg verbose 3 --arg ignoreDrvAttrs false
```

And see what you get.

Next steps I'd like to take at some future point:
- batch package listing and package instantiation in smaller batches to
  get RAM usage down to a few `GB`s.
- explore `nix` and garbage collection mechanisms to make it friendlier
  to large evaluations like `all-attrs.nix`

Have fun!
