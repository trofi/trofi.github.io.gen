---
title: "parallel installs in nixpkgs"
date: March 15, 2023
---

## Tl;DR

As of a few minutes ago `nixpkgs` does parallel installs for `Makefile`
based build systems using `make install -j$(nproc)`. As long as the
packages have `enableParallelBuilding = true;`.
Sequential packages are unchanged and still do sequential installs.
You can revert to previous behavior for your packages by using
`enableParallelInstalling = false;` if needed. But better try to fix the
issues upstream.

## More words

`Makefile`s are [hard](/posts/230-when-make-j-nproc-fails.html). I tried
enabling parallel builds by default in `nixpkgs` and
[failed](https://github.com/NixOS/nixpkgs/issues/142338).
The primary rejection reason was the worry that too many packages will
break and `nixpkgs` will degrade too much. I agree those problems are
not trivial to diagnose, debug and fix. We need a better way of weeding
out the issues.

But I did not completely give up. I still want my "parallel-by-default"
dream to come true. I added a new [`make --shuffle`](/posts/238-new-make-shuffle-mode.html)
to `GNU make` to ease reporting and validation of parallel build fixes.
And I'm still occasionally sending fixes for parallel build issues
upstream. I noticed others also do it time to time. That's so nice to
see!

A few weeks ago my main desktop [broke](/posts/280-cooling-failure.html)
and I had to spend some time on my older machine that is not that fast
to compile packages. There I noticed long install phase of `openssl`
package in `nixpkgs`.

Quick quiz: how log do you think `make install` takes for `openssl` on
modern hardware? 1 second? 10 seconds? 1 minute? 10 minutes? 1 hour?

Got you your estimate?

```
# We can grep most recent hydra build log:
$ nix log $(nix-build -A openssl) | fgrep 'Phase completed in'

buildPhase completed in 5 minutes 0 seconds
installPhase completed in 2 minutes 9 seconds
fixupPhase completed in 41 seconds
```

2 minutes! This time is comparable to the whole build phase that takes
5 minutes. Is it a lot? It really depends on what `installPhase` is
expected to do.
Some packages just copy one or two files into `$DESDIR`, some packages
run registration tools of sorts. It depends.
`openssl`'s install phase builds and compresses a few hundreds of manual
pages. The tasks are expressed as `Makefile` targets and are perfect for
parallelism.
One could argue that these heavyweight actions belong to the build (and
not install) phase. But sometimes things are not as straightforward.

Apparently one of frequent examples of non-trivial install actions is
`libtool`. There binary relinking happens on installation when shared
libraries get copied (relinked!) to their final directory and binaries
are updated (also relinked!) to contain the `RUNPATH` to point to new
library location.

You might think that relinking phase should not take that much. But
sometimes packages consist of tens if not hundreds of libraries and
binaries. Let's pick `solanum` `IRC` server as an example:

```
$ nix log $(nix-build -A solanum) | fgrep 'Phase completed in'
configurePhase completed in 39 seconds
buildPhase completed in 1 minutes 11 seconds
installPhase completed in 1 minutes 1 seconds
```

It takes almost as much time to install (and relink) the binaries as it
takes to build the package.

The fun thing is that both `openssl` and `solanum` use parallel builds
(`make -j$(pnroc)`) but use sequential installs! (`make install`).
I was very surprised to see missing parallelism in install phase. It
looked so simple to fix! If the package is already built-in parallel in
`nixpkgs` then the chances are high that parallel installs would work
as well.

To validate the theory I passed `make install -j$(nproc)` to `openssl`
and found that the whole `configure / make / make install` process
shrunk from `1m54s` down to `59s`. It's 2x speedup right there. Note
that `installPhase` has to have even more dramatic difference as 
unchanged build time is included into both times.

I quickly hacked up the [`PR`](https://github.com/NixOS/nixpkgs/pull/217568)
to enable parallelism and proposed it for review.
Surprisingly (or not so surprisingly) not everyone was happy to see the
change. The concerns were: possible install failures, possible
corruption on install, possibly added non-determinism, possible masking
of install-time issues by speeding install phase up.

To quantify the breakage concern NixOS Infra team set a one-off
[`pr-217568-stdenv-parallel-install`](https://hydra.nixos.org/jobset/nixpkgs/pr-217568-stdenv-parallel-install)
hydra job set for this change before it gets merged to any of the main
branches.
It [uncovered](https://github.com/NixOS/nixpkgs/pull/217568/commits) 12
new build failures:

- `net-snmp`
- `xfsprogs`
- `sssd`
- `subversion`
- `ocaml`
- `eresi`
- `s9fes`
- `vpnc`
- `asymptote`
- `gretl`
- `qsynth`
- `solanum`

The failures are obviously parallel install failures as they failed in
`installPhase` with very obscure complains about missing files.

As an example `solanum` install failure is being investigated in
[`Issue #405`](https://github.com/solanum-ircd/solanum/issues/405) upstream.
It's an interesting case of `libtool`-based build system with a bunch
of recursive `Makefile`s.
There are a few triggers there: source file deletion during install and
something related to unusual dependencies during install.
Source file deletion causes rebuild and relinking of the project during
install (ugh!).

Otherwise, it was a very small fallout which I plugged by sprinkling
`enableParallelInstalling = false`. We might need a few more of those
workarounds as parallelism bugs sometimes take a while to surface.

## Parting words

If you suspect that package fails parallel installs in `nixpkgs` try to
add a `enableParallelInstalling = false;` as a workaround.

`nixpkgs` made one step closer to build most packages with full
available parallelism. Packages like `openssl` already build faster in
`staging` branch of `nixpkgs`.

It did not take much code to enable parallel installs only for packages
that already enable parallel builds.

While it was a very conservative change it still broke 12 more packages.
12 is 2 orders of magnitude lower than typical amount of breakage
present in `master` (3000 to 4000 broken packages). Even if I missed a
few more cases it should be just a few cases and not thousands of new
failures.

If you are an upstream package owner then give parallel install a go
and try to address the install failures that arise. Here are a few hints
that might help:

- use `--shuffle` option for `GNU make 4.4` or later to reorder
  prerequisite execution.
- along with high parallelism also try to use low parallelism level,
  like `-j2`. That gives more chance to execute only subset of
  prerequisites.
- make sure your `/usr/lib` (or other system default pah) does not
  contain the libraries you are testing for relinking parallelism.
  Otherwise, you would not be able to reproduce the failure as relinking
  will accidentally happen against the system library.

It took `hydra` only 2 weeks of lowest priority to build all ~60000
`linux` packages `nixpkgs` has.

I have a few more thoughts on how to incrementally improve quality of
parallel builds in `nixpkgs` like enabling `--shuffle` by default.
Let's save that for another time.

Have fun!
