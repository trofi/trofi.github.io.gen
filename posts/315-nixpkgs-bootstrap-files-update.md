---
title: "nixpkgs bootstrap files update"
date: May 26, 2024
root: "http://trofi.github.io"
---

## Tl;DR

`nixpkgs` now has up to date `bootstrapFiles` at least for `i686-linux`
and `x86_64-linux`. Moreover we now have an easy procedure to update the
binaries! Instructions are hiding in
[`maintainers/scripts/bootstrap-files/README.md`](https://github.com/NixOS/nixpkgs/blob/master/maintainers/scripts/bootstrap-files/README.md#how-to-request-the-bootstrap-seed-update).
For example the
[PR#288866](https://github.com/NixOS/nixpkgs/pull/288866) to update
`i686-unknown-linux-gnu` was generated as:

```
$ maintainers/scripts/bootstrap-files/refresh-tarballs.bash \
    --commit \
    --targets=x86_64-unknown-linux-gnu
$ git push my-fork staging:bootstrapFiles-x86_64-unknown-linux-gnu-update
```

This work paves the way for more frequent updates of the existing
bootstrap files and simplifies the procedure of introducing support for
new targets into `nixpkgs`.

As a nice side-effect `x86_64-linux` bootstrap does not depend on `i686`
`busybox` binary any more and uses `x86_64` binary instead:

```{render=dot}
digraph G {
  node [shape=box];

  subgraph cluster_before {
    label="before";

    "busybox-before-i686" [label="busybox-i686"];
    "boostrap-files-before-i686" [label="boostrap-files-i686"];
    "boostrap-files-before-x86_64" [label="boostrap-files-x86_64"];

    "busybox-before-i686" -> "boostrap-files-before-i686";
    "busybox-before-i686" -> "boostrap-files-before-x86_64" [color=red];
  }

  subgraph cluster_after {
    label="after";

    "busybox-after-i686" [label="busybox-i686"];
    "busybox-after-x86_64" [label="busybox-x86_64" color=green];
    "boostrap-files-after-i686" [label="boostrap-files-i686"];
    "boostrap-files-after-x86_64" [label="boostrap-files-x86_64"];

    "busybox-after-i686"   -> "boostrap-files-after-i686";
    "busybox-after-x86_64" -> "boostrap-files-after-x86_64" [color=green];
  }
}
```

This means that updating `i686-linux` bootstrap files alone does not
trigger the rebuild of `x86_64-linux` world any more.

## More words

### Intro

About two years ago [I noticed](/posts/240-nixpkgs-bootstrap-intro.html)
that `nixpkgs` has quite old initial seed binaries used to bootstrap the
rest of the system.

Normally the version of bootstrap files does not matter as once bootstrap
finishes the original files are not referenced any more. There are a few
annoying exceptions that people stumbled from time to time. One of them
was stale `libgcc.so`.

Stale binaries also cause build breakages from time to time. One
example breakage is discussed in the
[bootstrap deep dive](/posts/275-nixpkgs-bootstrap-deep-dive.html) post,
another example is a [stale `gnumake`](https://github.com/NixOS/nixpkgs/pull/229898#issuecomment-1589179355)
breakage. There are a lot more. They usually get workarounds somewhere
else in `nixpkgs` but almost never in the bootstrap files as they are
harder to update.

These failures are not always easy to debug (or workaround). I
occasionally suggested updating the bootstrap binaries as I expected it
to be a trivial operation for people who did those initially.

I asked a few times on infra's Matrix if bootstrap files could be
updated as part of a release preparation and did not get anywhere.

At some point Bernardo visited me in person and explained the details of
what it takes to upload the binaries and what the requirements for the
new binaries are nowadays. In essence there are two requirements:

1. The binaries should be built by <https://hydra.nixos.org> to make
   sure that binaries come from a somewhat trusted location and others
   can replicate the same binaries from source.
2. The binaries must be uploaded to <https://tarballs.nixos.org> by
   someone someone has the permissions to do it.

Sounds trivial?

At some point I encountered complete failure of `i686-linux` bootstrap
on my file system due to
[64-bit inode values](/posts/297-32-bit-file-API-strikes-back.html) on my
`/nix/store`.

I filed an [Issue#253713](https://github.com/NixOS/nixpkgs/issues/253713)
to request automated periodic bootstrap files updates. I also listed a
few examples where periodic refresh would fix the problems people
encounter hoping that somebody will consider it bad enough and upload
the binaries.

### A light in the tunnel

Alas just filing an issue did not magically fix things.

I noticed that recently `riscv64-linux` bootstrap files were updated in
[PR#2826517](https://github.com/NixOS/nixpkgs/pull/282517) and I took it
as an opportunity to explore the mechanism and automate the whole PR
preparation as the first step.

The procedure looked trivial: you follow a few hydra links and put them
into the `.nix` file.

### Annoying details

I hoped for a script to be 2-3 `curl` calls. But there are always those
pesky details that get in your way.

#### Job names

Some targets (like `risc-v` or `powerpc64`) don't yet have a native
build support on `hydra` (lack of hardware). And yet we have bootstrap
files for those: they are cross-compiled.

Hydra job name and even job results format were different between native
and cross-builds:

   - native targets used only `.dist` style builds
   - cross-targets had `.bootstrapTools` style builds

Those were easy to fix by exposing builds unconditionally with
[PR#284090](https://github.com/NixOS/nixpkgs/pull/284090). And drop
unused `.dist` indirection with
[PR#301639](https://github.com/NixOS/nixpkgs/pull/301639) suggested by
Alyssa.

#### `nixpkgs` file inconsistency (`darwin`)

Once I had a glance at `darwin` bootstrap jobs I noticed it puts files
into slightly different location. I unified it with
[PR#284628](https://github.com/NixOS/nixpkgs/pull/284628) hoping that
somebody else will finish the `darwin` part. And help did come from
`annalee` in [PR#295557](https://github.com/NixOS/nixpkgs/pull/295557).

#### Actual regenerator

Once enough things were in place I hacked up a shell script that fetches
needed files and generates `.nix` files with enough contents as
[PR#284541](https://github.com/NixOS/nixpkgs/pull/284541). The script
ended up being almost 300 lines long!

As a first test I tried it on `musl` targets as they were not using
the binaries from `tarballs.nixos.org` and that felt list an urgent issue.
The [PR#285906](https://github.com/NixOS/nixpkgs/pull/285906) dealt with
`x86_64-unknown-linux-musl` bootstrap files.

#### cross-case

Updating existing files is slightly easier than bringing it a completely
new set of binaries. How do you deal with those?

Having dealt with changes above I could finally answer that question
with some confidence:

- add a new target to `lib/systems/examples.nix`, make sure it can build
  basic things like `pkgsCross.$target.hello`
- add `bootstrapFiles` build entry to
  `pkgs/stdenv/linux/make-bootstrap-tools-cross.nix`, wait for `hydra`
  to build binaries for you
- add your new target to `maintainers/scripts/bootstrap-files/refresh-tarballs.bash`
  your in `CROSS_TARGETS=()` list and run the script.
- send a resulting PR requesting binaries upload as described in
  `maintainers/scripts/bootstrap-files/README.md`

[PR#314823](https://github.com/NixOS/nixpkgs/pull/314823) should
document the same procedure in `nixpkgs`.

## Parting words

I managed to update bootstrap files for `i686-linux` and `x86_64-linux`!

It immediately got the following benefits:

- fixed `i686-linux` bootstrap on file systems with 64-bit inodes
- untangled `x86_64-linux` bootstrap from `i686` `busybox`
- switched `musl` bootstrap files to `hydra`-built files hosted on
  `tarballs.nixos.org`.
- documented a way to introduce new target into `nixpkgs` via
  cross-compiled bootstrap files.

Next steps:

- update `aarch64-linux` bootstrap files (trivial)
- update other cross-targets (trivial)
- work with release engineering team to periodically update the binaries
  on a defined cadence (moderate)

Have fun!
