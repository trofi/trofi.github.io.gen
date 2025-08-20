---
title: "four years on NixOS"
date: August 20, 2025
root: "http://trofi.github.io"
---

It's another yearly instance of my `NixOS` journey
([2024 instance](/posts/316-three-years-on-nixos.html)). I meant to
write it around `25.05` but completely forgot!

## system maintenance

As usual I don't remember what I did to my system over the past year,
so I'm at the `git log` for `/etc/nixos` as it contains all the changes:

- follow `hardware.opengl` -> `hardware.graphics` rename
- follow `hardware.opengl.driSupport{,32Bit}` removal
- follow rename of `gnome.adwaita-icon-theme`
- follow `hardware.pulseaudio` -> `services.pulseaudio` rename
- follow `okular` -> `kdePackages.okular` rename
- drop deprecated `i18n.supportedLocales`
- follow `networking.wireless.iwd.settings.General` -> `networking.wireless.iwd.settings.DriverQuirks` rename
- follow `services.postfix.config` -> `services.postfix.settings.main` rename
- fix `services.postfix.settings.main.mynetworks` type (changed from `string` to array)

It is quite a bit more of renames than last year. I think locale changes
actually broke my locales at runtime and I had to figure out what to
change to get them back.

I did not have major package build failures that required any local
changes.

This time I had the following non-trivial problems in upstream packages:

- `duperemove` would hang up on `NoCOW` files: <https://github.com/markfasheh/duperemove/pull/376>.
  I had to bisect the regression and fix it upstream. It was easy as I
  am somewhat familiar with `duperemove` implementation.
- `nix` started stripping too much in `gcc-14` warning logs: <https://github.com/NixOS/nix/pull/13109>.
  It came up after `nixpkgs` switched to `gcc-14`. Took some time to figure
  out what is so special about `gcc` warnings. Ended up being quite easy.
- `perf` stated to hang up on my system: <https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=c21986d33d6beb269a35b38dcb8adaa5bd228527>.
  It came up after I ran system-wide profiler to see what `wine` does to
  use 100% CPU on an ancient game. THe fix was trivial once looked at.
  I don't remember what changed to expose the hangups. Maybe I just never
  did it before? Ended up being easy as well.
- `mpv` started failing GPU rendering due to `libplacebo` / `shaderc`
  incompatibility: <https://code.videolan.org/videolan/libplacebo/-/issues/335>.
  Most arcane bug of all: had to bisect the whole system to find the
  package first, then had to bisect down to the commit. Upstream eventually
  fixed it. But if `nixpkgs` had up-to-date `shaderc` we would not stumble
  on this bug. The only non-trivial bug from the whole list.

## Community support

I still feel that `NixOS` community is a welcoming place for newcomers,
experimenters and people who do grunt maintenance work. `NixOS` community
now had elected their first Steering Committee who can help resolving
high-level conflicts.

Some of the amusing things I did over the past year:

- [`nix` language non-determinism in `sort` built-in](/posts/330-another-nix-language-nondeterminism-example.html)
- `stdenv` fix to handle root directories that start with dash (`-`): <https://github.com/NixOS/nixpkgs/pull/317106>.
  `diffoscope-269` release was a great stress test for `nixpkgs` `bash` code :)
- Found and fixed ~50 more eval failures in `nixpkgs` found by [the hack](/posts/309-listing-all-nixpkgs-packages.html).
  This hack was also the trigger that exposed `sort` non-determinism above.
- Fixed `nixpkgs` `isMachO` helper: <https://github.com/NixOS/nixpkgs/pull/432097>.
  Reading 4 bytes from the file in pure `bash`. How hard could it be?

Just like last year I managed to get about 800 commits into `nixpkgs`
this year.

I stopped reading any Matrix channels completely and only skim through
[discourse](https://discourse.nixos.org/) and read `github` notifications.

## Home server experience

I did not have to adapt anything for the past year. Things still Just Work.

## Local experiments

I switched to [`helix` editor](/posts/331-trying-out-helix-editor.html)
and to [`chromium` browser](/posts/333-to-chromium.html). Both were quite
smooth transitions.

I continued `gcc` testing. This year it was `gcc-15` branch. `nixpkgs`
still manages to serve as a reasonable vehicle to
[find bugs](/posts/332-gcc-15-bugs-pile-2.html). Just like last year I
found about 50 compiler bugs. Did not manage to fix any myself.

## Parting words

`NixOS` still works for me.

Give `NixOS` a go if you did not yet :)
