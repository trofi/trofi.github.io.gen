---
title: "Five years on NixOS"
date: August 02, 2026
---

Another yer, another instance of my `NixOS` journey log
([2025 instance](/posts/337-four-years-on-nixos.html)).

## System maintenance

As usual I don't remember what I did to my system over the past year,
so I'm at the `git log` for `/etc/nixos` as it contains all the changes:

- I had to switch `nix` GC job from monthly runs to weekly runs: a
  month is enough to generate so much garbage that individual GC runs
  are too painful.
- follow rename of `linuxPackages.perf` -> `perf`
- temporary revert to older `nix` to avoid [CA-derivation regression](https://github.com/NixOS/nix/issues/14193)
- follow `services.resolved.extraConfig` -> `services.resolved.settings` move
- temporary revert to older `nix` to avoid [another CA-derivation regression](https://github.com/NixOS/nix/issues/15003)
- adapt to `'xorg.xrdb' has been renamed to 'xrdb'`
- adapt to `wineWowPackages` -> `wineWow64Packages` deprecation
- temporary revert to older `nix` to avoid [yet another CA-derivation regression](https://github.com/NixOS/nix/issues/15713)
- try out `niri` and `zellij`
- temporary revert to older `nix` to avoid [one more CA-derivation regression](https://github.com/NixOS/nix/issues/15839)
- added a kernel range exclusion hack for my broken RAM after motherboard
  switch

On `nixpkgs` side it was pleasantly low on churn. But `nix` managed to
break `ca-derivations` a lot. Even now it's barely usable with unevictable
partial traces for [unstable derivations](https://github.com/NixOS/nix/issues/15880).
I have to patch derivations manually time to time to get past broken
partial states.

I had some fun trying out new packages like `niri` and `zellij`. Using
both now.

Annoyingly I have a single bit flip in my [RAM](/posts/347-another-memory-corruption-case.html).
Luckily I managed to yank `4K` page out of usable space to avoid problems.
I know for sure that it occasionally caused build failures and broke files
freshly written on disk.

On top of that my motherboard died this year. After plugging in a
replacement I had to adjust `4K` hack for `RAM` as it's physical
location moved.

I managed to report a few fun problems to various upstream packages:

- `chromium` stopped detecting video acceleration on `sway` on `amd`
  card for me: <https://issues.chromium.org/issues/446395287>. I had a
  deja vu with a similar [bug in `wine`](/posts/268-fixing-wine-wayland-on-polaris.html).
  Both were fixed promptly \o/
- `binutils` `ld` was very slow at handling [thousands of input sections](https://sourceware.org/PR33530)
  and manifested as a slow linkage for [`ghc` binaries](/posts/340-profiling-binutils-linkers-in-nixpkgs.html).
  Was fixed quickly.

## Community support

I still feel that `NixOS` community is a welcoming place for newcomers,
experimenters and people who do grunt maintenance work.

This year I managed to get about 450 commits into `nixpkgs`, half the
size of last years' values.

I can barely keep up with new topics on <https://discourse.nixos.org/>.

## Home server experience

I did not have to adapt anything for the past year. Things still Just Work.

## Local experiments

I switched to [`zellij` terminal multiplexer](/posts/344-zellij-terminal-emulator.html)
and to [`niri` `wayland` compositor](/posts/351-trying-out-niri.html).
I'm happy with both.

I continued `gcc` testing. This year it was `gcc-16` branch. `nixpkgs`
still manages to serve as a reasonable vehicle to
[find bugs](/posts/348-gcc-16-bug-pile.html). This year I
found about 30 compiler bugs. About the half of past year. Did not manage
to fix any myself.

## Parting words

`NixOS` still works for me. I feel like I had a lot of issues with
`CA-derivations` and minimal bootstrap pain. If those keep getting in
the way of using the system I'll have to investigate the alternatives.
