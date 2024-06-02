---
title: "Three years on NixOS"
date: June 2, 2024
---

This year I decided to shift yearly updates on my `NixOS` endeavours
([2023 instance](/posts/290-two-years-on-nixos.html)). This time
the occasion is [`NixOS 24.05 release`](https://nixos.org/blog/announcements/2024/nixos-2405/).

## System maintenance

Looking at the `git log` for `/etc/nixos` for the desktop system I see
the following things happening over the past year:

- follow `fonts.fonts` to `fonts.packages` rename (added in [PR#244332](https://github.com/NixOS/nixpkgs/pull/244332))
- follow `pipewire` migration to `extraConfig` (added in
  [PR#282377](https://github.com/NixOS/nixpkgs/pull/282377)).
- follow `programs.gnupg.agent.pinentryFlavor` to
  `programs.gnupg.agent.pinentryPackage` migration (added in
  [PR#133542](https://github.com/NixOS/nixpkgs/pull/133542)).
- follow the rename from `nix.unstable` to `nix.latest` (added in
  [PR#305951](https://github.com/NixOS/nixpkgs/pull/305951)).

All four were trivial to tweak and did not cause much confusion.

Similar to previous year I did not have any problems related to package
build failures for any on `nixos-unstable`. Again, probably because I
tested `staging` time to time.

This time I had two non-trivial problems in upstream packages:

- problematic `firefox` update broke `meet` video streaming. It was
  triggered by the use of `--with-system-libvpx` option in `nixpkgs`. That
  was one of the cases where I had the luxury of bisecting the whole
  system to pinpoint the bad component:
  <https://github.com/NixOS/nixpkgs/pull/283010#issuecomment-1925703583>.
  Local revert until the fix was shipped was trivial.
- problematic unstable `6.8` kernel upgrade caused kernel panic in
  `eevdf` scheduler. Was fixed upstream in
  <https://lore.kernel.org/lkml/ZicOSiEWHJJcahi%2F@yujie-X299/t/> around
  `6.9` kernel. The crashes were nasty as scheduler crash locks up the
  machine and does not print anything when it happens. Luckily
  `systemd` recovered crash log from `EFI`s `nvram` and it was clear
  from the backtrace that it's a bug related to scheduling kernel
  subsystem.

## Community support

`NixOS` community remains to be a friendly place that welcomes
newcomers, experiments and day-to-day maintenance work. This year
`NixOS Foundation` received some heat for how it governs some aspects of
the community. `NixOS Foundation` proposed a
[few major changes](https://discourse.nixos.org/t/nixos-foundation-board-giving-power-to-the-community/44552)
on how it will operate in future.

The most important (and hardest organizationally) change I did was to
document and exercise the procedure of updating
[bootstrap binaries](/posts/315-nixpkgs-bootstrap-files-update.html) in
`nixpkgs`.

My fanciest contribution was
[my failed attempt](/posts/309-listing-all-nixpkgs-packages.html) at
listing "all" the package attributes available in `nixpkgs`. While I was
not able to list all the attributes initially I managed to derive about
60 fixes to `nixpkgs` (all linked in the article) to make future listing
smoother. Tl:DR; of the fixes is: dynamic typing is hard.

The most unusual `nixpkgs` contribution was to find
[the non-determinism](/posts/292-nix-language-nondeterminism-example.html)
in `nix expression language` itself. It's not something I expected to
encounter in real code. Alas.

The trickiest from technical standpoint was the fix for
[parallel strip breakage](/posts/293-mysterious-patchelf-linkage-bug.html).
[`-Ofast`](/posts/302-Ofast-and-ffast-math-non-local-effects.html) and
[`libpam`](/posts/310-a-libpam-bug.html) bugs were also fun.

The most satisfying was to
[reduce the runtime closure](/posts/298-unexpected-runtime-dependencies-in-nixpkgs.html)
for many packages that use `__FILE__` just for for debug messages.

Surprisingly I managed to get about 800 commits into `nixpkgs` this year.
About ~90 of them is fixes to get compatibility with `gcc-13`. About 60
are evaluation fixes mentioned above. About 400 of them are various
package version updates.

I still read some of Matrix channels but I mostly skim through
[discourse](https://discourse.nixos.org/) as I have even less free time
than last year.

## Home server experience

I did not have to adapt anything for the past year. I switched from
`apache` to `nginx` as an `httpd` without any issues. And that's about
it. Things Just Work.

## Local experiments

As an experiment I gave [`hyprland`](https://hyprland.org/) a short try.
I had to switch back to `sway`. I had two issues with
`hyprland`: new applications are visibly changing layout a few times
before they settle on final window size (I could not get used to it) and
configuration language quirks (I frequently missed commas where empty
arguments are required).

I also did a bit of fresh `gcc` testing. This time frame also coincided
with `gcc-14` development and release cycle. `nixpkgs` ended up being a
reasonable vehicle to play with `gcc-14`. The
[last bug pile report](/posts/311-gcc-14-bug-pile-4.html) tells me that
I found about 50 `gcc` bugs and even fixed at least
[one non-trivial one](/posts/301-another-gcc-profiling-bug.html).

## Parting words

`NixOS` still works fine for me. I did not do as much as I managed to
last year. But looking back the list looks impressive.

Give it a go if you did not yet :)
