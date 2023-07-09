---
title: "Two years on NixOS"
date: July 9, 2023
---

Yet another year has passed since I switched to `NixOS` as my main
desktop system. Time to look back and see how it went
([2022 instance](/posts/257-a-year-on-nixos.html)).

## System maintenance

I did not have any problems related to package build failures for any
amount of time on `nixos-unstable`. Probably because I did a bit of
testing when changes were in `staging`.

I had to deal with a few fallouts around configuration changes related
to upgrades:

- `fontconfig` font hinting default changed: </posts/283-fontconfig-on-160-DPI.html>.
  I had to set explicit defaults to restore font rendering.
- `pipewire` configuration layout changed and I had to slightly adapt
  my non-standard socket overrides: <https://github.com/NixOS/nixpkgs/pull/220332#issuecomment-1480119470>
- I followed rename of `boot.tmpOn*` options to `boot.tmp.*`.

All 3 were easy to track down and fix. It's a bit more maintenance than
last year's but still manageable.

## Community support

`NixOS` community remains to be a friendly place that welcomes
newcomers, experiments and day-to-day maintenance work. I have not seen
instances of large conflicts or abusive behaviours towards anyone.

I had a few chances to interact with wider community by attempting
larger-scale `nixpkgs` changes: I succeeded on one change and failed on
another change.

More successful change was
[enabling parallel installs by default](/posts/282-parallel-installs-in-nixpkgs.html).
It's a good example of a small change with small impact that touches
most packages. I managed to speed up quite a few slow-to-install
packages without major regressions. We have 32 `nixpkgs` packages marked
with
[`enableParallelInstalling = false`](https://github.com/search?q=repo%3ANixOS%2Fnixpkgs+%22enableParallelInstalling+%3D+false%22&type=code).
The rest should install in parallel (as long as a package builds in
parallel).

Less successful change was the initially minor
[bootstrap process change](/posts/275-nixpkgs-bootstrap-deep-dive.html).
It's a good example of a medium change with some impact on most
packages. I successfully pushed a few small cleanups but ended up
reverting larger changes. Those exposed bugs either in changes
themselves (`gcc` bootstrap is hard) or on incorrect assumptions of
downstream packages. This change is also very hard to communicate:
people tend to dismiss finer details of the change and don't see the
reason for a particular change. Maybe I'll try again next year :) In any
case I learned a lot in the process!

I follow Matrix channels even less than I did last year as I focus a bit
more on personal and select `nixpkgs` projects.

## Maintenance model

I think `nixpkgs` maintenance model still holds today. There are `RFC`s
like [RFC127](https://github.com/NixOS/rfcs/pull/127) to label
problematic packages in a more fine-grained form than just
`ok`/`broken`. Most updates are still done by a
[robot](https://github.com/NixOS/nixpkgs/graphs/contributors)
auto-updater.

Looking at
[commit activity](https://github.com/NixOS/nixpkgs/graphs/commit-activity)
`nixpkgs` gets about 1000 commits a week. That's a lot.

## Home server experience

Don't have anything to complain about here. The server required zero
maintenance from me this year as well.

The only thing I changed was the switch from `nixos-unstable` to direct
`master` branch from `nixpkgs` to get upstream kernel upgrades slightly
faster at the expense of local kernel build.

Fun fact: when I had a [cooling failure](/posts/280-cooling-failure.html)
on my main desktop system I switched to home server as my main machine.
It took a few config lines to turn a headless machine to wayland desktop
(and then back a week later when repair finished). As it's an old
machine I noticed how some builds are slower than they ought to be. This
prompted me to work on
[parallel installs](/posts/282-parallel-installs-in-nixpkgs.html).

## Local experiments

I switched my machine configurations to
[flake](https://nixos.wiki/wiki/Flakes)-based configuration to have
configuration update history in a single `/etc/nixos` `git` repository.

While at it I also switched my desktop from `nixos-unstable` channel
directly to `master` branch to get latest kernels and `firefox` as soon
as they are available. This means a bit of local compiling from time to
time.

I switched from `i3+x11` to `wayland+sway` as my main desktop window
manager. This exposed minor bugs in
[waypipe](/posts/265-waypipe-fixes.html) and even
[wine](/posts/268-fixing-wine-wayland-on-polaris.html)!

## Parting words

`NixOS` still works fine for me. Compared to last year I was able to do
larger projects within `NixOS` community (bootstrap, parallel builds,
closure shrinking, large package updates, ...) and outside `NixOS`
community (`gcc`, `wayland`, `xmms2`, `ski`, ...).

Give it a go if you did not yet :)
