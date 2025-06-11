---
title: "Nix and Guix in Gentoo in 2022"
date: February 9, 2022
---

## A new home

In 2020 `sys-apps/nix` and `sys-apps/guix` were removed from
the main `::gentoo` tree into their own `::nix-guix` overlay:
<https://github.com/trofi/nix-guix-gentoo/>
The move makes the installation process slightly more cumbersome
than before ([`nix` in 2017](/posts/196-nix-on-gentoo-howto.html),
[`guix` in 2017](/197-guix-on-gentoo-howto.html)): overlays are
not the first thing users have to deal with.
Maciej moved setup basics out to overlay's own readme files:
<https://github.com/trofi/nix-guix-gentoo/#readme>. I added a standard
blurb on how one enables the overlay to make it slightly easier
for newcomers.
`::nix-guix` overlay is still safe to use on a daily basis on top
of stable or unstable Gentoo system: it provides the minimum of extra
packages needed to get any of both package managers running.

## Please contribute :)

**PSA**: I personally don't use the overlay on a regular basis as I don't
use `gentoo` as my main system. Thus bugs might creep in and stay there
for quite a while until I notice. Please file the
[issues](https://github.com/trofi/nix-guix-gentoo/issues) if you notice
something odd. Or just send the fixes straight away. These packages are
not that complicated. We'll try to figure something out.
I'll try to get a minimal CI locally but it will take some time.

## Some updates meanwhile

* `nix` is updated to latest `2.6.0` version.
  Normally it would be a trivial version update, but in case of `nix`
  `DESTDIR=` support does not quite work and needs a few lines of
  `Makefile` code: <https://github.com/NixOS/nix/issues/5781>.
* `nix-9999` and `guix-9999` `ebuilds` are in a reasonable shape for
  day-to-day use.
* `guix-1.3.0` was fixed to pull successfully `zstd` binaries from
  cache.

## Tip of the day

The other day I found out that `nixpkgs` already has a script to update
packages automatically to latest version available. For `github` based
projects it's a matter of adding one line:

```nix
passthru.updateScript = nix-update-script { attrPath = pname; }
```

Recent real world example is `re2c`: <https://github.com/NixOS/nixpkgs/pull/156972>
For others you can add arbitrary code to extract current version. I tried
it on `poke` recently: <https://github.com/NixOS/nixpkgs/pull/157108>
Then package update becomes trivial by running `nix-update`.

Enjoy!
