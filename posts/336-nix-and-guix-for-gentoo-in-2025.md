---
title: "nix and guix for Gentoo in 2025"
date: August 19, 2025
---

Two years have passed since the [last issue](/posts/287-nix-and-guix-for-gentoo-in-2023.html)
of [`::nix-guix`](https://github.com/trofi/nix-guix-gentoo) overlay updates.
The overlay still ships latest `nix-2.30.2` and `guix-1.4.0`
packages. **One notable addition is `lix-2.93.3`!**

Our list of contributors over past 2 years is:

```
dependabot[bot]
G-Src
Jiajie Chen
Kris Scott
Sergei Trofimovich
Vincent de Phily
```

There are no major user-visible changes. But a few things to note are:

- `sys-apps/lix` was added to the family of `nix`-like package managers
- `sys-apps/guix` is not masked any more as `guile-3` was unmasked in
  `::gentoo`!
- old pre-`meson` version of `nix` are dropped
- `sys-apps/nix` does not enable fallback if user namespaces fail to
  initialize. This should guard users from accidentally building
  non-hermetic packages (they are very likely to break on Gentoo for
  various reasons)
- added `USE=allocate-build-users` to `sys-apps/nix` to use fully dynamic
  user builders (instead of requiring `acc-user/` set of static users)
- `sys-apps/guix` `ebuild` was ported to `guile-single.eclass`



Have fun!
