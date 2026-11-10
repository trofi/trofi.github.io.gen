---
title: "Zero Hydra Failures towards 26.11 NixOS release"
date: November 10, 2026
---

PSA: `NixOS-26.11` is about to be released by the end of
[November](https://github.com/NixOS/nixpkgs/issues/567567). Release process
is in `ZHF` phase now where no major changes are allowed in `master`
and most known failures are being fixed now.

[`ZHF` issue](https://github.com/NixOS/nixpkgs/issues/584347) has a nice
step-by-step on how to find and fix a broken package.

I'll not provide complete session compared to
[past example](/posts/349-Zero-Hydra-Failures-towards-26.05-NixOS-release.html)
and will link a few `ZHF` fixes for this cycle here:

- [`h5v`](https://github.com/NixOS/nixpkgs/pull/585673)
- [`sapling`](https://github.com/NixOS/nixpkgs/pull/585130)
- [`fedistar`](https://github.com/NixOS/nixpkgs/pull/584720)
- [`fceux`](https://github.com/NixOS/nixpkgs/pull/584718)

All fixes are one-liners.

It's a good time to contribute to `nixpkgs` if you never did and are
thinking of it.

Have fun!
