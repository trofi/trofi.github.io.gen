---
title: "-fno-common legacy"
date: May 13, 2022
---

## -fno-commn in ::gentoo

Around January 2020 (more than 2 years ago) **gcc** development version
switched the default from **-fcommon** to **-fno-common**: <https://gcc.gnu.org/PR85678>.

I personally like **-fno-common** as it catches accidental
global variable name collision like the one in
[spacenavd](https://github.com/FreeSpacenav/spacenavd/commit/7c271fa265613bd7d47601daaeb0e08e7c5b5a75)
or in **iozone**. Occasional upstreams'
[reluctance](https://github.com/FreeSpacenav/spacenavd/issues/17)
to make code **-fno-common**-compatible was a big surprise to me. I just
don't see any benefit of commoning. Luckily such a reluctance is rare.

I built local system with **-fno-common** toolchain and found 16 buggy
packages: **syslinux**, **tftp-hpa**, **minicom**, **freeglut**, **openrc**,
**iproute2**, **gpm**, **xorg-server**, **logrotate**, **gnupg**, **libtirpc**,
**gdbm**, **cpio**, **postfix**, **xfsprogs** and even **glibc**. Some are
very high profile packages. I had ~2K packages installed locally.

I extrapolated 16 failures per 2K packages to 20K packages
of the whole ::gentoo repository (~10x) and hoped that we won't get
more than 160 failures. My intuition was saying that most of other
packages should be **python**, **ruby**, **perl**, **php** packages
and are probably unaffeched. Thus the final number could be lower
than 100. That sounded like a manageable fallout.

When Toralf started **CFLAGS=-fno-common** **tinderbox** build test
on the whole ::gentoo repository he began discovering more failures.
I proposed fixes for first few tens of failure and was quicly overwhelmed
with more failure reports. After a few weeks of **tinderbox** run
the [bug list](https://bugs.gentoo.org/705764) collected ~800 failures!

800 is 5 times worse than I estimated. It's 4% of the repository. Some of those
reports are probably duplicates but vast majority are unique real failures.
If I knew it's so widespread I might have taken another way to roll it out.
Alas. Anyway, by now **-fno-common** is a fixed problem for Gentoo.

## -fno-common in nixpkgs

A few weeks ago I got commit access to **nixpkgs**. I'm still feeling a
bit uneasy about it as I don't formally maintain any packages there. At
least I have a few PRs to merge :)

Scrolling through pull requests I noticed that **nixpkgs** actually flipped
**gcc-10** (and **llvm-11**) default back to **-fcommon** to avoid widespread
breakage. And there was a PR to restore the default:
<https://github.com/NixOS/nixpkgs/pull/110571>. Having a bit more
collaborative tools at my disposal I decided to sort it out for **nixpkgs**.
I messed it up once before and learned something. Should be easy this time :)

**-fno-commn** default was relased as **gcc-10.1.0** on May 7. That
makes it almost exactly 2 years ago. Since then some projects adapted to
**-fno-common** uptream and released newer versions. But some did not.
What is the ratio of those?

For still broken packages I usually write a patch against dormant
upstream and attach it to the bug tracker so others could use patch
as is even if patch does not get merged in any form.

Sometimes there is no place upstream to make patch publicly available.
In this case I resort to **NIX_CFLAGS_COMPILE = "-fcommon"** in **.nix**
expressions. So far I had to do it in 10 packages (maybe 30% of all
I tried to fix?). I hope it will save some time to others.

If you are the author of a package that had no release in past 2 years
and have a few minor tweaks then consider releasing it. Maybe you even
have a **-fno-common** fix pending?

## Update from 8 July 2022

**-fno-common** change was merged into **nixpkgs**. Some stats:

- 240 packages were broken (down from 800 in Gentoo). Only 30% were
  still affected and 70% were already fixed upstream and trickled down
  to downstream.
- About 140 of unfixed packages got **-fcommon** workaround in **nixpkgs**.
  This means ~60% did not have any form of an upstream fix and 40% did
  (or do now).
- It took me 2 months to do it.

Have fun!
