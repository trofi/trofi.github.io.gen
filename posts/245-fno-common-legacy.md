---
title: "-fno-common legacy"
date: May 13, 2022
---

Around January 2020 (more than 2 years ago) **gcc** development version
switched from **-fcommon** to **-fno-common** by default:
<https://gcc.gnu.org/PR85678>.

I personally like **-fno-common** default as it catches accidental
global variable sharing like the one in
[spacenavd](https://github.com/FreeSpacenav/spacenavd/commit/7c271fa265613bd7d47601daaeb0e08e7c5b5a75)
or **iozone**. Occasional upstreams'
[reluctance](https://github.com/FreeSpacenav/spacenavd/issues/17)
to make code **-fno-common**-clean was a bigsurprise to me. I just don't
see any benefits of commoning like that. Luckily such reluctance is rare.

When I built my system with **-fno-common** toolchains I found 16 buggy
packages: **syslinux**, **tftp-hpa**, **minicom**, **freeglut**, **openrc**,
**iproute2**, **gpm**, **xorg-server**, **logrotate**, **gnupg**, **libtirpc**,
**gdbm**, **cpio**, **postfix**, **xfsprogs** and even **glibc**.

Some are very high profile packages. I would not expect such bugs in them.
My excuse for myself was that those are ancient code bases. Perhaps they
were initially written in a style that did not expect large size of project
and were never cleaned up later?

I had ~2K packages installed locally. I extrapolated 16 failures to 20K packages
of the whole repository (~10x) and hoped that we won't get more than 160
failures. My intuition was saying that most of those packages should
be **python**, **ruby**, **perl**, **php** packages (probably unaffeched)
and thus the final number would be lower than 100.

When Toralf ran **CFLAGS=-fno-common** **tinderbox** build test
on the repository he started discovering more failures. I proposed fixes for
first few ten and was quickly overwhelmed. After a few weeks of **tinderbox**
run the bug list
contained ~800 failures! That is 5 times worse than I expected. Some of those
reports are probably duplicates but vast majority are unique real failures.
If I knew it's so widespread I might have taken another way to roll it out.
Alas. Anyway, by now **-fno-common** is a fixed problem for Gentoo.

A few weeks ago I got commit access to **nixpkgs**. I'm feeling a bit uneasy
about it as I don't formally maintain any packages there. At least I have
a few PRs to merge :)

Scrolling through pull requests I noticed that **nixpkgs** actually flipped
**gcc-10** (and **llvm-11**) back to **-fcommon** to avoid widespread breakage
and there was a PR to restore the default:
<https://github.com/NixOS/nixpkgs/pull/110571>. Now having a bit more
collaborative tools at my disposal I decided to sort it out for **nixpkgs**.
I did it once before :)

**gcc-10.1.0** was released on May 7. That makes it almost exactly 2 years ago.
Some projects incorporated **-fno-common** fixes uptream and released
newer versions. But some did not. What would be the ratio of those?

So far I found **80** still broken packages in **nixpkgs**:
[incomplete list](https://github.com/NixOS/nixpkgs/pull/110571#issuecomment-1119343199).
From the list we can see that upstream mostly caught up.
That is only 10% from what we saw 2 years before. The caveat is that
it's also an incomplete list of failures. It's what I managed to build
in past 4 days. Once we sort most of there out I'll request one full
hydra run.

We can see that vast majority of high-profile packages are fixed and
released. But there still are counterexamples like **cpio** (fixed in
git, no release yet) and **syslinux** (no fix upstream).

Some of packages are completely abandoned and should be removed like
a standalone **dirmngr** package.

For still broken packages I usually write a patch against dormant
upstream and attach it to the bug tracker so others could use patch
as is even if patch does not get merged in any form.

Sometimes there is no place upstream to make patch publicly available.
Then I have to resort to **NIX_CFLAGS_COMPILE = "-fcommon"** in **.nix**
expressions. So far I had to do it in 10 packages (maybe 30% of all
I tried to fix?). I hope it will save some time to others.

If you are the author of a package that had no release in past 2 years
and have a few minor tweaks then consider releasing it. You might save
some time for others. Maybe you even have a **-fno-common** fix pending?
:)

If you'd like to contribute to **nixpkgs** consider fixing a
**-fno-common** bug from the list above :) TO get you started the
reproducer is usually simple. Something like:

```
$ nix build -L --impure --expr 'with import <nixpkgs> {}; jfsutils.overrideAttrs (oa: { NIX_CFLAGS_COMPILE = (["-fno-common"] ++ [oa.NIX_CFLAGS_COMPILE or ""]); })'
...
jfsutils> /nix/store/rs684lgm8k7akkgbisb49z4vpxxc2zns-binutils-2.38/bin/ld: extract.o:/build/jfsutils-1.1.15/fscklog/extract.c:67: multiple definition of `xchklog_buffer'; display.o:/build/jfsutils-1.1.15/fscklog/display.c:57: first defined here
jfsutils> collect2: error: ld returned 1 exit status
jfsutils> make[2]: *** [Makefile:373: jfs_fscklog] Error 1
jfsutils> make[2]: Leaving directory '/build/jfsutils-1.1.15/fscklog'
jfsutils> make[1]: *** [Makefile:363: all-recursive] Error 1
jfsutils> make[1]: Leaving directory '/build/jfsutils-1.1.15'
jfsutils> make: *** [Makefile:302: all] Error 2
error: builder for '/nix/store/cp4wavdy1x3rpbswzx141g794m0qsca7-jfsutils-1.1.15.drv' failed with exit code 2;
```

That should get you started. See <https://discourse.nixos.org/t/help-disabling-fno-common-hack/19031>
for more details.

Have fun!
