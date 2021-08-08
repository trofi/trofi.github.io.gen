---
title: "farewell, gentoo dev"
date: August 8, 2021
---

:PostID: 228
:Title: "farewell, gentoo dev"
:Keywords: gentoo
:Categories: notes

PSA: I asked my slyfox@gentoo.org account to be retired.

a bit of history
----------------

Here is a bit of my history with gentoo (and Linux as it overlaps for 90%).

I started using Linux in 2003. It was an Alt Master 2.2(ish)
distribution (russian mandrake sibling). Year later quickly degraded (or
ascended) into BLFS.

I became gentoo user around 2005. I was an undergrad and not a fully
grown up person. Arguably I'm not yet either. Let's check in 16 years.

To get a feeling of what I was like here is my first (well, second)
email sent from gentoo box:

.. code-block::

    Date: Thu, 3 Nov 2005 22:11:50 +0200
    From: Sergei Trofimovich <slyich@gmail.com>
    To: gqview@users.sourceforge.net
    Subject: feature request
    Message-ID: <20051103221150.460d3cbf@SlyFox.SlyNet.org>
    X-Mailer: Sylpheed-Claws 1.9.99 (GTK+ 2.8.6; i686-pc-linux-gnu)
    Mime-Version: 1.0
    Content-Type: text/plain; charset=US-ASCII
    Content-Transfer-Encoding: 7bit

    Hello.
     I use GQview for a long time :], but i would like to see in 
     it _MOVING_ .GIFs. Thanks a lot!!! 

I think I was asking for animated .gif support in gqview.

Message-ID reminds me I had an odd notion of the way one claims their
own domains in internet. I pretended SlyNet.org belongs to me at least
in my local network of one computer.

I had just destroyed my main LFS system with
a **./configure && make && make install** of fresh weekly glibc CVS
snapshot. I set dual boot of Debian and gentoo to try those out until
I restore LFS.

I think I did my first meaningful contribution to nouveau project when they
collected BIOS dumps for video cards. At that time you would need to patch
your kernel with Pekka's MMIO trace support and run glxgears on Nvidia's
binary driver. Then run a script to generate C-looking BIOD dump.

If you paste it to nouveau kernel driver it would be enough to get your
card running under nouveau. Getting if to work for the first time for was
a great feeling!

Around 2008 I got into #gentoo-haskell trying to build https://wiki.haskell.org/Lambdabot
and trying to make any sense off haskell by looking at the regression tests
in GHC tree. Internet was still a dial-up thing for me at home.

One of first non-trivial bug reports I did was missing **AC_LARGEFILE**
in some parts of GHC. It was an inconsistent getrlimit struct size
and corrupted memory as a result: https://gitlab.haskell.org/ghc/ghc/-/issues/2038

Around 2009 I was on board of midnight commander development team, got hackage
upload right, adopted abandoned **fquery** tool and subscribed to lkml to read
it every morning on train to work and back (2 hours per day). I also sent my
first non-trivial bug report to kernel around i915 chip and a month later got
my first trivial commit to linux kernel:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=168f5ac668f63dfb64439766e3ef9e866b83719d

I aspired to get at least one commit to linux, gcc, glibc and binutils
one day. I never dreamed of commit bit to those projects. They all looked
complex, magic and flawless. I thought the time order would be (in order
of perceived to be required knowledge): binutils, glibc, gcc, linux.
It ended up being exactly the opposite and has very little to do with knowledge
or complexity.

Also around 2009 I (as upstream) received first contribution to mc's part
I am responsible for: ebuild file syntax highlighting in mcedit (a bunch
of keywords). It was from Lars, gentoo staffer by then. I though, wow,
being a gentoo dev is very cool! Maybe one day I'll have a chance? I did
an **mc** live ebuild for my own use after all!

And by the end of 2009 Lennart mentored me as a new gentoo dev to help him
with haskell packages: https://bugs.gentoo.org/296463. In 6 months I got
the CVS commit bit! My first ebuild was an **xmms2** one. Mike helped me
to shape it up in gentoo-dev@ ML.

It took me about 6 years to start meaningfully contribute to the FOSS
community. Such a long time. If you think of contributing and have not
started yet then start today. It is trivial and fun.

Gentoo gave me access to various exotic platforms. First thing first I tried
to refresh GHC binary on alpha, ia64, powerpc and sparc. GHC required fancy
fixes on each of them. In case of sparc Mike fixed glibc for me first:
https://bugs.gentoo.org/336792#c13. Years later I was able to debug similar
bugs PIE-related myself.


Roughly around that time Google contacted me for the first time and I failed
the onsite interview in Zurich. That was my first time to visit english
speaking Europe.

In 2010 a good friend of mine gave me sheevaplug armv5tel device to play with
embedding gentoo into it. He helped me to file https://bugs.gentoo.org/333679.

In 2011 I managed to debug and fix very scary data corruption bug on btrfs:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=3387206f26e1b48703e810175b98611a4fd8e8ea
and then severe degradation on \-\-mixed btrfs filesystems:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=c4f675cd40d955d539180506c09515c90169b15b

On alpha in attempt to follow some outdated guide in alignment debugging I
accidentally fixed some very obscure **setsysinfo()** interface nobody seemed
to use:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=2df7a7d1cd07626dd235ca102830ebfc6c01a09e

That helped me to find a bug on alpha in OPENSSL_cleanse() function. My first
encounter of openssl's perl scripts to generate the assembly.

Only later I found out that standard **prctl \-\-unaligned=signal** Just Works
for the same purpose :)

In the same 2011 I mentored Mark to get on board with gentoo dev to help me
with haskell.

In the same 2011 I tried to fix a few prominent packages to support **x32**
ABI and stumbled on xorg devs who refused to accept that **__x86_64__** can
be an **ilp32** system. It was eventually accepted upstream but I gave up
quickly being blocked by xorg.

In 2012 at a day job I got to work at an x86 emulator and extended my interests
to **qemu** and interpreters. Few trivial fixes landed into **qemu**. I also
botched the qemu ebuild down to a state when qemu VMs could not boot anymore.
Doug was rightfully upset. I settled on butchering my own live ebuild.

About the same time I started taking on maintenance of other tiny packages,
like **sys-block/seekwatcher**, **app-misc/bb** and other non-haskell stuff.

I mentored Alexander on board of gentoo dev to help me with haskell packages.

In 2012 Google contacted me second time and I failed phone interview for a
Haskell position in Munich.

At the end of 2012 I mentored Heather to join gentoo devs and help me with
haskell. Heather also maintained C#-related packages that required very
special expertise nobody except Heather had.

I submitted my first tmpfilesd.eclass in 2012 for review and it was shot
down as unneded. Only to be added in 2016 by someone else. There are my
communication skills at that time (and now, really).

In 2013 my mentor Lennart retired from gentoo dev and moved on to Fedora.

I fixed a memory leak in long-running **CVS** sessions which allowed
me to convert whole of gentoo's CVS tree into git as a single **git cvsimport**
run.

I took on GHC maintenance in gentoo: building binaries on a few stalled
arches, fixing obscure GHC or haskell bugs that happened only on exotic
arches. I found out all the gory details of how RTS adjustors worked
and how libffi was (slightly incorrectly) hooked to it.

Later I took on cross-compiler cleanup and maintenance of upstream GHC
around unregisterised backend. It was a great way to understand GHC's
evaluation model and unique debugging tricks.

I upstreamed by Most Important Ever patch to linux kernel to enable
**-Werror=implicit-int** prompted by hardened-specific backport gone wrong
I had on btrfs:
https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=80970472179a45609c0b11b80619bc8c32b15f77

Reminds me I used a hardened kernel at work at that time and fixed a few
packages for it. Somehow it was the simplest way to fix hardened-specific
bugs. Many hardened users frequently refused to show any build.log and
described the problems as they see it without actual evidence.

I mentored Michael to join gentoo devs to work on various packages.

I was contacted by Google yet again and finally passed the interview.
In 2014 I moved to London. My first commit to gentoo's portage also
happened that year.

I did my first substantial change to ghc-package.eclass to support new
style package database and while at it got rid of many orphan haskell
config files on gentoo. That was probably my first successful eclass work.

In 2015 I subscribed to gentoo-project@ because I was nominated for council@
for the first time /o\\. I did not really know what happens in that list
until then as it never popped up in a day-to-day work.

I unsubscribed from lkml as I had no time to read after the move.
I subscribed to libc-alpha@ (glibc mailing list). I got my first
gcc patch upstreamed to properly fix **ia64** relocations:
http://trofi.github.io/posts/189-glibc-on-ia64-or-how-relocations-bootstrap.html

gentoo moved from CVS to git which made it a lot easier for external users to
contribute. It felt like development speed accelerated quite a bit since.

In 2016 I fixed GHC on m68k to mostly test how easy it is to
cross-compile GHC on something I never tried before:
http://trofi.github.io/posts/191-ghc-on-m68k.html

In 2017 I fixed kernel module loading on **ia64** that broke by my gcc
patch from 2015: http://trofi.github.io/posts/199-ia64-machine-emulation.html

I also was elected as a member of gentoo council@ for the first time. It
was an eye opening event: I had only vague idea what council actually does,
yet I nominated. I wonder if most of devs generally have as much feeling
about it. That's a scary thought on how election process actually go and
what it achieves. It also explains why I was elected at all :)

Mike stopped contributing to toolchain packages. I joined newly formed
toolchain@ team in gentoo to maintain gcc. At that point I got some
expertise to fix GHC on various arches and was a very frequent user of
crossdev. This naturally exposed me to very rare arch-specific
cross-specific build bugs. First gcc I pushed to gentoo was gcc-6.4.0.
First major gcc was probably a gcc-7. I don't remember anything special
about it. Probably because I had no idea what I was doing.

Later I found out about nix and guix as an elegant solution not to break
your existing system while building an update to the new one. It felt
a bit clunky as a gentoo replacement. It feels like the right solution,
but it also requires quite a bit of time investment.

Later gentoo enabled 17.0 profiles with pie-by-default. I hoped (and
asked) that clang, go, ocaml, crystal would follow gcc's lead of -fPIE
byt defaults (and -fstack-protector while at it). But it never happened.
It keeps biting users and keeps providing inconsistent results when trying
to mix the binaries and libraries from different toolchains.

In 2017 I was kicked out of #gentoo-dev IRC channel over a seemingly
minor issue. It was not an isolated incident. By the time it was clear
part of gentoo dev community had different views and values from mine
on what is appropriate in casual conversations. I realized it was a big
effort to sift through bile and snarky comments on #gentoo-dev in search
of something constructive. I never came back.

I did not feel my contributions were welcomed at the time and started
thinking of resignation. New toolchain@ and council@ roles cheered me
up slightly and allowed me to distract from the thought.

That was the time when I could no longer safely expand my interests in
gentoo. I started explicitly avoiding quite a few areas and distance
myself from very toxic environments.

I realized I'll eventually lose the connection with gentoo development
in general and fall behind the development practices. In this regard
I was probably the worst council@ member ever :)

sparc architecture support went from stable to exp for a short while
as we lost our last sparc dev box from HDD hardware failure.

I think demoting to exp was a good move. It signalled people to step in
and save the platform support by getting new fancy hardware, by setting
it up and starting more active stabilization process by. Rolf++ saved
sparc and hppa. He still diligently files bugs that ought to be filed
and fixed by maintainers themselves. I think over time we found a few
non-trivial bugs that benefit every arch as a result.

I personally think portability is a great asset of gentoo. Mechanically
it's a great way to find future bugs. https://bugs.gentoo.org/613418 is
a good example when unexpected memory overlap in inplace arithmetics on
long numbers caused problems on sparc first but could (and will) happen
on x86_64.

In 2018 I debugged very fancy hardware memory fault on my machine related
to non-temporal instructions handling:
https://trofi.github.io/posts/209-tracking-down-mysterious-memory-corruption.html

By then I saw everything :)

A bit later Alexander retired from gentoo and moved on to nixos ecosystem.

In 2019 I removed 13.0 profiles as their presence slowed repoman down
and gave an impression of 13.0 to stay forever while devs did not
normally test software on it: https://bugs.gentoo.org/672960. As a result
we found out infra used 13.0 as well. Whoops.

In 2019 I joined riscv project to help with basic toolchain support.
I think I only made a minor glibc tweak.

In 2020 I started working on gcc-10 which (who knew!) lexicographically
is less than gcc-9 and that broke software in very unusual ways:
http://trofi.github.io/posts/213-gcc-10-in-gentoo.html

As you can see in that post gcc-10 was very harsh on it's users.
It took us a long time to get reverse dependencies fixed. To isolate users
from simplest bugs and be able to discover breakages early I decided to
switch my main development box to gcc built from git. I hope I succeeded
at catching one or two of those before the release:
http://trofi.github.io/posts/224-a-year-on-gcc-from-git.html

My goal as part of toolchain@ was to clean up and forward all gcc-related
failures that looked like compiler problems: be it LTO, PGO, exotic
-m\* or -f\* flags being used or cross-compiler support. It's a joy to
see enthusiasts try out various fancy setups I could never come up with
myself and get it to work together.

And it's always sad to see when people just disable certain optimizations
in ebuilds without a bug report or any specifics. Almost always there is
a proper fix lurking on toolchain side, client project or both.

To help Agostino find packages that don't follow ${CHOST}-${tool} convention
I added USE=-native-symlinks to gcc-config and binutils-config:
https://wiki.gentoo.org/wiki/Project:Toolchain/use_native_symlinks

This tiny effort shown an interesting detail of gentoo dev community:
everybody has slightly different notion of what gentoo provides as an
interface to the user.

This specific example is how many variables should user override
in make.conf to get CC applied? Just CC? Or also CC_FOR_BUILD?
Or maybe HOST_CC? How about CHOST override? Should it affect CC
automatically or user also has to specify CC?

My optimion is seemingly very simple: for native builds ${CHOST}-gcc
should be used for all build systems to (until overridden) But not
everyone shares it. Mismatch causes cross-compilation failures on a
regular basis.

Ideally QA would help us here to establish some guidance. Any
written convention would be fine. But that did not happen yet:
https://bugs.gentoo.org/726034.

In 2020 I got commit bit in gcc, binutils and llvm projects for a few
small contributions. That gave me the confidence to contribute more.
That looks like a great model.

In 2021 Heather retired from gentoo dev.

Wolfgang (my mentee) was rejected as a gentoo dev candidate.
This was my failure as a mentor. It's a sign I'm not up to speed
with current gentoo development practices and should step down.

I unassigned myself from all the packages I left all the gentoo teams.

Losing access to exotic arches if a bit unfortunate, but maybe it will
force me to improve **qemu** a bit. Or get hardware access via other
means :D

Possible improvements
---------------------

On developer pool size and their will to contribute. I personally think
quizzies cover both too much and too little of the scope to evaluate the
candidate for an ultimate question if having them onboarded now would be
net benefit or not. For example ::haskell occasionally gets active and
diligent contributors that don't have much interest outside haskell
packages. I don't see a reason not to allow them to sync their work to
::gentoo.

On information flow around gentoo-wide. council@, trustees@, qa@, infra@
topics like new policies or decisions being made. Would be nice to always
post those to gentoo-dev-announce@. As a crazy idea it might help gentoo
if each new member joined over past 2 years would personally be invited
to lurk in one of council meetings to get the idea what they are usually
about. And get meeting logs back over the email in case they could not
make it. For example I still have no idea what decisions qa@ did over
past 5 years.

On policy clarificaiton requests against qa@. Perhaps monthly meetings
could go through bug backlog to have follow-up and steps to closure.

On lack of basic tooling or tool fragmentation. I think gentoo needs more
trivial ubiquitous tools:

- tool to file a bug report based on failed build.log without much manual
  fiddling. Or even better a portage prompt when encountered.
- tool to file a stabilization (or keywording) request for users
- tool to continuously perform builds for CCed arches in bugs and have the
  ability to keyword/stable packages without manual interaction from arch team
  (unless requested explicitly). It might also be a hatural place to maintain
  some form of binpkgs in gentoo.
- tool to setup and enter chroot
- tool to run the QA checks against all the packages one maintains without
  resort to hacking up their own big wrappers.
- many other tools for daily use I forgot

Most of them are implementable in 5 lines of code as a start. It would be
a great start to improve quality of reports and interaction.

Parting words
-------------

I'll probably still be around for a while as a gentoo user. No more **ia64**
access though :)

These past 11 years had their nice moments.

Good luck!
