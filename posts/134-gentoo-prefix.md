---
title: gentoo prefix
date: February 5, 2011
---

Once upon a time I've got a bunch of emails from `gentoo`'s bugzilla
about `haskell` prefix support:

- [`haskell-cabal.eclass` PoC changes](https://bugs.gentoo.org/show_bug.cgi?id=312595)
- [`ghc` `ebuild` PoC changes](https://bugs.gentoo.org/show_bug.cgi?id=312597)

The idea of prefix is quite fun. In one sentence it can be described as:

run `gentoo` in your `$HOME/gentoo/` directory on any host operating
system: `MacOS`, `Linux`, `Solaris`, etc.

I tried to read [the
notes](https://www.gentoo.org/proj/en/gentoo-alt/prefix/techdocs.xml)
about porting ebuilds to support it and was seriously confused by
`$D` vs `$ED`, `$ROOT` vs `$EROOT` and `$EPREFIX`
variable meanings.

So I gave up on it for extended time and returned back to prefix this
week.

I've decided to solve my `$ROOT` vs. `$EROOT` confusion and to
try to merge `haskell` related changes for `prefix`. They are very useful
after all: you could hold various `ghc` versions without need to keep
full-fledged `chroot`. Say, `prefix-ghc612` user could hold
`ghc-6.12.3` in his `$HOME/gentoo/` and `prefix-ghc700` user
could hold currently unstable `ghc-7.0.1`. So you just issue:

    $ sudo su - prefix-ghc612 # or prefix-ghc700

and get needed `ghc` and it's environment.

So far I've decided to actually install prefix. It was quite simple.
With great help from `grobian` and [`solaris`
HOWTO](https://www.gentoo.org/proj/en/gentoo-alt/prefix/bootstrap-solaris.xml)
I\'ve bootstrapper prefix. There were some minor deviations from the guide: I
had to install `coreutils6` instead of `coreutils` and `tar22` instead
of `tar`. The rest of stuff was smooth.

Then I once again started to look the `"porting to prefix"` so called
[`techdoc`](https://www.gentoo.org/proj/en/gentoo-alt/prefix/techdocs.xml).
The thing, which annoyed me is a lost meaning of commonly used
`$ROOT` and `$D` variables in `ebuilds`. Prefix team seds them out
to `$EROOT` and to `$ED` almost in every place of every `ebuild`!
(exceptional cases are to be described below)

So, what are those `E*` variables? Basically the sole difference from
usual `gentoo` installation is slightly changed absolute path for all the
packages. The prefixed `gentoo` root has `EPREFIX` variable name.

An example:

Having `EPREFIX=$HOME/gentoo/` you will have (say) `gcc` installed in
`$EPREFIX/usr/bin/gcc` (aka `$HOME/gentoo/usr/bin/gcc`).

In order to achieve it portage should call not
`./configure --prefix=/usr` but `./configure --prefix=$EPREFIX/usr`. And
that's all!

Except you need to adjust all the `ebuilds`, which use absolute paths to
use `$EPREFIX`. An example:

    # somewhere in pkg_setup() on gentoo-x86 tree
    [[ -e /usr/bin/python2 ]] && einfo "Whoa, you have python"

    # on prefix tree should be changed to
    [[ -e $EPREFIX/usr/bin/python2 ]] && einfo "Whoa, you have python"

Everything else should use relative path and do not suffer from change
at all!

But! There is one serious culprit. Under prefix `$ROOT` is a
meaningless variable! There is `EROOT=$ROOT/$EPREFIX` to mean `gentoo`
root.

Let's see at the following merge session:

    >>> Completed installing less-440 into /home/prefix/gentoo/var/tmp/portage/sys-apps/less-440/image/home/prefix/gentoo/

    strip: i686-pc-linux-gnu-strip --strip-unneeded -R .comment
       usr/bin/lessecho
       usr/bin/lesskey
       usr/bin/less

The `$D` variable is
`/home/prefix/gentoo/var/tmp/portage/sys-apps/less-440/image/`, and if
you plan to use any modifications in the image you should use
`${D}${EPREFIX}` as an absolute path. Like the following:

    make DESTDIR="${D}" install
    strip ${D}${EPRFIX}/usr/bin/less

For simplicity there is `$ED` variable `ED=${D}${EPREFIX}` (I
would call it `$DE`, which would have shortening of: `D` +
`E`(prefix)). So you can use the following to access to the staging
area:

    make DESTDIR="${D}" install
    strip ${ED}/usr/bin/less

Ok, once again: `$D` points to the sandbox root for a staging install
(as it always was). My apologies to `darkside` and `grobian` for not
getting right it from the very start.

And `$EROOT` is for prefixed root to access things like:

- `preserve_old_libs`
- `/var/db/pkg`
- update fonts caches
- configure vim plugins
- root for `/usr/src/linux` source and modules installation path
- gnome schemas (no idea what is it, but sounds host OS unrelated)
- logs path offset
- `mysql` bases offset
- `/etc/`

What about the `$ROOT` then? And here we have a problem. Under prefix
`$ROOT` is not our prefixed gentoo's root, but host's root (aka
`"/"`)! Gah! Why would we need to know about it at all? And there is
rare cases when we need it:

- very rarely you have to rely on host's environment:
  - critical things used by prefixed gentoo (host's devices directory
    for example)
  - host's libraries (like `libSM` `ebuild`)
    which prefix *has* to use: authentication is one of examples
- think about poor user running something like `ROOT=/foo emerge bar`.
  He should get `/foo/$EPREFIX/usr/bin/less` in the end and nothing
  else.

The short guideline for prefix porter:

- always change `$ROOT` to `$EROOT` if the package is not tangled
  to host
- make sure you understand where `$D` (sandbox root) and `$ED`
  (`gentoo` root in sandbox) point to and use them wisely
- don't forget to add `$EPREFIX` when absolute names are used
