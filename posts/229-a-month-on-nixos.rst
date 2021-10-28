---
title: "A month on NixOS"
date: October 11, 2021
---

:PostID: 229
:Title: "A month on NixOS"
:Keywords: nixos, nix, nixpkgs
:Categories: notes

About a month ago I decided to give NixOS a try on my main desktop. I was
slightly worried I could not use it due to lack of software I usually use.
Thus I installed it into a btrfs subvolume along with existing system.

Installation
============

The installation process is very lightweight if you already have **nix**
package manager in some form. Otheriwse you can boot from from KDE-based ISO
image. Here my full installation procedure log:

.. code-block::

    # btrfs su cr /nixos
    # nixos-generate-config --root /nixos
    # $EDITOR /nixos/etc/nixos/configuration.nix
    # nixos-install --root /nixos

The only thing I added to **configuration.nix** was a new root:

.. code-block::

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa";
      fsType = "btrfs";
      options = [ "subvol=nixos" "noatime" "compress=zstd" ];
    };

That was surprisingly smooth.

Once I got something that boots tweaking system was also straightforward:

.. code-block::

    # from configuration.nix
    ..
    services.openssh.enable = true;
    services.unbound.enable = true;
    #
    zramSwap.enable = true;
    zramSwap.memoryPercent = 150;

Here I flipped on **openssh**, **unbound** and **zram**. Those flags
control what contents generated files will have in `/etc`. It's a tiny
layer of indirection.

Of already available packages I lacked only **xmms2** which was
trivial to package.

Exotic platforms
================

How about getting binaries for other targets?

**nixpkgs** package repository makes it trivial to add new platform
support as a cross-compilation target.

For example it takes 5 simple lines of "code" to add **s390x** support:
https://github.com/NixOS/nixpkgs/commit/34e468dc4268cee86aa019ae9bc52768e60fb5f7.
It's somewhat a cheat as **s390** was already there. But it does not
get much worse for a brand new linux target.

Testing it on **x86_64** host is trivial by overriding **crossSystem**
parameter:

.. code-block::

        $ nix-build --arg crossSystem '{ config = "s390x-unknown-linux-gnu"; }' -A re2c
        $ file ./result/bin/re2c
        $ ./result/bin/re2c: ELF 64-bit MSB executable, IBM S/390, version 1 (SYSV),
        dynamically linked, interpreter ...-gnu-2.33-50/lib/ld64.so.1, for GNU/Linux 2.6.32, not stripped
        $ qemu-s390x ./result/bin/re2c --version
        re2c 2.2

The joy of hacking
==================

The above **re2c** example did not require privileged user operations on
any step. It it true for almost any build operation.

Unprivileged "installs" make **nix** and **NixOS** a great interactive
environment for hacking on system itself and on upstream packages without
fear of damage to the main system.

For example **NixOS** still uses **gcc-10** by default. How hard would
it be to switch over to **gcc-11** and attempt to build my whole system
against it? It takes 3 commands to try (all unprivileed as well):

.. code-block::

    $ git clone https://github.com/NixOS/nixpkgs.git
    $ cd nixpkgs
    $ $EDITOR pkgs/top-level/all-packages.nix
    $ nix build -f nixos system

Here is the full one-liner change I applied to **pkgs/top-level/all-packages.nix**:

.. code-block:: diff

    --- a/pkgs/top-level/all-packages.nix
    +++ b/pkgs/top-level/all-packages.nix
    @@ -11225,7 +11225,7 @@ with pkgs;
             if (with stdenv.targetPlatform; isVc4 || libc == "relibc") then 6
             else if (stdenv.targetPlatform.isAarch64 && stdenv.isDarwin) then 11
             else if stdenv.targetPlatform.isAarch64 then 9
    -        else 10;
    +        else 11;
           numS = toString num;
         in {
           gcc = pkgs.${"gcc${numS}"};

There are more elaborate and maintainable ways to achieve the similar effect.
But that's the gist of configurability.

Rebuilding system instantly shown up quite a few yet unfixed packages. Typical
two liner fix looks like that:
  https://github.com/NixOS/nixpkgs/commit/646e7aa079fbe894e49efb6aa3a4fe3585bf8163

Binary substitutions
====================

As a general rule any minor change in package definition triggers rebuild of the
package and all it's reverse dependencies. This usually menas you need to rebuild
A Lot if you change frequently used package and plan to rebuild it's reverse
dependencies.

To avoid local rebuilds **NixOS** runs a CI system called Hydra. Hydra
continuously attempts to build every package definition on
**x86_64-linux** and a few other targets:
  https://hydra.nixos.org/jobset/nixpkgs/trunk

Build failures are a great source for low hanging fruit to fix for newcomers.
Most failures have one last successful and first failed commit against **nixpkgs**
repository. This makes bisection trivial and fun to get the idea what change
caused breakage:
  http://trofi.github.io/posts/228-bisects-all-the-way-down.html

As a general rule most packages run some test suite after the build (and
even install!). Those also tend to flag regressions or even old bugs. Recent
example is a https://github.com/Changaco/python-libarchive-c/pull/116 where
python object was garbage collected before it was accessed from C code where
it was registered before.

nix repl
========

**nixpks** is a huge package library. To navigate through it there are a few
tools like **nix search** or even **git grep**.

I personally use **nix repl** to poke at package definitions as is and fetch,
build or edit anything related to them. TAB completion is just great. Here is
my typical session:

.. code-block::

    $ nix repl '<nixpkgs>' # or "nix repl ."
    nix-repl> python3Packages.libarchive-c.src.urls
    [ "https://github.com/Changaco/python-libarchive-c/archive/3.1.tar.gz" ]
    
    nix-repl> :p python3Packages.libarchive-c.meta
    { ... description = "Python interface to libarchive"; homepage = "https://github.com/Changaco/python-libarchive-c"; license = { ... shortName = "cc0"; ...
    
    nix-repl> python3Packages.libarchive-c.meta.homepage
    "https://github.com/Changaco/python-libarchive-c"
    
    nix-repl> :b python3Packages.libarchive-c
    this derivation produced the following outputs:
      out -> /nix/store/w0sibclvsx4jjp85nnrxy66jzm1yfxgk-python3.9-libarchive-c-3.1

We looked at package metadata and built it. The output ended up in "/nix/store/...".
**:e** command would allow editing it.

Another example is poking at build toolchain details for a given package:

.. code-block::

    nix-repl> re2c.stdenv.cc
    «derivation /nix/store/fs3448rnjfypqz20wxxjv766zfjz53a0-gcc-wrapper-10.3.0.drv»
    # looks like gcc-10!
    
    nix-repl> (re2c.override { stdenv = gcc11Stdenv; }).stdenv.cc
    «derivation /nix/store/ni2cpxgyyhh9pmzysgjb53afxv5q3kjq-gcc-wrapper-11.1.0.drv»
    # now it's gcc-11!
    
    nix-repl> :b re2c
      out -> /nix/store/fmf0hd26h8cssbvy848aswqdrspnnbr3-re2c-2.2
    nix-repl> :b re2c.override { stdenv = gcc11Stdenv; }
      out -> /nix/store/sdcf0q26x2xa8x49010prk985zay542n-re2c-2.2
    
    nix-repl> re2c.<TAB>
    re2c.__ignoreNulls                re2c.nativeBuildInputs
    re2c.all                          re2c.out
    re2c.args                         re2c.outPath
    re2c.buildInputs                  re2c.outputName
    re2c.builder                      re2c.outputUnspecified
    re2c.configureFlags               re2c.outputs
    re2c.depsBuildBuild               re2c.override
    re2c.depsBuildBuildPropagated     re2c.overrideAttrs
    re2c.depsBuildTarget              re2c.overrideDerivation
    re2c.depsBuildTargetPropagated    re2c.passthru
    re2c.depsHostHost                 re2c.patches
    re2c.depsHostHostPropagated       re2c.pname
    re2c.depsTargetTarget             re2c.preCheck
    re2c.depsTargetTargetPropagated   re2c.propagatedBuildInputs
    re2c.doCheck                      re2c.propagatedNativeBuildInputs
    re2c.doInstallCheck               re2c.src
    re2c.drvAttrs                     re2c.stdenv
    re2c.drvPath                      re2c.strictDeps
    re2c.enableParallelBuilding       re2c.system
    re2c.enableParallelChecking       re2c.type
    re2c.inputDerivation              re2c.userHook
    re2c.meta                         re2c.version
    re2c.name

Now we have a package built with two toolchain versions and can do various
side-by-side comparisons. I usually use something similar when track down
regressions. Recent example is broken **firefox** when built with **gcc-12**.

Or you could have a look at a difference between two builds:

.. code-block:: diff

    $ diffoscope /nix/store/fmf0hd26h8cssbvy848aswqdrspnnbr3-re2c-2.2 /nix/store/sdcf0q26x2xa8x49010prk985zay542n-re2c-2.2
    readelf --wide --sections {}
    @@ -1,39 +1,39 @@
    ...
      -Symbol table '.dynsym' contains 98 entries:
      +Symbol table '.dynsym' contains 99 entries:
      -    37: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND strftime@GLIBC_2.2.5 (4)
      +    37: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND _ZSt28__throw_bad_array_new_lengthv@GLIBCXX_3.4.29 (8)
      +    38: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND strftime@GLIBC_2.2.5 (4)
      ...

Or you could build and run **php** or **cmake** against this version of **re2c**.

General impression
==================

I think **NixOS** is very much usable as a desktop system. I'll try it
for a little while longer to see how it goes.

I would say **NixOS** requires basic understanding of **nix** expression
language to effectively debug and explore less documented parts of the
system. **nix** as a tool has it's warts on UI side. But they are not serious.

Otherwise it's a nice system that provides large set of packages software
and allows for very easy plugging of local overrides of existing packages
or adding own packages not present in main repository.

There are various user-maintained overlays and repositories I did not yet
have a chance to look at. Focus on reproducible builds makes it trivial to
verify locally that fetched build matches locally built one bit for bit
(and when it does not **diffoscope** can point at exact diff).

Large binary cache makes is trivial trying out various packages with huge
dependency trees even as one-off run.

Autogenerated **/etc/** is very lean and never contains leftover configs
from a service you have uninstalled 5 years ago. It's a nice feeling.

Precise dependencies and immutable store allow for high parallelism of
package installs (or rebuilds). Final build result is more likely be the
same on various systems.

Immutable style of the store makes package "deletion" instant and garbage
collection very fast. Certainly way faster than typical package uninstall
times in Debian or Gentoo.

Functional-style dependency declaration effectively does not require any
dependency conflict or upgrade resolution complexity. You just install a
version of a package without touching existing one. Activation of the newly built
system is a single symlink switch:

.. code-block::

    $ ls -ld /run/current-system
    lrwxrwxrwx 1 root root 81 окт  9 21:22 /run/current-system -> /nix/store/js6s88x1gfsnf1ggh690chfmbibdpbvk-nixos-system-nz-21.11.git.4793d22a4c7

Same for the whole system rollback.

It is trivial to mix multiple versions of the package or flavours of the
package with different dimensions in the same system: optimization flags,
target system settings (cross-compilation), libc swapping, older version
of **nixpkgs** repository and many more.

Have fun!
