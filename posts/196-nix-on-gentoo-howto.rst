---
title: nix on gentoo howto
date: January 19, 2017
---

:PostID: 196
:Title: nix on gentoo howto
:Keywords: nix, gentoo, howto
:Categories: notes

TL;DR
=====

Getting nix on gentoo (`same steps <https://gitweb.gentoo.org/repo/gentoo.git/tree/sys-apps/nix/nix-1.11.6-r1.ebuild#n50>`_):

- install nix from gentoo main tree: **emerge nix**
- enable nix-daemon service:
  * on systemd: **systemctl enable nix-daemon**
  * on openrc: **rc-update add nix-daemon**

- relogin your user (it will source environment and profile update)
- run **nix-channel \-\-update**

Ready!

More fun commands
=================

- install a package: **nix-env -iA nixpkgs.firefox**
- uninstall a package: **nix-env -e nixpkgs.firefox**
- start a new shell with only specified package(s) in environment: **nix-shell \-\-pure -p chromium**

More words
==========

**haskell** overlay for **gentoo** had a **sys-apps/nix** package manager
for a long time. I bumped it's version a few times but never actually tried to
use it.

WARNING: I've been using nix only for a week and wrote like 3 **.nix** packages
for a few binaries and libraries. Take all I write here with a grain of salt.

Nix is not gentoo-specific at all. Nix is not even haskell-specific.
The implementation language is **C++**, **Perl** and `**Nix Expression Language** <https://nixos.org/releases/nix/latest/manual.pdf>`_.

Nix tries to solve (among other things) an interesting problem of software
distribution: make packages (binaries + everything else) runnable on machines
other than the one package was built on.

Nix tracks precise dependencies of packages it was built against down to libc.
Once you've changed glibc you need to rebuild all dependent packages.

Every build gets installed into it's own unique prefix under **/nix/store**:

.. code-block:: bash

    $ LANG=C ls -l /nix/store/ | egrep '^d.*(glibc|firefox)'
    dr-xr-xr-x 1 root root      36 Jan  1  1970 49hz360b2i923wg8dcccwlbqc8yv4yii-glibc-2.24-dev
    dr-xr-xr-x 1 root root      14 Jan  1  1970 iyvj5p6xq12f5b24cjiqi4pphsvna0cc-glibc-iconv-2.24
    dr-xr-xr-x 1 root root      14 Jan  1  1970 jkas4im9rr89n86h8p4sh3p6j3zdapcr-glibc-2.24-bin
    dr-xr-xr-x 1 root root      46 Jan  1  1970 kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24
    dr-xr-xr-x 1 root root      22 Jan  1  1970 w74ads27sakc524hfffi8iz4g8qdcill-firefox-unwrapped-50.1.0
    dr-xr-xr-x 1 root root      44 Jan  1  1970 xsqz79bzclgrgnnlffp3qa0g4wnbcrp1-firefox-50.1.0

And then gets symlinked into user's environment:

.. code-block:: bash

    $ nix-env -iA nixpkgs.firefox
    replacing old ‘firefox-50.1.0’
    installing ‘firefox-50.1.0’
    building path(s) ‘/nix/store/7ckh9ldic5fdlcqjjjpxadbya9lcj2gg-user-environment’
    created 85 symlinks in user environment

    $ type -P firefox
    /home/testie/.nix-profile/bin/firefox

    $ ls -l /home/testie/.nix-profile/bin/firefox
    lrwxrwxrwx 1 root root 70 Jan  1  1970 /home/testie/.nix-profile/bin/firefox -> /nix/store/xsqz79bzclgrgnnlffp3qa0g4wnbcrp1-firefox-50.1.0/bin/firefox

    $ ldd /nix/store/w74ads27sakc524hfffi8iz4g8qdcill-firefox-unwrapped-50.1.0/lib/firefox-50.1.0/firefox
    linux-vdso.so.1 (0x00007fffd9966000)
    libpthread.so.0 => /nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib/libpthread.so.0 (0x00007f5a3f720000)
    libdl.so.2 => /nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib/libdl.so.2 (0x00007f5a3f51c000)
    libstdc++.so.6 => /nix/store/jar52969wyf10sh2wj62ipfjiw7xaq2j-gcc-5.4.0-lib/lib/libstdc++.so.6 (0x00007f5a3f1a4000)
    libm.so.6 => /nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib/libm.so.6 (0x00007f5a3ee9f000)
    libgcc_s.so.1 => /nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib/libgcc_s.so.1 (0x00007f5a3ec89000)
    libc.so.6 => /nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib/libc.so.6 (0x00007f5a3e8eb000)
    /nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib/ld-linux-x86-64.so.2 (0x00007f5a3f93d000)

    $ LANG=C readelf -a /nix/store/w74ads27sakc524hfffi8iz4g8qdcill-firefox-unwrapped-50.1.0/lib/firefox-50.1.0/firefox | grep RUNPATH
    0x000000000000001d (RUNPATH)            Library runpath: [/nix/store/kk71vkqipf30qc165718jmp0s8cggn2y-glibc-2.24/lib:/nix/store/jar52969wyf10sh2wj62ipfjiw7xaq2j-gcc-5.4.0-lib/lib]

Unique interpreter and **RUNPATH** injection trick allows binaries just work without changes in environment.

That allows having multiple firefox versions (potentially built agains different glibc versions)
coexist at the same time and not interfere with others.

From perspective of a gentoo user nix feels like a very fast binary distribution.
Nix does not need to perform heavyweight file system operations when packages
are installed or uninstalled. It's just a matter of unpacking a couple of archives
into new locations (or reusing existing prebuilt packages).

As a gentoo dev I feel **.nix** files are almost like **.ebuild**s. But **.nix** files are
quite unfriendly to a user who does not deeply understand **Nix Expression Language**.

Overriding **CFLAGS** / **LDFLAGS** system-wide is a challenge. It's not a
fundamental problem of nix. Just a matter of exposing a few hooks to users
(at a price of losing binary cache).

Writing new packages
====================

There is quite a few packages still missing in nixpkgs repository. Let's see how easy
it is to add a new package locally. I'm used to **lv** tool (it's a **more**, **less**
and **most** sibling).

To get it accessible I had to write the following files:

.. code-block::

    # cat ~/.nixpkgs/config.nix 
    {
      packageOverrides = pkgs:
        let
          callPackage = pkgs.lib.callPackageWith (pkgs // self);
          self = rec {
            lv = callPackage ./lv/default.nix {};
          };
      in self;
    }

.. code-block::

    # cat ~/.nixpkgs/lv/default.nix 
    {stdenv, fetchpatch, fetchurl, ncurses}:
    
    stdenv.mkDerivation {
      name = "lv-4.51";
    
      src = fetchurl {
        # used to be http://www.ff.iij4u.or.jp/~nrt/freeware/lv451.tar.gz
        # Picking new mirror:
        url = http://distfiles.gentoo.org/distfiles/lv451.tar.gz;
        sha256 = "1kwb2mqavhghmixjyp8zjjc2ivx6r2cklb2z8dnvrgcz20kjxkg1";
      };
    
      # fix empty /bin/ recreation
      patches = [(fetchpatch {
        name = "lv-4.51-gentoo.patch";
        url = "https://gitweb.gentoo.org/repo/gentoo.git/plain/app-text/lv/files/lv-4.51-gentoo.patch";
        sha256 = "1s4d9gdmh2q8zr2bpi8ack648zpjz8i5wa0wf3bqd5lp90gaflx4";
      })];
    
      configureScript = "src/configure";
    
      buildInputs = [ ncurses ];
    
      meta = {
        description = "Powerful Multilingual File Viewer";
        homepage = http://www.ff.iij4u.or.jp/~nrt/lv/;
        license = stdenv.lib.licenses.gpl2;
        platforms = stdenv.lib.platforms.gnu; # random choice
      };
    }

The **lv/default.nix** looks straightforward: it's a "function" of one table-like argument
with 4 key-value pairs with names **stdenv**, **fetchpatch**, **fetchurl**, **ncurses**.

Local **callPackage** function picks fields with the same names from a table merged
of **pkgs** and **self** tables and passes them to **lv/default.nix**. Here
nixpkgs exploit lazy evaluation nature of **Nix Expression Language** to allow user to override,
say, **ncurses** or any other "function" argument.

Overriding CFLAGS
=================

How easy would it be to override **CFLAGS**? There is a hack to do it
at least on per-package level.

.. code-block::

    # cat ~/.nixpkgs/config.nix 
    {
      packageOverrides = pkgs:
        let
          callPackage = pkgs.lib.callPackageWith (pkgs // self);
          self = rec {
            lv = callPackage ./lv/default.nix {};
    
            # CFLAGS tweak example:
            sudo = pkgs.sudo.overrideDerivation (o: {CFLAGS = ["-O0"];});
          };
        in self;
    }

Links
=====

- Nix tools and Nix Expression Language manual: https://nixos.org/releases/nix/latest/manual.pdf
- Nixpkgs manual: https://nixos.org/nixpkgs/manual/
- NixOS manual: http://nixos.org/nixos/manual/

Have fun!
