---
title: "Bisects all the way down"
date: September 01, 2021
---

:PostID: 228
:Title: "Bisects all the way down"
:Keywords: git, bisect, nix, imake
:Categories: notes

Bisect is a great tool to nail down regression in a project like
**linux** where you usually have no slightest idea what broke
your
`suspend <https://fa.linux.kernel.narkive.com/hk0pvpD8/bisected-regression-v3-6-rc1-resume-from-s2ram-does-not-restore-ata-piix-v3-5-worked>`_,
`boot <https://www.spinics.net/lists/kernel/msg3840785.html>`_,
`video <https://linux-kernel.vger.kernel.narkive.com/epI9yBFu/bisected-i915-linux-2-6-32-rc3-regression>`_,
`more video <https://lore.kernel.org/lkml/YKUjvoaKKggAmpIR@sf/>`_,
`audio <http://yhbt.net/lore/all/20091225162528.5dbbbea0@mosly/>`_,
`tcp <https://gitlab.freedesktop.org/pulseaudio/pulseaudio/-/issues/164>`_,
`tun <https://linux-kernel.vger.kernel.narkive.com/v3qKMlzd/oops-2-6-31-rc1-tun>`_,
`toaster <https://www.spinics.net/lists/kernel/msg3840823.html>`_
and whatnot.

Example of other complex projects are **firefox**, **gcc**, **glibc**
and ... complete linux distributions!

Rolling release linux distributions tend to have frequent incremental
updates where each update produces mostly working system. And when
update breaks roll back is cheap and feedback loop to upstream is fast.

The key for rolling release system to work is to be able to narrow down
quickly on faulty component to be able to isolate it.

A typical example of distribution regression would be Firefox failure to
render fonts correctly. What package update caused the regression?
Sometimes we might guess easily if font-related package updated recently
and we might try rolling it back to verify.

But sometimes it's a compiler or even build environment bug (like bash
`miscompilation <https://gcc.gnu.org/PR88936>`_ caused by a gcc bug).
In that case it will take a while until we get to the culprit. Or it
might be a **glibc** regression which is not trivial to rollback at all.

Wouldn't it be nice to mechanically bisect package repository the same
way we do projects like linux?

Today's real world example is `nixpkgs <https://github.com/NixOS/nixpkgs>`_
repository. **nixpkgs** is a package tree available via **nix** tool.

One of **nix**'s fancy features is the ability to install packages (and
it's dependencies) as an unprivileged user. Or even fetch the package
into local cache for one-off use without installing it. Another feature
is precise hermetic dependencies tracking.

Example one-off package usage session:

.. code-block::

    # existing systemwide package (installed outside nixpkgs)
    $ re2c -v
    re2c 2.2
    
    # fetching version from nixpkgs:
    $ nix-shell -p re2c
    [nix-shell:~]$ re2c -v
    re2c 2.1.1

**nixpkgs** repository has a few branches of different stabilities.
Most frequently encountered are:

- **staging**: things are truly bleeding edge there, no binary cache,
  very fresh versions
- **staging-next**: things are being stabilized here to build and pass
  tests before merge to **master**
- **master**: things mostly build there and have binary cache of most
  packages

More branching details are at https://github.com/NixOS/rfcs/blob/master/rfcs/0026-staging-workflow.md.

I was foolish enough to try building **ccache** out of **staging**
branch (I planned to update it's version there):

.. code-block::

    # fetch repository:
    $ cd /tmp
    $ git clone https://github.com/NixOS/nixpkgs.git
    $ cd nixpkgs
    $ git checkout staging
    
    # build ccache:
    $ nix-build -A ccache
    ... <a few minutes later>
    make[1]: *** [Makefile:998: fig2dev/Makefile] Error 1
    make[1]: Leaving directory '/build/transfig.3.2.4'
    make: [Makefile:1006: Makefiles] Error 2 (ignored)
    make includes
    including in ./fig2dev...
    make[1]: Entering directory '/build/transfig.3.2.4/fig2dev'
    make[1]: *** No rule to make target 'includes'.  Stop.
    make[1]: Leaving directory '/build/transfig.3.2.4/fig2dev'
    make: *** [Makefile:1064: includes] Error 2
    error: builder for '/nix/store/149n49648mzf1c9g199jhq9qi6x35c9v-transfig-3.2.4.drv' failed with exit code 2;

Here we fetched git repository of the whole **nixpkgs** repo and tried
to build **ccache** along with all it's dependencies. One of them
(**transfig**) failed to build.

**transfig** happens to use **imake** build system. I knew nothing
about it and had no idea how to debug it. I looked at the generated
**Makefile**s and still had no idea why (or if) things are wrong there.

Having failed at understanding the failure mode I checked if **master**
branch was able to build **transfig** (it's normally expected to):

.. code-block::

    $ git checkout master
    
    $ nix-build -A transfig
    /nix/store/pfzhccslyzgl0wl127yahrk902gj54xs-transfig-3.2.4
    
    $ nix-build -A transfig --check
    ... <build log>
    /nix/store/pfzhccslyzgl0wl127yahrk902gj54xs-transfig-3.2.4

Built fine. **\-\-check** forces local rebuild instead of using binary
available in cache. I used it to get a build log from successful package
and to make sure I don't have something else horribly broken in my
build environment.

Now I could bisect against **master** and **staging** states:

.. code-block::

    $ git bisect start staging master
    $ git bisect run nix-build -A transfig
    ... < a few minutes later>
    commit 8675ca0e947f7e847d82828e6bfd4d08822c489c
    Date:   Wed Aug 4 08:29:53 2021 +0000
    
      xorg.xorgcffiles: 1.0.6 -> 1.0.7
    
      https://lists.x.org/archives/xorg-announce/2021-August/003105.html

Just two shell commands and wer are there! The commit looks vaguely
related to **imake**. Reverting:

.. code-block::

    $ git bisect reset
    $ git checkout staging
    $ git revert 8675ca0e947f7e847d82828e6bfd4d08822c489c # minor conflict fix
    $ nix-build -A transfig
    ...
    /nix/store/7z7q1a9176cy0adcs98l4dc8rh9ks4ki-transfig-3.2.4

Revert worked. I looked at the difference between **1.0.6** and
**1.0.7** sources and found nothing obviously broken. I still had no
idea what I was looking at.

We can bisect **xorg-cf-files** project as well. For that we can repoint our
**xorg.xorgcffiles** package to local checkout we could modify:

.. code-block:: diff

    --- a/pkgs/servers/x11/xorg/overrides.nix
    +++ b/pkgs/servers/x11/xorg/overrides.nix
    @@ -841,6 +841,7 @@ self: super:
       });
    
       xorgcffiles = super.xorgcffiles.overrideAttrs (attrs: {
    +    src = /tmp/cf; # added line
         postInstall = lib.optionalString stdenv.isDarwin ''
           substituteInPlace $out/lib/X11/config/darwin.cf --replace "/usr/bin/" ""
         '';

Let's prepare source tree in **/tmp/cf** as if it was just from tarball:

.. code-block::

    $ cd /tmp
    $ git clone https://gitlab.freedesktop.org/xorg/util/cf.git
    $ cd cf
    $ ./autogen.sh

Now we can build **transfig** against local checkout:

.. code-block::

    $ nix-build /tmp/nixpkgs -A transfig
    ... a few seconds later
    make: *** No rule to make target 'install'.  Stop.

Same problem.

**nix** will rebuild **xorg-cf-files** from local checkout and then will
rebuild all dependencies that need to change automatically. No need to
manually calculate the effect of the update. Sometimes it means a lot of
rebuilds (say, if you bisect **gcc**). But in our case **xorg-cf-files**
dependencies are just **imake** and **transfig**:

.. code-block::

    $ nix why-depends -f . --derivation transfig xorg.xorgcffiles
    /nix/store/...-transfig-3.2.4.drv
        → /nix/store/...-imake-1.0.8.drv
            → /nix/store/...-xorg-cf-files-1.0.7.drv

Both are tiny packages. Bisecting:

.. code-block::

    $ git bisect start xorg-cf-files-1.0.7 xorg-cf-files-1.0.6
    $ git bisect run nix-build /tmp/nixpkgs -A transfig
    ... a second later
    commit d47131ed97ee491bb883c29ec0b106e8d5acfcd3
    Date:   Thu Jul 5 10:42:09 2018 -0400
    
        linux: Update LinuxDistribution == LinuxRedHat section

That was simpler than I thought! But still very confusing :) The
`upstream commit <https://gitlab.freedesktop.org/xorg/util/cf/-/commit/d47131ed97ee491bb883c29ec0b106e8d5acfcd3>`_
is literally a few defines under seemingly unrelated **#if**:

.. code-block:: diff

    --- a/linux.cf
    +++ b/linux.cf
    @@ -190,7 +190,13 @@ InstallNamedTargetNoClobber(install,file.ad,$(INSTAPPFLAGS),$(XAPPLOADDIR),class
     #endif /* LinuxDebian */
     
     #if LinuxDistribution == LinuxRedHat
    -#define FSUseSyslog		YES
    +# define FSUseSyslog		YES
    +# define BuildRman		NO
    +# define BuildHtmlManPages	NO
    +# define ProjectRoot		/usr
    +# define ManPath		/usr/share/man
    +# define XAppLoadDir		/usr/share/X11/app-defaults
    +# define ConfigDir		/usr/share/X11/config
     #endif
     
     #ifndef HasDevRandom

**nix** does not use **/usr** host OS hierarchy (in my case host OS is
Gentoo) and always uses **/nix/store** path instead. Thus I would expect
**LinuxDistribution** to be something different from **LinuxRedHat**
(unless it's a way for **cf** to say "any linux").

Let's check how **LinuxDistribution** gets defined. It's hidden in
**imake** itself. We can extract unpatched and patched **imake**
right from **nixpkgs**:

.. code-block::

    $ cd /tmp/nixpkgs
    $ nix-shell -A xorg.imake
    
    # unpack vanilla source:
    $$ unpackPhase
      unpacking source archive /nix/store/dfjcsfxf15zxrbcw62ml1zbczm8zf7d0-imake-1.0.8.tar.bz2
      source root is imake-1.0.8
      setting SOURCE_DATE_EPOCH to timestamp 1552778797 of file imake-1.0.8/INSTALL

    # apply nixkpgs-specific patches:
    $$ cd imake-1.0.8
    $$ patchPhase
      applying patch /nix/store/9hl5c2sg2n6yfia0hy06wdf7yiry4arq-imake.patch
      patching file imake.c
      applying patch /nix/store/kmhjr434iv05bgazd5xbzwygn59pl9k0-imake-cc-wrapper-uberhack.patch
      patching file imake.c

Here is the unpatched bit of **LinuxRedHat** definition from https://gitlab.freedesktop.org/xorg/util/imake/-/blob/master/imake.c#L1046:

.. code-block:: c

    #if defined CROSSCOMPILE || defined linux || defined(__GLIBC__)
    static void
    get_distrib(FILE *inFile)
    {
      struct stat sb;
    
      static const char*   suse = "/etc/SuSE-release";
      static const char* redhat = "/etc/redhat-release";
      static const char* debian = "/etc/debian_version";
    
      fprintf (inFile, "%s\n", "#define LinuxUnknown    0");
      fprintf (inFile, "%s\n", "#define LinuxSuSE       1");
      fprintf (inFile, "%s\n", "#define LinuxCaldera    2");
      fprintf (inFile, "%s\n", "#define LinuxCraftworks 3");
      fprintf (inFile, "%s\n", "#define LinuxDebian     4");
      fprintf (inFile, "%s\n", "#define LinuxInfoMagic  5");
      fprintf (inFile, "%s\n", "#define LinuxKheops     6");
      fprintf (inFile, "%s\n", "#define LinuxPro        7");
      fprintf (inFile, "%s\n", "#define LinuxRedHat     8");
      fprintf (inFile, "%s\n", "#define LinuxSlackware  9");
      fprintf (inFile, "%s\n", "#define LinuxTurbo      10");
      fprintf (inFile, "%s\n", "#define LinuxWare       11");
      fprintf (inFile, "%s\n", "#define LinuxYggdrasil  12");
    
    # ifdef CROSSCOMPILE
      if (CrossCompiling) {
          fprintf (inFile, "%s\n",
               "#define DefaultLinuxDistribution LinuxUnknown");
          fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Unknown");
          return;
      }
    # endif
      if (lstat (suse, &sb) == 0) {
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxSuSE");
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistName SuSE");
        return;
      }
      if (lstat (redhat, &sb) == 0) {
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxRedHat");
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistName RedHat");
        return;
      }
      if (lstat (debian, &sb) == 0) {
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxDebian");
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Debian");
        /* You could also try to get the version of the Debian distrib by looking
         * at the content of /etc/debian_version */
        return;
      }
      /* what's the definitive way to tell what any particular distribution is? */
    
      fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxUnknown");
      fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Unknown");
      /* would like to know what version of the distribution it is */
    }

Distribution flavour is defined by presence of **/etc/redhat-release**
file on disk. But I dont have it! I should have gotten **LinuxUnknown**.

The culprit is in that suspicious **/nix/store/9hl5c2sg2n6yfia0hy06wdf7yiry4arq-imake.patch**
patch we see in **patchPhase** log. It turns the code above to the
following:

.. code-block::

    #if defined CROSSCOMPILE || defined linux || defined(__GLIBC__)
    static void
    get_distrib(FILE *inFile)
    {
    #if 0
      struct stat sb;
    
      static const char*   suse = "/etc/SuSE-release";
      static const char* redhat = "/etc/redhat-release";
      static const char* debian = "/etc/debian_version";
    
      fprintf (inFile, "%s\n", "#define LinuxUnknown    0");
      fprintf (inFile, "%s\n", "#define LinuxSuSE       1");
      fprintf (inFile, "%s\n", "#define LinuxCaldera    2");
      fprintf (inFile, "%s\n", "#define LinuxCraftworks 3");
      fprintf (inFile, "%s\n", "#define LinuxDebian     4");
      fprintf (inFile, "%s\n", "#define LinuxInfoMagic  5");
      fprintf (inFile, "%s\n", "#define LinuxKheops     6");
      fprintf (inFile, "%s\n", "#define LinuxPro        7");
      fprintf (inFile, "%s\n", "#define LinuxRedHat     8");
      fprintf (inFile, "%s\n", "#define LinuxSlackware  9");
      fprintf (inFile, "%s\n", "#define LinuxTurbo      10");
      fprintf (inFile, "%s\n", "#define LinuxWare       11");
      fprintf (inFile, "%s\n", "#define LinuxYggdrasil  12");
    
    # ifdef CROSSCOMPILE
      if (CrossCompiling) {
          fprintf (inFile, "%s\n",
                   "#define DefaultLinuxDistribution LinuxUnknown");
          fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Unknown");
          return;
      }
    # endif
      if (lstat (suse, &sb) == 0) {
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxSuSE");
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistName SuSE");
        return;
      }
      if (lstat (redhat, &sb) == 0) {
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxRedHat");
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistName RedHat");
        return;
      }
      if (lstat (debian, &sb) == 0) {
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxDebian");
        fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Debian");
        /* You could also try to get the version of the Debian distrib by looking
         * at the content of /etc/debian_version */
        return;
      }
    #endif
      /* what's the definitive way to tell what any particular distribution is? */
    
      fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxUnknown");
      fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Unknown");
      /* would like to know what version of the distribution it is */
    }

Note now **#if 0** removes not just **#define DefaultLinuxDistName LinuxRedHat**
but also **#define LinuxUnknown    0** and **#define LinuxRedHat     8**.

Or in diff form imake's output change is:

.. code-block:: diff

    @@ -1,3 +1 @@
    -#define LinuxUnknown    0
    -#define LinuxRedHat     8
     #define DefaultLinuxDistName Unknown

Is it a big deal? How does it change
**#if LinuxDistribution == LinuxRedHat** condition? Let's try two
following examples:

.. code-block::

    $ printf "#define a 1\n#define b 2\n#if a == b\n    EQUAL\n#else\n    DIFFERENT\n#endif\n"
    #define a 1
    #define b 2
    #if a == b
        EQUAL
    #else
        DIFFERENT
    
    $ printf "#if a == b\n    EQUAL\n#else\n    DIFFERENT\n#endif\n"
    #if a == b
        EQUAL
    #else
        DIFFERENT

Running the preprocessor:

.. code-block::

    $ printf "#define a 1\n#define b 2\n#if a == b\n    EQUAL\n#else\n    DIFFERENT\n#endif\n" | gcc -E -
        DIFFERENT
    
    $ printf "#if a == b\n    EQUAL\n#else\n    DIFFERENT\n#endif\n" | gcc -E -
        EQUAL

According to great **imake** intro at http://www.snake.net/software/imake-stuff/config-X11R4.pdf
it's one of the common **imake** pitfalls: in integer evaluation
contexts unknown symbols get turned onto zeros.

.. code-block::

    $ printf "#if undef == 0\n    ZERO\n#endif\n"
    #if undef == 0
        ZERO
    #endif
    $ printf "#if undef == 0\n    ZERO\n#endif\n" | gcc -E -
        ZERO

Thus the fix is trivial: don't omit any enum definition
as other packages using **imake** actually rely on them being present.
Possible fix:

.. code-block:: diff

    --- a/imake.c
    +++ b/imake.c
    @@ -998,121 +998,121 @@ get_libc_version(FILE *inFile)
     #if defined CROSSCOMPILE || defined linux || defined(__GLIBC__)
     static void
     get_distrib(FILE *inFile)
     {
    -#if 0
       struct stat sb;
    
       static const char*   suse = "/etc/SuSE-release";
       static const char* redhat = "/etc/redhat-release";
       static const char* debian = "/etc/debian_version";
    
       fprintf (inFile, "%s\n", "#define LinuxUnknown    0");
       fprintf (inFile, "%s\n", "#define LinuxSuSE       1");
       fprintf (inFile, "%s\n", "#define LinuxCaldera    2");
       fprintf (inFile, "%s\n", "#define LinuxCraftworks 3");
       fprintf (inFile, "%s\n", "#define LinuxDebian     4");
       fprintf (inFile, "%s\n", "#define LinuxInfoMagic  5");
       fprintf (inFile, "%s\n", "#define LinuxKheops     6");
       fprintf (inFile, "%s\n", "#define LinuxPro        7");
       fprintf (inFile, "%s\n", "#define LinuxRedHat     8");
       fprintf (inFile, "%s\n", "#define LinuxSlackware  9");
       fprintf (inFile, "%s\n", "#define LinuxTurbo      10");
       fprintf (inFile, "%s\n", "#define LinuxWare       11");
       fprintf (inFile, "%s\n", "#define LinuxYggdrasil  12");
    
    +#if 0
     # ifdef CROSSCOMPILE
       if (CrossCompiling) {
           fprintf (inFile, "%s\n",
                   "#define DefaultLinuxDistribution LinuxUnknown");
           fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Unknown");
           return;
       }
     # endif
       if (lstat (suse, &sb) == 0) {
         fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxSuSE");
         fprintf (inFile, "%s\n", "#define DefaultLinuxDistName SuSE");
         return;
       }
       if (lstat (redhat, &sb) == 0) {
         fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxRedHat");
         fprintf (inFile, "%s\n", "#define DefaultLinuxDistName RedHat");
         return;
       }
       if (lstat (debian, &sb) == 0) {
         fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxDebian");
         fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Debian");
         /* You could also try to get the version of the Debian distrib by looking
          * at the content of /etc/debian_version */
         return;
       }
     #endif
       /* what's the definitive way to tell what any particular distribution is? */
    
       fprintf (inFile, "%s\n", "#define DefaultLinuxDistribution LinuxUnknown");
       fprintf (inFile, "%s\n", "#define DefaultLinuxDistName Unknown");
       /* would like to know what version of the distribution it is */
     }

We move **#if 0** below to always define **#define LinuxRedHat     8** and friends.

Original `imake.patch <https://github.com/NixOS/nixpkgs/commit/7dba8848ed4bcceb4187a754f221af26f10b2063>`_
was added in 2006. This makes it 15 years old bug.

The fix is pending at https://github.com/NixOS/nixpkgs/pull/135414
pull request. Fixing **imake** immediately broke **xcruiser**,
**xvkbd** and **xxkb** packages (reviewers++). It was failing for the
lack of path overrides that were now exposed on non-**LinuxRedHat**
systems. We will probably see more subtle breakages. I hope future
breakages will not be as magic as this one.

Now I can test my **ccache** update against **nixpkgs/staging**.

`Imake doc <http://www.snake.net/software/imake-stuff/config-X11R4.pdf>`_
shares a few amusing facts and subtle tips on how to workaround certain
C preprocessor behaviours to force it to generate valid makefiles. For
example if you want cpp to print '#' you need to prepend it with ... a
C comment!

.. code-block::

    $ printf '# Makefile comment\n'
    # Makefile comment
    
    $ printf '# Makefile comment\n' | gcc -traditional -E -
    <stdin>:1:3: error: invalid preprocessing directive #Makefile
    
    $ printf '/**/# Makefile comment\n' | gcc -traditional -E -
    # Makefile comment

Parting words
-------------

**nixpkgs** makes it trivial to bisect faulty package updates on
a package level as you would normally do it on project level.

I found a few new things along the way:

- **nixs**'s dependency "resolution" is instant. Constructing
  dependency graph is so much faster than trying to search a path in
  existing graph (like Gentoo's portage does).
- **nix-shell** is a nice way to poke at package unpacking,
  building and intallation steps.
- **imake** is both fun and scary way to (ab)use C preprocessor to
  generate **Makefile**s.

Have fun!
