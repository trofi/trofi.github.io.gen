---
title: "Trying out gcc-12"
date: November 14, 2021
---

:PostID: 231
:Title: "Trying out gcc-12"
:Keywords: gcc-12
:Categories: notes

Having updated locally new **NixOS** system to **gcc-11.2.0** I decided
I'm familiar enough with **NixOS** to try yet unreleased **gcc-12** on
a main system.

Would it just work? That would be boring :)

First I added new **gcc** snapshot to **nixpkgs** locally. It was only a
matter of pulling in **flex** as a dependency (**gcc** does not provide
genrated lexers for snapshots, only does fore releases and release candidates):

.. code-block:: diff

    --- pkgs/development/compilers/gcc/11/default.nix       2021-10-27 07:15:23.996869137 +0100
    +++ pkgs/development/compilers/gcc/12/default.nix       2021-11-14 07:34:59.850516073 +0000
    @@ -34,6 +34,9 @@
     , gnused ? null
     , cloog # unused; just for compat with gcc4, as we override the parameter on some places
     , buildPackages
    +
    +# snapshots don't have all the environment in front of them
    +, flex
     }:
    
     # LTO needs libelf and zlib.
    @@ -56,8 +59,9 @@
     with lib;
     with builtins;
    
    -let majorVersion = "11";
    -    version = "${majorVersion}.2.0";
    +let majorVersion = "12";
    +    version = "${majorVersion}.0.0";
    +    snapDate = "20211107";
    
         inherit (stdenv) buildPlatform hostPlatform targetPlatform;
    
    @@ -94,8 +102,8 @@
       builder = ../builder.sh;
    
       src = fetchurl {
    -    url = "mirror://gcc/releases/gcc-${version}/gcc-${version}.tar.xz";
    -    sha256 = "sha256-0I7cU2tUw3KhAQ/2YZ3SdMDxYDqkkhK6IPeqLNo2+os=";
    +    url = "mirror://gcc/snapshots/${majorVersion}-${snapDate}/gcc-${majorVersion}-${snapDate}.tar.xz";
    +    sha256 = "sha256-YrpYs8GHGjpxto8h7JZI3VJLZAn4aRekbCvx+e1WjII=";
       };
    
       inherit patches;
    @@ -161,7 +169,7 @@
         libcCross crossMingw;
    
       depsBuildBuild = [ buildPackages.stdenv.cc ];
    -  nativeBuildInputs = [ texinfo which gettext ]
    +  nativeBuildInputs = [ flex texinfo which gettext ]
         ++ (optional (perl != null) perl)
         ++ (optional langAda gnatboot)
    ;

And a few more lines to set **gcc-12** as a default.

I attempted to rebuild packages in my system with **nix build -f nixos system**.

First failure was a mysterious early **ed-1.17** test failure. I updated
**ed** to **1.18-pre3** locally and that fixed the test. I made a mental note
to get back to it a bit later to find out what fixed the test.

Then there was a long list of minor failures related to standard library
header interdependencies where fix is a one-liner to add required header.
A few examples are:

- oggvideotools: https://sourceforge.net/p/oggvideotools/bugs/13/
- libopenmpt: https://github.com/OpenMPT/openmpt/pull/8
- jfx

This is the most typical breakage for compiler updates.

Then there were a few failures related to now forbidden **std::string(NULL)**
constructor. The fix is to use simpler **std::string()** constructor.
A few affected projects are:

- binutils: https://sourceware.org/git/?p=binutils-gdb.git;a=commitdiff;h=068a039b8bd7c7386bb0d88f0ae245b0fe4132e9
- nlohmann/json (bug only): https://github.com/nlohmann/json/issues/3138
- ccache

Then I noticed another change in ADL resolution, where **operator<<** was not
searched in current namespace if none of arguments are in current namespace.
**clang** had this behaviour for a while. Now **gcc** follows the lead.
Only one project is affected so far:

- source-highlighting: https://git.savannah.gnu.org/cgit/src-highlite.git/commit/?id=ab9fe5cb9b85c5afab94f2a7f4b6d7d473c14ee9

After getting through this trivia I got an unusual build failure for **libX11**.
Here is a shorter example:

.. code-block:: c

    #include <string.h>
    int extract(char*);
    int XReadBitmapFileData (void) {
        char name_and_type[255];
        for (;;) {
            extract (name_and_type);
            char * type = strrchr (name_and_type, '_');
            if (type) type++; else type = name_and_type;
            if (strcmp ("hot", type) == 0) {
            if (type-- == name_and_type || type-- == name_and_type) continue;
            if (strcmp ("ax_hot", type) == 0) return 1;
        }
      }
      return -1;
    }

It took me a while to wrap my head around what is being done here. And
after that while I still got it wrong :) (but not by too much).

Here is what **gcc-12** has to say about the code:

.. code-block::

    # ok:
    $ gcc-11.2.0 -Wall -Werror=array-bounds -fno-strict-aliasing -O2 -c bug.c -o bug.o
    # bad:
    $ gcc-12.0.0 -Wall -Werror=array-bounds -fno-strict-aliasing -O2 -c bug.c -o bug.o
    bug.c: In function 'XReadBitmapFileData':
    bug.c:10:48: error: array subscript -2 is outside array bounds of 'char[9223372036854775807]' [-Werror=array-bounds]
       10 |             if (type-- == name_and_type || type-- == name_and_type) continue;
          |                                            ~~~~^~
    bug.c:4:14: note: at offset [0, 253] into object 'name_and_type' of size 255
        4 |         char name_and_type[255];
          |              ^~~~~~~~~~~~~
    cc1: some warnings being treated as errors

Note: it's normally just a warning. But **libX11** explicitly uses
"-Werror=array-bounds" as part of it's build system assuming this
warning can catch important problems.

The error makes no sense in tis case: a) there is no array dereference,
b) there should be no out-of-bounds pointers here.

I was also not sure if **(type-- == name_and_type || type-- == name_and_type)**
is a valid construct. I filed https://gcc.gnu.org/PR103215. There Andrew
explained that **||** is a sequence point and it has expected defined
behaviour. In hindsight it's obvious as **||** is also a lazy evaluation
device.

Apparently this build failure is somewhat common. Same failure on
identical code is present in:

- libXmu
- gdk-pixbuf

Having sorted the above I returned to **ed-1.17** failure.  I assumed it
was some ancient undefined behaviour in **ed** uncovered by new **gcc**
optimisations (like recent **modref** improvements). I looked at
the diff between **ed-1.17** and **ed-1.18-pre3** tarballs but found
nothing suspicious.

**ed** is an old and short codebase. Instead of staring a lot at generated
assembly I decided to shrink it down to something I could easily understand
and possibly fix. In the hindsight it was a good decision.

After I arrived to a 2KB self-contained example of C code I still could
not see what exactly **gcc-12** does to it to make it fail.

I filed https://gcc.gnu.org/PR103202 and Aldy instantly fixed very obscure
**PHI** importing order bug. From what I understand we used stale variable
names before a "rename" and used incorrect metadata attached to them
(like value ranges).

These kinds of bugs are very hard to extract from large projects. Even for
**ed** case it took me 3 evenings. On a positive side now I know **ed**'s
editor model :)

After getting **ed** fixed I encountered an (already fixed) ICE on
**gperftools**: https://gcc.gnu.org/PR103099.

Currently I can build almost all packages for my system with **gcc-12**.
I have a few local hacks too dirty to share, but they are mostly about
getting headers imported correctly and getting around too aggressive
warnings-as-errors in a few packages libe **libjxl**.

The only yet unsolved mystery is 4 newly failing tests on **llvm-13**
package. Somehow **llvm** generates FunctionLength=0 unwind information
for learly non-empty functions. I suspect (hope) it's another **gcc** bug.
I'm trying to debug it right now. **UPDATE:** confirmed and extracted
example to illustrate **gcc** bug https://gcc.gnu.org/PR103266

Fun facts
---------

- It took me 2.5 months to get to gcc-from-git after switching the distribution :)
- Not too much was broken for these past two months.
- Most package failures are legitimate code bugs: missing includes, invalid or
  tautological code.
- Some failures against **gcc-12** are still good old compiler bugs :)
- Package test on **nixpkgs** are great at catching real bugs.

I think **gcc-12** is quite usable for early experiments.

Have fun!
