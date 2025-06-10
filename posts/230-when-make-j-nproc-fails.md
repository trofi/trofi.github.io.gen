---
title: "When make -j$(nproc) fails"
date: October 28, 2021
---

The other day I noticed that `nixpkgs` repository has slightly
outdated `bison-3.7.6` package while `3.8.2` was available upstream.
I tried to update it. Usually it takes a 2 line change: update version
and update upstream tarball hash. Something like:
<https://github.com/NixOS/nixpkgs/commit/88bdfc3b6cdd489abf480880aa01d71b6965a53c>

``` diff
--- a/pkgs/development/tools/parsing/bison/default.nix
+++ b/pkgs/development/tools/parsing/bison/default.nix
@@ -7,11 +7,11 @@

 stdenv.mkDerivation rec {
   pname = "bison";
-  version = "3.7.6";
+  version = "3.8.2";

   src = fetchurl {
     url = "mirror://gnu/${pname}/${pname}-${version}.tar.gz";
-    sha256 = "sha256-adwLtG6o/DB9TKHgthyMNV6yB9Cwxp9PhGIyjnTXueo=";
+    sha256 = "sha256-BsnhO99+sk1M62tZIFpPZ8LH5yExGWREMP6C+9FKCrs=";
   };
```

I updated it locally and noticed that `bison` was somewhat slow to
build. The build process looked sequential:

``` 
$ time nix build --impure --expr 'with import ./.{}; bison' --rebuild
real    0m28,695s
```

I tried to enable parallelism and got `1.5x` speedup:

``` 
$ time nix build --impure --expr 'with import ./.{}; bison.overrideAttrs (oa: { enableParallelBuilding = true; })'
real    0m19,090s
```

And then applied the same to `bison/default.nix` build definition:
<https://github.com/NixOS/nixpkgs/commit/faf06a88de74364d4544e1ae8adbc5a31b51e4fe>

``` diff
--- a/pkgs/development/tools/parsing/bison/default.nix
+++ b/pkgs/development/tools/parsing/bison/default.nix
@@ -17,8 +17,10 @@ stdenv.mkDerivation rec {
   nativeBuildInputs = [ m4 perl ] ++ lib.optional stdenv.isSunOS help2man;
   propagatedBuildInputs = [ m4 ];

-  doCheck = false; # fails
-  doInstallCheck = false; # fails
+  enableParallelBuilding = true;
+
+  doCheck = true;
+  doInstallCheck = true;

   meta = {
     homepage = "https://www.gnu.org/software/bison/";
```

While at it I also enabled bison tests and slowed the build down to `1m22s`.
`bison` has `~700` tests. `3x` slowdown compared to initial state. But at
least tests now use all the CPUs I have :) Leaving it for another day.

`bison` happens to be a bootstrap package that causes rebuild of
almost all packages. None of `~2400` failed for my system. Yay! While at
it I noticed that a few other packages were running sequentially, like
`coreutils`:

``` 
$ time nix build --impure --expr 'with import ./. {}; coreutils' --rebuild
real    3m4,589s

$ time nix build --impure --expr 'with import ./. {}; coreutils.overrideAttrs (oa: { enableParallelBuilding = true; })'
real    0m52,312s
```

`3.5x` speedup just like that. At this point I wondered why parallelism is
not a default. I suspected I have a lot more packages that build
sequentially for no real reason.
Mechanically it's a tiny change in `stdenv.mkDerivation` function:

``` diff
--- a/pkgs/stdenv/generic/make-derivation.nix
+++ b/pkgs/stdenv/generic/make-derivation.nix
@@ -60,6 +60,8 @@ in
     (stdenv.hostPlatform != stdenv.buildPlatform)
     [ "build" "host" ]

+, enableParallelBuilding ? true
+
 # TODO(@Ericson2314): Make unconditional / resolve #33599
 # Check phase
 , doCheck ? config.doCheckByDefault or false
@@ -310,7 +312,8 @@ else let
           llvm-config = 'llvm-config-native'
         '';
       in [ "--cross-file=${crossFile}" ] ++ mesonFlags;
-    } // lib.optionalAttrs (attrs.enableParallelBuilding or false) {
+    } // lib.optionalAttrs enableParallelBuilding {
+      inherit enableParallelBuilding;
       enableParallelChecking = attrs.enableParallelChecking or true;
     } // lib.optionalAttrs (hardeningDisable != [] || hardeningEnable != [] || stdenv.hostPlatform.isMusl) {
       NIX_HARDENING_ENABLE = enabledHardeningOptions;
```

It is safe? Some packages will likely fail horribly when they get built
in parallel. Scary and so tempting!
With the change above I built all the packages my system had with `nix
build -f nixos system --keep-going` and immediately uncovered a bunch
of failures:

- `cramfsswap`
- `discount`
- `perlPackages.X11XCB`
- `libomxil-bellagio`
- `vde2`
- `adns`
- `portaudio`
- `dmraid`
- `directfb`
- `ocaml`
- `cdparanoiaIII`
- `gnome2.ORBit2`

12 packages out of `~2400` is not too much. Worst case I could mark them
with `enableParallelChecking = false;` and move on.
I decided to spend some time on each of failures to possibly fix them
upstream or at least get some insight into why build systems sometimes
fail at this.
I love build system failures. Most of the time they are trivial to debug
and fix. And when they are not it's always a mystery to find out what a
fancy failure mode awaits us there.

## `cramfsswap`

`cramfsswap` was easy to reproduce on rerun:

``` 
$ nix build --impure --expr 'with import ./.{}; cramfsswap.overrideAttrs (oa: { enableParallelBuilding = true; })' -L
...
cramfsswap> build flags: -j16 -l16 SHELL=/nix/store/wadmyilr414n7bimxysbny876i2vlm5r-bash-5.1-p8/bin/bash
cramfsswap> gcc -Wall -g -O    -o cramfsswap cramfsswap.c -lz
cramfsswap> strip cramfsswap
cramfsswap> strip: 'cramfsswap': No such file
cramfsswap> make: *** [Makefile:10: strip] Error 1
cramfsswap> make: *** Waiting for unfinished jobs....
```

Looks like `strip` is called a bit early. Let's check the
`Makefile` contents (`nix develop -f. cramfsswap --phase unpack`):

``` Makefile
all: cramfsswap strip

cramfsswap: cramfsswap.c
        $(CC) -Wall -g -O $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o cramfsswap cramfsswap.c -lz

strip:
        strip cramfsswap
```

Can you spot the failure?

`strip` target should depend on `cramfsswap` target. Nice thing
about such bugs is that these are easy to verify by running an
individual target with incomplete depends:

``` 
$ LANG=C make strip
strip cramfsswap
strip: 'cramfsswap': No such file
make: *** [Makefile:10: strip] Error 1
```

The fix is trivial:

``` diff
--- a/Makefile
+++ b/Makefile
@@ -6,7 +6,7 @@ debian: cramfsswap
 cramfsswap: cramfsswap.c
        $(CC) -Wall -g -O $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) -o cramfsswap cramfsswap.c -lz

-strip:
+strip: cramfsswap
        strip cramfsswap

 install: cramfsswap
```

Even better would be to get rid of `strip` entirely. But let's keep
the build system's behaviour unchanged for now.
Reported upstream as
<https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=996964>.
This is an example of class of bugs where direct dependency is missing
in `Makefile`. Very similar failures were found in:

- `discount`:
  <https://github.com/Orc/discount/commit/c00ad392bf96d723f4ae01edebb3328290d11a5c>
- `X11-XCB`:
  <https://github.com/stapelberg/X11-XCB/commit/813608dacdae1ae35c9eb0f171a958617e014520>
- `libomxil-bellagio` (no upstream fix yet)
- vde2:
  <https://github.com/virtualsquare/vde-2/commit/7dd9ed46d5dca125ca45d679ac9f3acbfb0f9300>

## `adns`

`adns` was more elusive and required a bit of persistence to reproduce
again:

``` 
$ nix build --impure --expr 'with import ./.{}; adns.overrideAttrs (oa: { enableParallelBuilding = true; })' -L
...
m4 -P -I. hredirect.h.m4 >hredirect.h-a.new
sed -e 's/hm_comma/,/g; s/hm_squote/'\''/g; /^[  ]*$/d' <hredirect.h-a.new >hredirect.h-b.new
m4 -P -I. hfuzzraw.c.m4 >hfuzzraw.c-a.new
sed -e 's/hm_comma/,/g; s/hm_squote/'\''/g; /^[  ]*$/d' <hfuzzraw.c-a.new >hfuzzraw.c-b.new
gcc -g -O2 -Wall -Wmissing-prototypes -Wwrite-strings -Wstrict-prototypes -Wcast-qual -Wpointer-arith -Wno-pointer-sign -Wno-unused-value  -I../src -I. -I./../src  -DADNS_REGRESS_TEST -I. -I ./../src -c -g -o adnsresfilter_c.o ../client/adnsresfilter.c
In file included from ../client/adnsresfilter.c:46:
./hredirect.h:3:10: fatal error: hsyscalls.h: No such file or directory
    3 | #include "hsyscalls.h"
      |          ^~~~~~~~~~~~~
compilation terminated.
make[1]: *** [Makefile:130: adnsresfilter_c.o] Error 1
```

In this case very occasionally `hsyscalls.h` file was not present. In
this case `hsyscalls.h` was autogenerated from `Makefile` itself (as
opposed to `./configure`-time generation) in `regress/Makefile.in`:
<http://www.chiark.greenend.org.uk/ucgi/~ianmdlvl/git?p=adns.git;a=blob;f=regress/Makefile.in;h=993c1af66e2fbda331aee965ac2bfc8d3665c43e;hb=HEAD>

``` Makefile
REDIRLIBOBJS=   $(addsuffix _d.o, $(basename $(LIBOBJS)))
HARNLOBJS=      hcommon.o $(REDIRLIBOBJS)
ALL_OBJS=       $(HARNLOBJS) dtest.o hrecord.o hplayback.o hnonfuzz.o hfuzz.o

%_c.o:          $(srcdir)/../client/%.c hredirect.h
            $(CC) $(CFLAGS) $(HCPPFLAGS) -I $(srcdir)/../src -c -g -o $@ $<

$(ALL_OBJS):    $(srcdir)/../src/adns.h $(srcdir)/../src/internal.h
$(ALL_OBJS):    harness.h hsyscalls.h

%::     %.m4 hmacros.i4 hsyscalls.i4
    $(M4) -P -I$(srcdir) $< >$@-a.new
    sed -e 's/hm_comma/,/g; s/hm_squote/'\''/g; /^[  ]*$$/d' <$@-a.new >$@-b.new
    @mv -f $@-b.new $(srcdir)/$@; rm -f $@-a.new
```

Here `hsyscalls.h.m4` is used to produce `hsyscalls.h`, but
`adnsresfilter_c.o` in no way specifies the dependency against it. It
should.
Minor note: `../client/adnsresfilter.c` includes `hsyscalls.h` only
indirectly via `hredirect.h` and thus requires some care to specify
actual dependency. Should `adnsresfilter.c` require `hsyscalls.h`
dependency? Or should `hredirect.h` require `hsyscalls.h` as a
dependency? Up to build system author.
Upstream report: <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=51329>.

This is a class of bugs where source dependency is autogenerated, but
`Makefile` does not specify object dependency on generated source.

Very similar failures were found in:

- `portaudio`
- `dmraid`
- `directfb`

## `ocaml`

`ocaml` is a big project that takes a while to compile. Normally
such projects have `Makefile` ready for parallel build as is. And yet:

``` 
$ nix build --impure --expr 'with import ./.{}; ocaml.overrideAttrs (oa: { enableParallelBuilding = true; })' -L
...
ocaml> build flags: -j16 -l16 SHELL=/nix/store/wadmyilr414n7bimxysbny876i2vlm5r-bash-5.1-p8/bin/bash world bootstrap world.opt
ocaml> make -C runtime  all
ocaml> make promote-cross
ocaml> make coldstart
ocaml> make[1]: Entering directory '/build/ocaml-4.12.0/runtime'
ocaml> make[1]: Entering directory '/build/ocaml-4.12.0'
ocaml> make[1]: Entering directory '/build/ocaml-4.12.0'
ocaml> make -C runtime  all
ocaml> cp ocamlc boot/ocamlc
ocaml> make[2]: Entering directory '/build/ocaml-4.12.0/runtime'
ocaml> cp: cannot stat 'ocamlc': No such file or directory
ocaml> make[1]: *** [Makefile:179: promote-common] Error 1
ocaml> make[1]: Leaving directory '/build/ocaml-4.12.0'
ocaml> make: *** [Makefile:233: coreboot] Error 2
ocaml> make: *** Waiting for unfinished jobs....
```

Such an early failure! Note that builder calls `make` with multiple
targets: `make -j16 world bootstrap world.opt`. Looking at
`Makefile` reveals an interesting pattern:
<https://github.com/ocaml/ocaml/blob/trunk/Makefile#L305>

``` Makefile
.PHONY: world
world: coldstart
        $(MAKE) all

# Compile also native code compiler and libraries, fast
.PHONY: world.opt
world.opt: checknative
        $(MAKE) coldstart
        $(MAKE) opt.opt
```

Normally I would expect it to be something like:

``` Makefile
world: coldstart all
world.opt: checknative coldstart opt.opt
```

Otherwise nothing prevents `coldstrat` to be built from both `world`
and `world.opt` targets in parallel. That's what happens in our case.
To work around this failure my suggestion is to artificially sequence
targets together:

``` Makefile
nixpkgs_world_bootstrap_world_opt:
        $(MAKE) world
        $(MAKE) bootstrap
        $(MAKE) world.opt
```

That way we can still preserve the ordering and have each component to
build in parallel. Longer term the build system should allow us to
specify multiple top-level targets. They should not be special in any
way.
This change speeds `ocaml` build time from `6m55s` to `1m35s` on my
machines. A `4.3x` speedup does not sound bad for 4 lines of `Makefile`.

## `cdparanoiaIII`

`cdparanoiaIII` took me quite a while to be able to reproduce the
failure. The following managed to trigger rebuild eventually:

``` 
$ while nix build -f . cdparanoiaIII --rebuild --cores 64; do echo again; done
...
make libcdda_interface.a CFLAGS="-O2 -fsigned-char -g -O2"
make[2]: Entering directory '/build/cdparanoia-III-10.2/interface'
gcc -O2 -fsigned-char -g -O2 -c scan_devices.c
...
make[2]: Leaving directory '/build/cdparanoia-III-10.2/interface'
make libcdda_interface.so CFLAGS="-O2 -fsigned-char -g -O2 -fpic"
...
gcc -O2 -fsigned-char -g -O2 -fpic -c scan_devices.c
...
gcc -fpic -shared -o libcdda_interface.so.0.10.2 -Wl,-soname -Wl,libcdda_interface.so.0 scan_devices.o  common_interface.o cooked_interface.o interface.o scsi_interface.o smallft.o toc.o test_interface.o -lm -lrt
scan_devices.o: file not recognized: file format not recognized
collect2: error: ld returned 1 exit status
make[2]: *** [Makefile:49: libcdda_interface.so] Error 1
```

Here `scan_devices.o` gets rebuilt twice within a single `make`
invocation: once with `-fpic` option (to build shared library) and
once without (to build static library):
<https://svn.xiph.org/trunk/cdparanoia/interface/Makefile.in>.
`interface/Makefile.in` is very illustrative in this case:

``` Makefile
all: lib slib

lib:
        $(MAKE) libcdda_interface.a CFLAGS="$(OPT)"

slib:
        $(MAKE) libcdda_interface.so CFLAGS="$(OPT) -fpic"

libcdda_interface.a:    $(OFILES)
        $(AR) -r libcdda_interface.a $(OFILES)
        $(RANLIB) libcdda_interface.a

libcdda_interface.so:   $(OFILES)
        $(CC) -fpic -shared -o libcdda_interface.so.0.$(VERSION) -Wl,-soname -Wl,libcdda_interface.so.0 $(OFILES) $(LIBS)

.c.o:
        $(CC) $(CFLAGS) -c $<
```

Note that `$(OFILES)` are shared across `libcdda_interface.a` and
`libcdda_interface.so`. Nothing prevented both to run on parallel.
Nowadays those have slightly different names (or paths) to allow for
parallel build. Reusing file names in different parts of a single build
is very tricky.

## Is 12 failures a lot?

Getting 12 failures for my local system was a bit worrying: most of the
failures happened in somewhat niche but not that rare packages. What if
we try to build really obscure ones?
I attempted to build a few random packages from the whole of `nixpkgs`
as:

``` 
$ nix-env -qaP | awk '{print $1}' | sed -e 's/^nixos.//g' | shuf | xargs -n 10 nix build -f. --no-link
```

Over one night 20 more failures popped up in over `~5000` built packages:

- `pth`
- `openjdk`
- `libb64`
- `m17n_lib`
- `xalanc`
- `ion3`
- `espeak-ng`
- `dhcp`
- `judy`
- `ocaml-ng.ocamlPackages_4_12.stdcompat`
- `mlton20130715`, `mltonHEAD`
- `mlkit`
- `clisp`
- `cloog`
- `xfce.garcon`
- `munin`
- `jade`
- `ocamlPackages.camlidl`
- `lazarus-qt`
- `ocamlPackages_4_03.camlp4`
- `gengetopt`

I think we are heading towards `~200` failures across all the `nixpkgs`.
`~200` a lot more than I personally could triage and fix myself but is
very reasonable to offload to individual maintainers or enthusiasts who
would like to get their feet wet by fixing a simple package build
failure problem.

## Bonus question

So these are examples of failures when sequential build normally
succeeds due to deterministic rule execution order.

**The question**: Do you think there could be a bug when parallel build
almost always succeeds while sequential would always fail?

I encountered it once in my life: <https://bugs.gentoo.org/566620>. By
There missing dependency was always satisfied by a builder from other
rules running in parallel.
The fix adds missing dependency (as expected):
<https://github.com/skvadrik/re2c/commit/88a7cec2e644c6d890f68133ed8fd6362e5beeab>

## Parting words

To state the obvious: building packages in parallel speeds builds up.
Sometimes by a lot.
Vast majority of packages do support parallel builds. Probably not a
surprise to anyone. I think we can flip a default on in `nixpkgs` and
sort out the fallout.

Recursive calls to `$(MAKE)` work as is only if you build
non-overlapping targets (say, disjoint self-contained subdirectories).
They blow up when you try to do clever things like slight environment
change to rebuild the same target and reuse intermediate filenames or
have cross-directory dependencies.

There is a list of yet unfixed packages at
<https://github.com/NixOS/nixpkgs/issues/142338>. Feel free to grab one
and give it a try.

Have fun!
