---
title: "nixpkgs overlay nano howto"
date: July 31, 2022
---

`nixpkgs` is a huge repository of various packages. But what if you want
to package a tiny package and don't plan to share it with anyone. Is it
hard? How would you go about it?

There are many ways to do it: maintain a `nixpkgs` fork, use
`packageOverrides`, `overlays`, `flakes` and many others methods.

I'll focus here only on `overlays` method as it feels to me like the
simplest way to write packages ready to copy to (or from) `nixpkgs`.

## `nixpkgs` structure

Before we start with an example let's take a look at `nixpkgs`
"schema". `nixpkgs` provides a `pkgs` map ("attribute set") from package
name to package definition (it's nested in a few places):

```nix
pkgs = {
    callPackage = pkgPath: ...somehow-load-the-package;

    # unnested examples:
    glibc = callPackage ../development/libraries/glibc { ... };
    re2c = callPackage ../development/tools/parsing/re2c { };
    # ...

    # nested examples:
    python39Packages = {
      # the name is slightly changed for clarity
      callPythonPackage = pkgPath: ...somehow-load-python-package;

      black = callPackage ../development/python-modules/black { };
      blessed = callPackage ../development/python-modules/blessed { };
      # ...
    };
};
```

Our goal here is to inject something very simple into top-level
`pkgs = { ... };` structure. We'll ignore nested attributes.
`overlays` allow us to override existing attributes in `pkgs` or
introduce the existing ones. Until you get familiar with the way
attributes interact with one another I suggest adding only new
attributes.

## an example

Our running example will be [`ski`](https://github.com/trofi/ski) package.
It's an `autotools`-based package with very conventional dependencies.
Let's package it!

I'll do 3 things below:

1. create `/tmp/overlay/ski/default.nix` expression ready to be included
   into `nixpkgs` repository
2. create `/tmp/overlay/local-packages.nix` expression ready to be used
   in `/etc/nixos/configuration.nix` and/or in `~/.config/nixpkgs/overlays/`.
3. add our overlay to `/etc/nixos/configuration.nix`.

Here is a simple `/tmp/overlay/ski/default.nix` expression enough to
build it:

```nix
# $ cat /tmp/overlay/ski/default.nix
{ lib , stdenv , fetchFromGitHub

, autoconf, automake, bison, flex, gperf
, libtool, pkg-config

, elfutils, libbfd, libiberty , ncurses
}:

stdenv.mkDerivation rec {
  pname = "ski";
  version = "unstable-2022-07-07";

  src = fetchFromGitHub {
    owner = "trofi";
    repo = "ski";
    rev = "568efd789fab1f932aa926b1db86dcb75e9c115c";
    sha256 = "sha256-dwHccL89bXzsjDr8O1DmVHlBQQ6aHgNLEaHJCJqHG9w=";
  };

  postPatch = ''
    ./autogen.sh
  '';

  nativeBuildInputs = [ autoconf automake
    bison flex gperf libtool pkg-config ];

  buildInputs = [ elfutils libbfd libiberty ncurses ];

  meta = with lib; {
    description = "ia64 (Itanium) instruction set simulator.";
    homepage = "https://github.com/trofi/ski";
    license = licenses.gpl2Only;
    platforms = platforms.linux;
  };
}
```

Now we need to create an actual overlay expression. I'll put it in a
separate `/tmp/overlay/local-packages.nix` file as well:

```nix
# $ cat /tmp/overlay/local-packages.nix
final: prev: {
  # we create new 'ski' attribute here!
  ski = final.callPackage ./ski {};

  # add more packages below:
  # ...
}
```

Now we are ready to use the overlay in our
`/etc/nixos/configuration.nix`:

```nix
{ config, pkgs, ... }:

{
  # Add an overlay to augment existing 'pkgs' map.
  nixpkgs.overlays = [
    (import /tmp/overlay/local-packages.nix)
  ];

  # use augmented 'pkgs':
  environment.systemPackages = with pkgs; [
    ski
  ];
  # ...
}
```

Now we can get `ski` installed into our system:

```
$ sudo nixos-rebuild switch
$ ski -help
Options:
    -help        Display command-line options
    -i <file>    Process initialization file at startup
    -rest <file> Restore simulation state from <file>
    -nonet       Disable networking feature
    -srcroot     Source Root Directory
    -forceuser   Force user-level simulation
    -forcesystem Force system-level simulation
    -strace      Trace system call execution
    -simroot     Simulated root directory
    -conslog <file> Log the console output to the specified file
    -palen <n>      Implemented physical address bits.  Default: 63
    -valen <n>      Implemented virtual address bits.  Default: 61
    -ridlen <n>     Implemented RR.rid bits.  Default: 24
    -keylen <n>     Implemented PKR.key bits.  Default: 24
    -grfile <n>     GR file size.  Default: 128
```

Seems to work!

We can also get the packages pulled into user's `<nixpkgs>` expression:

```
$ mkdir -p ~/.config/nixpkgs/overlays/
$ ln -s /tmp/overlay/local-packages.nix ~/.config/nixpkgs/overlays/
```

Now we can use it as a `nixpkgs` attribute:

```
$ nix-build '<nixpkgs>' -A ski
/nix/store/rpb5iikr6p0x49zkpw5cjwp9lg8lnl7d-ski-unstable-2022-07-07
```

A few relevant links:

- [overlays in `nixpkgs` manual](https://nixos.org/manual/nixpkgs/stable/#sec-overlays-install)
- [overlays in `nixos.wiki`](https://nixos.wiki/wiki/Overlays)

Done!
