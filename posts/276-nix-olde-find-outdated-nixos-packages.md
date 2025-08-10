---
title: "nix-olde - find outdated NixOS packages"
date: January 19, 2023
---

## Tl;DR

I wrote a [nix-olde](https://github.com/trofi/nix-olde) tool to print
outdated packages on your `NixOS` system. Run it as:

```
$ git clone https://github.com/trofi/nix-olde
$ cd nix-olde
$ cargo build && target/debug/nix-olde
...
Fetching 'installed' ...
Fetching 'repology' ...
Fetching 'available' ...
... 'available' done.
... 'installed' done.
... 'repology' done.
repology a52dec "0.8.0" | nixpkgs {"0.7.4"} {"nixos.a52dec"}
repology alsa-lib "1.2.8" | nixpkgs {"1.2.7.2"} {"nixos.alsa-lib"}
...
repology xterm "378" | nixpkgs {"377"} {"nixos.xterm"}
repology xz "5.4.1" | nixpkgs {"5.4.0"} {"nixos.xz"}
repology zxing-cpp-nu-book "2.0.0" | nixpkgs {"1.4.0"} {"nixos.zxing-cpp"}
```

Here we see all sorts of packages outdated in `nixpkgs` along with their
versions and versions known to `repology.org` database and a package
attribute name.

I wrote it for the purpose of tricking you (and myself) to send pull
requests against [`nixpkgs`](https://github.com/NixOS/nixpkgs) to update
some of those outdated packages :). Most of the time there is no major
reason why a specific package is outdated.

## More words

[A while ago](/posts/272-peeking-at-stale-nixpkgs-packages.html) I
shared a set of hacks I use to get a list of outdated packages. My
system has ~1550 packages. How many of them are actually outdated?
Let's compare that hack with `nix-olde` result.
The hack looked good enough to catch packages with simple naming scheme
that patches upstream, `nixpkgs` and `repology`. It returned 240
packages (about 15%). It's quite a lot. I would expect not that many.
Moreover, some packages never got into the list: none of `python`,
`perl` or `haskell` are in the report either. I skimmed through all
outdated [`nix unstable` package known to `repology`](https://repology.org/projects/?inrepo=nix_unstable&outdated=1)
and found a few `python` and `haskell` I use. Not good.

## `nix-olde` improvements

My hack clearly failed to map some of packages back to `repology` names.
If I could just print unresolved packages maybe it would be a good
starting point to see what I fail to cover. Thus, the tool's idea was
born.

The idea was simple:

- get local package list of **installed** packages
- get local package list of **available** packages with their `nix`
  attributes out of `<nixpkgs>` path.
- **fetch repology list** of available and outdated packages in `nixpkgs`

Having looked at `list of available packages` I realized it's exactly
the source `nixpkgs` uses to tell `repology` what's in `nixpkgs`!
It's hidden in [make-tarball.nix](https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/make-tarball.nix#L67):

```
$ echo -n '{"version":2,"packages":' > tmp
$ nix-env -f . -I nixpkgs=$src -qa --meta --json --arg config 'import ${./packages-config.nix}' >> tmp
echo -n '}'
```

Here we extract metadata around `<nixpkgs>` attributes by passing a
special `config` that untangles metadata from current system.
Thus, the mapping from installed to available packages is trivial using
the same technique and the same output data.
With help of [`serde`](https://serde.rs/) `rust` crate I was able to get a
trivial `json` reader in 2 lines of code.

`nix-olde` reports `391` outdated package (`151` more, or about `25%` of
packages). That is a substantial improvement over a hack. New additions
are mainly `python` and `perl` packages.

As a bonus with `--verbose` flag I can see now what packages fail to map
an attribute to `repology` database:

```
$ ./mkrun.bash -n ~/n --verbose
...
Installed packages missing in available list: ["antlr-runtime-cpp-4.9.3",
"binutils-patchelfed-ld-2.40", "binutils-patchelfed-ld-wrapper-2.40",
"boost-build-boost-1.79.0", "bootstrap-stage0-binutils-wrapper-",
"bootstrap-stage0-glibc-bootstrap", "bootstrap-stage1-gcc-wrapper-",
"bootstrap-stage2-gcc-wrapper-", "bootstrap-stage3-gcc-wrapper-",
"bootstrap-stage4-gcc-wrapper-13.0.0", "cargo-bootstrap-1.65.0",
"catalog-legacy-uris.patch", "compiler-rt-static-wasm32-unknown-wasi-12.0.1",
"d3-flame-graph-templates-4.1.3", "dejavu-fonts-full-2.37",
"dejavu-fonts-minimal-2.37", "eglexternalplatform-1.1", "gfortran-13.0.0",
"gmp-with-cxx-stage3-6.2.1", "gmp-with-cxx-stage4-6.2.1",
"i686-w64-mingw32-binutils-2.40", "i686-w64-mingw32-binutils-wrapper-2.40",
"i686-w64-mingw32-stage-final-gcc-13.0.0",
"i686-w64-mingw32-stage-final-gcc-wrapper-13.0.0",
"i686-w64-mingw32-stage-static-gcc-13.0.0",
"i686-w64-mingw32-stage-static-gcc-wrapper-13.0.0", "isl-stage3-0.20",
"libcxx-static-wasm32-unknown-wasi-12.0.1",
"libcxxabi-static-wasm32-unknown-wasi-12.0.1", "libmpc-stage3-1.3.1",
"linux-config-6.1.6", "linux-headers-static-6.1",
"mcfgthreads-i686-w64-mingw32-git", "mcfgthreads-x86_64-w64-mingw32-git",
"mingw-w64-i686-w64-mingw32-10.0.0", "mingw-w64-x86_64-w64-mingw32-10.0.0",
"mpfr-stage3-4.2.0", "musl-static-x86_64-unknown-linux-musl-1.2.3",
"nixos-version", "nss-cacert-certdata-3.86",
"python3.10-cryptography-vectors-38.0.4",
"python3.10-pycryptodome-test-vectors-1.0.10",
"rakshasa-libtorrent-0.13.8+date=2021-08-07", "ruby2.7.6-msgpack-1.5.1",
"ruby2.7.6-neovim-0.9.0", "rubygems-3.3.20", "rustc-bootstrap-1.65.0",
"rustfmt-nightly-1.66.1", "systemtap-4.5", "texlive-bin-2022",
"texlive-core-big.bin-2022", "vkd3d-1.5", "wasilibc-static-wasm32-unknown-wasi-17",
"wasm32-unknown-wasi-clang-wrapper-12.0.1",
"wasm32-unknown-wasi-llvm-binutils-wrapper-12.0.1",
"x86_64-unknown-linux-musl-binutils-2.40",
"x86_64-unknown-linux-musl-binutils-wrapper-2.40",
"x86_64-unknown-linux-musl-stage-final-gcc-13.0.0",
"x86_64-unknown-linux-musl-stage-final-gcc-wrapper-13.0.0",
"x86_64-unknown-linux-musl-stage-static-gcc-13.0.0",
"x86_64-unknown-linux-musl-stage-static-gcc-wrapper-13.0.0",
"x86_64-w64-mingw32-binutils-2.40", "x86_64-w64-mingw32-binutils-wrapper-2.40",
"x86_64-w64-mingw32-stage-final-gcc-13.0.0",
"x86_64-w64-mingw32-stage-final-gcc-wrapper-13.0.0",
"x86_64-w64-mingw32-stage-static-gcc-13.0.0",
"x86_64-w64-mingw32-stage-static-gcc-wrapper-13.0.0",
"xmms2-unstable-2022-12-30"]
```

Entries like `bootstrap-stage3-gcc-wrapper` are not very interesting:
they are the synthetic packages built for bootstrap and should be an alias
to a `nixpkgs`-specific shell wrapper. I'll try to filter them out by
default.

Entries like `gfortran` are `nixpkgs`-specific forms of `gcc`. It would
be nice to somehow alias those back to `gcc` as well. Maybe we should
provide a bit of extra fields in `meta` section of `nixpkgs` package
descriptions to make it machine-generated.

Entries like `xmms2` are the packages from my local overlays. It is
expected to be in the list.

Entries like `systemtap` look like a mapping bug. Needs more
investigation. At least these are a minority and are easily debuggable
to make `nix-olde` better reflect.

## Parting words

`nix-olde` was the first non-trivial program I wrote in `rust`. The
experience is very pleasant: option parsing and `json` parsing is
trivial, data types are naive and yet good enough to get the job done.

`nixpkgs` has quite a few outdated packages in base install for my
desktop system: around `25%`. Worth improving individual packages and
extending them to add auto-update scripts for [r-ryantm](https://ryantm.github.io/nixpkgs-update/r-ryantm/)
to help humans in this task.

Have fun!
