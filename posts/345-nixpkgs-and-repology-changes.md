---
title: "nixpkgs and repology changes"
date: January 25, 2026
---

## Tl;DR: `nixpkgs` changed the way it exports data to `repology` and many packages need fixing

In [`nixpkgs/451424`](https://github.com/NixOS/nixpkgs/pull/451424) `nixpkgs`
changed substantially how it exports data about available packages.
As a result some packages like `flare` stopped being reported to `repology.org`
correctly. Luckily the fix is to usually use `pname` / `version` instead of
direct `name`. Example [fix](https://github.com/NixOS/nixpkgs/pull/483476)
for `flare` package:

```diff
--- a/pkgs/by-name/fl/flare/package.nix
+++ b/pkgs/by-name/fl/flare/package.nix
@@ -6,7 +6,8 @@
 }:

 buildEnv {
-  name = "flare-1.14";
+  pname = "flare";
+  version = "1.14";

   paths = [
     (callPackage ./engine.nix { })
```

## The bug

I happen to maintain [`nix-olde`](https://github.com/trofi/nix-olde) program.
It's a tool to show you what outdated packages your system has installed.
The tool is hacky: both in a way how it looks up what you have installed
in the system and in how it maps that information to `repology.org` database.
It errs on the side to not print possibly wrong or missing information.

A few days ago I casually ran `nix-olde` against my system and got very
odd outputs, like:

```
        repology r:clock "0.7.4" | nixpkgs {"0.8.4"} {"haskellPackages.clock"}
        repology r:digest "0.6.39" | nixpkgs {"0.0.2.1"} {"haskellPackages.digest"}
        repology r:hedgehog "0.2" | nixpkgs {"1.5"} {"haskellPackages.hedgehog"}
        repology r:mmap "0.6-23" | nixpkgs {"0.5.9"} {"haskellPackages.mmap"}
        repology r:warp "0.2.3" | nixpkgs {"3.4.9"} {"haskellPackages.warp"}
        repology r:yaml "2.3.12" | nixpkgs {"0.11.11.2"} {"haskellPackages.yaml"}
```

Here `nix-olde` says that it compared `R` language packages against
`Haskell` language packages and was unhappy about version mismatch. That
looked very wrong. Turns out that `repology.org` changed the way it
reports `nixpkgs` packages for `nixpkgs_unstable` repository.
Before the change `repology` reported the following package descriptions:

```json
"python:networkx": [
  {
    "repo": "nixpkgs_unstable",
    "srcname": "python310Packages.networkx",
    "visiblenamename": "python3.10-networkx",
    "version": "2.8.6",
    "status": "outdated",
  },
]
```

After the change it started producing the following output:

```json
"python:networkx": [
  {
    "repo": "nixpkgs_unstable",
    "srcname": "python310Packages.networkx",
    "visiblename": "networkx",
    "version": "2.8.6",
    "status": "outdated",
  },
]
```

`nix-olde` used to rely on `visiblename`: it was conveniently not too
specific (did not contain a version) and yet was specific enough to contain
package ecosystem prefix. But now `visiblename` omits ecosystem entirely.
This caused all the problematic reports.
I fixed it with [this commit](https://github.com/trofi/nix-olde/commit/ec6313f5abf33ae2701afa6c165a8abc2087699e).

## How does `nixpkgs` export data to repology?

`repology.org` is configured to fetch package lists from `nixpkgs`
[here](https://github.com/repology/repology-updater/blob/85f1accfc4d6edd1c940a620b886b4b21c0c1fb8/repos.d/nixos.yaml#L65):

```yaml
- name: nix_unstable
  type: repository
  desc: nixpkgs unstable
  statsgroup: nix
  family: nix
  ruleset: [nix, nix_name]
  color: '7eb2dd'
  minpackages: 120000
  default_maintainer: fallback-mnt-nix@repology
  sources:
    - name: packages-unstable.json
      fetcher:
        class: FileFetcher
        url: https://channels.nixos.org/nixos-unstable/packages.json.br
      parser:
        class: NixJsonParser
        use_pname: true
```

Note that it was changed
[very recently](https://github.com/repology/repology-updater/commit/e134ff779ed1f776784473403dca19bdd3a64a64) to enable `use_pname`.

`nixpkgs` on its side generates `packages.json.br` with an equivalent of
[this](https://github.com/NixOS/nixpkgs/blob/aa6e0f1bcb02bcb16084e5ec00d56df94e1e235e/pkgs/top-level/make-tarball.nix#L47):

```bash
NIX_STATE_DIR=$TMPDIR NIX_PATH= nix-instantiate --eval --raw --expr "import $src/pkgs/top-level/packages-info.nix {}" | sed "s|$src/||g" | jq -c > packages.json
brotli -9 < packages.json > packages.json.br
```

Note that this also
[changed recently](https://github.com/NixOS/nixpkgs/commit/e6fd1262842edfd00c54523a4b18d1a16f5c0587).
Before the change the code used to use `nix-env`:

```bash
(
  echo -n '{"version":2,"packages":'
  NIX_STATE_DIR=$TMPDIR NIX_PATH= nix-env -f $src -qa --meta --json --show-trace --arg config 'import ${./packages-config.nix}'
  echo -n '}'
) | sed "s|$src/||g" | jq -c > packages.json
brotli -9 < packages.json > packages.json.br
```

## Is there a difference introduced?

Why does it matter? Conveniently currently only `nixpkgs_unstable` has
the change. Previous releases like `nixos-25.11` still use the previous
mechanism. Let's compare the outputs of `python3Packages.networkx`:

```
$ curl -L https://channels.nixos.org/nixos-unstable/packages.json.br > unstable-packages.json.br

$ brotli -d <unstable-packages.json.br | jq '.packages."python313Packages.networkx"|[.name, .pname, .version]'
[
  "python3.13-networkx-3.5",
  "networkx",
  "3.5"
]

$ curl -L https://channels.nixos.org/nixos-25.11/packages.json.br > 25.11-packages.json.br

$ brotli -d <25.11-packages.json.br | jq '.packages."python313Packages.networkx"|[.name, .pname, .version]'
[
  "python3.13-networkx-3.5",
  "python3.13-networkx",
  "3.5"
]
```

Here we see that `pname` changed from `python3.13-networkx` to `networkx`.
For this package it's a reasonable change. Let's look at `flare` instead:

```
$ brotli -d <unstable-packages.json.br | jq '.packages.flare|[.name, .pname, .version]'
[
  "flare-1.14",
  "flare-1.14",
  ""
]

$ brotli -d <25.11-packages.json.br | jq '.packages.flare|[.name, .pname, .version]'
[
  "flare-1.14",
  "flare",
  "1.14"
]
```

Here the change effectively broke both package name and version reporting
in `unstable`. As a result `flare-rpg` is missing an `unstable` entry on
[repology](https://repology.org/project/flare-rpg/versions):

```
nixpkgs stable 23.11	flare	1.14
nixpkgs stable 24.05	flare	1.14
nixpkgs stable 24.11	flare	1.14
nixpkgs stable 25.05	flare	1.14
nixpkgs stable 25.11	flare	1.14
```

Normally `unstable` entry looks like
[this](https://repology.org/project/re2c/versions):

```
nixpkgs stable 23.11	re2c	3.1
nixpkgs stable 24.05	re2c	3.1
nixpkgs stable 24.11	re2c	3.1
nixpkgs stable 25.05	re2c	4.1
nixpkgs stable 25.11	re2c	4.3.1
nixpkgs unstable	re2c	4.4
```

## Why does it happen?

Before the [`nixpkgs/451424`](https://github.com/NixOS/nixpkgs/pull/451424)
change the split from `"flare-1.14"` down to `"flare" "1.14"` was done
by `nix-env -qa` itself! It does not have any advanced heuristics. I
noticed it before when I just started on `nix-olde`.
For example in
[`nix/7540`](https://github.com/NixOS/nix/issues/7540) `nix` splits
`"font-adobe-75dpi-1.0.3"` in an unexpected way:

```
$ nix-env -f. -qa --json | fgrep -A9  xorg.fontadobe75dpi
  "xorg.fontadobe75dpi": {
    "name": "font-adobe-75dpi-1.0.3",
    "outputName": "out",
    "outputs": {
      "out": null
    },
    "pname": "font-adobe",
    "system": "x86_64-linux",
    "version": "75dpi-1.0.3"
  },
```

It's `"font-adobe" "75dpi-1.0.3"`. But it should have been
`"font-adobe-75dpi" "1.0.3"` instead!

Normally all 3 `name / pname / version` come directly from the attributes
of a package:

```
$ nix repl -f '<nixpkgs>'

nix-repl> with re2c; [ name pname version ]
[
  "re2c-4.4"
  "re2c"
  "4.4"
]
```

For some packages not all of them are defined:

```
nix-repl> with flare; [ name pname version ]
[
  "flare-1.14"
  «error: undefined variable 'pname'»
  «error: undefined variable 'version'»
]
```

The fix is usually simple: instead of defining `name` directly use
`pname` / `version` split. For [example](https://github.com/NixOS/nixpkgs/pull/483476):

```diff
--- a/pkgs/by-name/fl/flare/package.nix
+++ b/pkgs/by-name/fl/flare/package.nix
@@ -6,7 +6,8 @@
 }:

 buildEnv {
-  name = "flare-1.14";
+  pname = "flare";
+  version = "1.14";

   paths = [
     (callPackage ./engine.nix { })
```

## How does repology handle such packages?

Does `repology` do anything special about these `"version": ""` cases?
`repology-updater/repology/parsers/parsers/nix.py` handles versions
somewhere [here](https://github.com/repology/repology-updater/blob/85f1accfc4d6edd1c940a620b886b4b21c0c1fb8/repology/parsers/parsers/nix.py#L160).

It already has to work around cases of wrong version splits:

```python
    for verprefix in ['100dpi', '75dpi']:
        if packagedata['version'].startswith(verprefix):
            pkg.log('dropping "{}", "{}" does not belong to version'.format(packagedata['name'], verprefix), severity=Logger.ERROR)
            skip = True
            break
```

Otherwise, `"version"` is extracted
[as is](https://github.com/repology/repology-updater/blob/85f1accfc4d6edd1c940a620b886b4b21c0c1fb8/repology/parsers/parsers/nix.py#L180):

```python
    pname = packagedata['pname']
    version = packagedata['version']
    # This is temporary solution (see #854) which overrides pname and version with ones
    # (ambigiously) parsed from name. That's what nix currently does (instead of exposing
    # explicitly set pname and version), and we do the same instead of using pname/version
    # provided by them to avoid unexpected change in data when/if they change their logic
    # As soon as they do and changed data is verified, this block may be removed
    match = re.match('(.+?)-([0-9].*)$', packagedata['name'])
    if match is None:
        pkg.log('cannot parse name "{}"'.format(packagedata['name']), severity=Logger.ERROR)
        continue
    elif not self._use_pname:
        pname = match.group(1)
        version = match.group(2)
```

Thus, `_use_pname` (enabled in `nixpkgs_unstable`) effectively disables `name` version
heuristics.

## Are there any more affected packages?

I wondered now many more packages in `packages.json` that do have
version in `pname` with a version specified, but lack `name` form:

```
$ brotli -d <unstable-packages.json.br | fgrep -B4 '"version": ""' d | fgrep pname | sort -u | grep -P -- '-[0-9]+(\.[0-9]+)*\"' | wc -l
260
```

Here I filtered only `pname` that seemingly look like a version. I probably
missed a few complicated cases. But it's a good first pass. Here are
package examples:

```
$ brotli -d <unstable-packages.json.br | fgrep -B4 '"version": ""' d | fgrep pname | sort -u | grep -P -- '-[0-9]+(\.[0-9]
      "pname": "afro-graphics-theme-47.05",
      "pname": "ajantv2-module-17.5.0-5.10.248",
      "pname": "ajantv2-module-17.5.0-5.15.198",
      "pname": "ajantv2-module-17.5.0-6.1.161",
      "pname": "ajantv2-module-17.5.0-6.12.66",
      "pname": "ajantv2-module-17.5.0-6.12.67",
      "pname": "ajantv2-module-17.5.0-6.18.6",
      "pname": "ajantv2-module-17.5.0-6.18.7",
      "pname": "ajantv2-module-17.5.0-6.6.121",
      "pname": "android-studio-for-platform-2024.2.2.13",
      "pname": "android-studio-for-platform-canary-2024.3.1.9",
      "pname": "auditable-cargo-1.92.0",
      "pname": "autoreiv-theme-47.01",
      "pname": "bbswitch-unstable-2021-11-29-5.10.248",
      "pname": "bbswitch-unstable-2021-11-29-5.15.198",
      "pname": "bbswitch-unstable-2021-11-29-6.1.161",
      "pname": "bbswitch-unstable-2021-11-29-6.12.66",
      "pname": "bbswitch-unstable-2021-11-29-6.12.67",
      "pname": "bbswitch-unstable-2021-11-29-6.18.6",
      "pname": "bbswitch-unstable-2021-11-29-6.18.7",
      "pname": "bbswitch-unstable-2021-11-29-6.6.121",
      "pname": "binary-black-2024-02-15",
      "pname": "binary-blue-2024-02-15",
      "pname": "binary-red-2024-02-15",
      "pname": "binary-white-2024-02-15",
      "pname": "broadcom-sta-6.30.223.271-59-5.10.248",
      "pname": "broadcom-sta-6.30.223.271-59-5.15.198",
      "pname": "broadcom-sta-6.30.223.271-59-6.1.161",
      "pname": "broadcom-sta-6.30.223.271-59-6.12.66",
      "pname": "broadcom-sta-6.30.223.271-59-6.12.67",
      "pname": "broadcom-sta-6.30.223.271-59-6.18.6",
      "pname": "broadcom-sta-6.30.223.271-59-6.18.7",
      "pname": "broadcom-sta-6.30.223.271-59-6.6.121",
      "pname": "bundler-audit-0.9.2",
      "pname": "cabal2nix-2.21.0",
      "pname": "caribou-0.4.21",
      "pname": "catppuccin-frappe-2024-02-15",
      "pname": "catppuccin-latte-2024-02-15",
      "pname": "catppuccin-macchiato-2024-02-15",
      "pname": "catppuccin-mocha-2024-02-15",
      "pname": "cctools-binutils-darwin-dualas-1010.6",
      "pname": "cfn-nag-0.8.10",
      "pname": "chicken-base64-3.3.1",
      "pname": "chicken-defstruct-1.6",
      "pname": "chicken-http-client-0.18",
      "pname": "chicken-intarweb-1.7",
      "pname": "chicken-matchable-3.7",
      "pname": "chicken-sendfile-1.8.3",
      "pname": "chicken-simple-md5-0.0.1",
      "pname": "chicken-uri-common-1.4",
      "pname": "chicken-uri-generic-2.46",
      "pname": "cloudformation-0.9.64",
      "pname": "compass-1.0.3",
      "pname": "d1x-rebirth-full-2.0.0.7",
      "pname": "d2x-rebirth-full-2.0.0.7",
      "pname": "dbus-1",
      "pname": "deadbeef-with-plugins-1.10.0",
      "pname": "dejavu-fonts-2.37",
      "pname": "Dell-5130cdn-Color-Laser-1.3-1",
      "pname": "dfgraphics-theme-42.05",
      "pname": "distcc-masq-gcc-15.2.0",
      "pname": "docbook-sgml-3.1",
      "pname": "docbook-sgml-4.1",
      "pname": "dracula-2020-07-02",
      "pname": "drawio-headless-29.0.3",
      "pname": "eclipse-plugin-antlr-runtime-4.5.3",
      "pname": "eclipse-plugin-antlr-runtime-4.7.1",
      "pname": "ecm-7.0.6",
      "pname": "exact-audio-copy-1.8.0",
      "pname": "faust2alqt-2.83.1",
      "pname": "faust2alsa-2.83.1",
      "pname": "faust2csound-2.83.1",
      "pname": "faust2firefox-2.83.1",
      "pname": "faust2jack-2.83.1",
      "pname": "faust2jackrust-2.83.1",
      "pname": "faust2jaqt-2.83.1",
      "pname": "faust2ladspa-2.83.1",
      "pname": "faust2lv2-2.83.1",
      "pname": "faust2sc.py-2.83.1",
      "pname": "faust2sndfile-2.83.1",
      "pname": "fcitx5-with-addons-5.1.16",
      "pname": "flare-1.14",
      "pname": "fluentd-1.18.0",
      "pname": "foreman-0.87.2",
      "pname": "frogatto-unstable-2023-02-27",
      "pname": "geany-with-vte-2.1",
      "pname": "gear-2022-04-19",
      "pname": "gemset-theme-47.05",
      "pname": "gimp-with-plugins-2.10.38",
      "pname": "gimp-with-plugins-3.0.6",
      "pname": "git_fame-3.2.19",
      "pname": "gitweb-2.52.0",
      "pname": "glibc-iconv-2.42",
      "pname": "glibc-multi-2.42-47",
      "pname": "glob2-0.9.4.4",
      "pname": "gradient-grey-2018-10-20",
      "pname": "helm-3.19.1",
      "pname": "hiera-eyaml-4.3.0",
      "pname": "homesick-1.1.6",
      "pname": "html-proofer-5.0.8",
      "pname": "hxnodejs-6.9.0",
      "pname": "ibus-with-plugins-1.5.33",
      "pname": "idris-1.3.4",
      "pname": "idris-with-packages-1.3.4",
      "pname": "indi-full-2.1.6",
      "pname": "indi-full-nonfree-2.1.6",
      "pname": "indi-with-drivers-2.1.6",
      "pname": "inkscape-with-extensions-1.4.3",
      "pname": "ironhand-theme-47.05",
      "pname": "jolly-bastion-theme-47.04",
      "pname": "jool-4.1.14-5.10.248",
      "pname": "jool-4.1.14-5.15.198",
      "pname": "jool-4.1.14-6.1.161",
      "pname": "jool-4.1.14-6.12.66",
      "pname": "jool-4.1.14-6.12.67",
      "pname": "jool-4.1.14-6.18.6",
      "pname": "jool-4.1.14-6.18.7",
      "pname": "jool-4.1.14-6.6.121",
      "pname": "kakoune-2025.06.03",
      "pname": "keeagent-0.12.0",
      "pname": "keepass-charactercopy-1.0.0",
      "pname": "keepasshttp-1.8.4.2",
      "pname": "keepass-keetraytotp-0.108.0",
      "pname": "keepass-qrcodeview-1.0.4",
      "pname": "keepassrpc-1.16.0",
      "pname": "klibc-2.0.14",
      "pname": "legends-browser-1.19.2",
      "pname": "libidn2-2.3.8",
      "pname": "libxml2+py-2.15.1",
      "pname": "license_finder-7.0.1",
      "pname": "llvm-binutils-18.1.8",
      "pname": "llvm-binutils-19.1.7",
      "pname": "llvm-binutils-20.1.8",
      "pname": "llvm-binutils-21.1.8",
      "pname": "matrix-synapse-wrapped-1.145.0",
      "pname": "mayday-theme-47.05",
      "pname": "moonscape-2022-04-19",
      "pname": "mosaic-blue-2016-02-19",
      "pname": "mpv-with-scripts-0.41.0",
      "pname": "msp430-newlib-4.5.0.20241231",
      "pname": "nemo-with-extensions-6.6.3",
      "pname": "net-tools-1003.1-2008",
      "pname": "nineish-2019-12-04",
      "pname": "nineish-catppuccin-frappe-2025-01-27",
      "pname": "nineish-catppuccin-frappe-alt-2025-01-27",
      "pname": "nineish-catppuccin-latte-2025-01-27",
      "pname": "nineish-catppuccin-latte-alt-2025-01-27",
      "pname": "nineish-catppuccin-macchiato-2025-01-27",
      "pname": "nineish-catppuccin-macchiato-alt-2025-01-27",
      "pname": "nineish-catppuccin-mocha-2025-01-27",
      "pname": "nineish-catppuccin-mocha-alt-2025-01-27",
      "pname": "nineish-dark-gray-2020-07-02",
      "pname": "nineish-dark-gray-2021-07-20",
      "pname": "nineish-dark-light-2021-07-20",
      "pname": "nix-generate-from-cpan-3",
      "pname": "nix-index-0.1.9",
      "pname": "nixops-2.0.0-unstable-2025-12-28",
      "pname": "obsidian-theme-47.05",
      "pname": "ocaml5.3.0-uucp-17.0.0",
      "pname": "ocaml5.3.0-vg-0.9.5",
      "pname": "ocaml5.4.0-uucp-17.0.0",
      "pname": "ocaml5.4.0-vg-0.9.5",
      "pname": "open-watcom-bin-1.9",
      "pname": "open-watcom-v2-0-unstable-2025-11-15",
      "pname": "otpkeyprov-2.6",
      "pname": "phoebus-theme-47.05",
      "pname": "plikd-1.3.7",
      "pname": "postgresql-plperl-14.20",
      "pname": "postgresql-plperl-15.15",
      "pname": "postgresql-plperl-16.11",
      "pname": "postgresql-plperl-17.7",
      "pname": "postgresql-plperl-18.1",
      "pname": "postgresql-plpython3-14.20",
      "pname": "postgresql-plpython3-15.15",
      "pname": "postgresql-plpython3-16.11",
      "pname": "postgresql-plpython3-17.7",
      "pname": "postgresql-plpython3-18.1",
      "pname": "postgresql-pltcl-14.20",
      "pname": "postgresql-pltcl-15.15",
      "pname": "postgresql-pltcl-16.11",
      "pname": "postgresql-pltcl-17.7",
      "pname": "postgresql-pltcl-18.1",
      "pname": "postgrey-1.37",
      "pname": "powerline-symbols-2.8.4",
      "pname": "procps-1003.1-2008",
      "pname": "python3.13-subunit-1.4.5",
      "pname": "python3.14-subunit-1.4.5",
      "pname": "python3-3.13.11-llm-0.28",
      "pname": "rally-ho-theme-47.05",
      "pname": "recursive-2022-04-19",
      "pname": "retroarch-with-cores-1.22.2",
      "pname": "roundcube-plugin-carddav-4.4.6",
      "pname": "roundcube-plugin-contextmenu-3.3.1",
      "pname": "roundcube-plugin-custom_from-1.6.6",
      "pname": "roundcube-plugin-persistent_login-5.3.0",
      "pname": "roundcube-plugin-thunderbird_labels-1.6.0",
      "pname": "run-npush-0.7",
      "pname": "scope-lite-0.2.0",
      "pname": "service-wrapper-19.04",
      "pname": "signwriting-1.1.4",
      "pname": "simple-blue-2016-02-19",
      "pname": "simple-dark-gray-2016-02-19",
      "pname": "simple-dark-gray-2018-08-28",
      "pname": "simple-dark-gray-bootloader-2018-08-28",
      "pname": "simple-light-gray-2016-02-19",
      "pname": "simple-red-2016-02-19",
      "pname": "stripes-2016-02-19",
      "pname": "stripes-logo-2016-02-19",
      "pname": "system76-io-module-1.0.4-5.10.248",
      "pname": "system76-io-module-1.0.4-5.15.198",
      "pname": "system76-io-module-1.0.4-6.1.161",
      "pname": "system76-io-module-1.0.4-6.12.66",
      "pname": "system76-io-module-1.0.4-6.12.67",
      "pname": "system76-io-module-1.0.4-6.18.6",
      "pname": "system76-io-module-1.0.4-6.18.7",
      "pname": "system76-io-module-1.0.4-6.6.121",
      "pname": "system76-module-1.0.17-5.10.248",
      "pname": "system76-module-1.0.17-5.15.198",
      "pname": "system76-module-1.0.17-6.1.161",
      "pname": "system76-module-1.0.17-6.12.66",
      "pname": "system76-module-1.0.17-6.12.67",
      "pname": "system76-module-1.0.17-6.18.6",
      "pname": "system76-module-1.0.17-6.18.7",
      "pname": "system76-module-1.0.17-6.6.121",
      "pname": "systemtap-5.4",
      "pname": "taffer-theme-47.04",
      "pname": "teamocil-1.4.2",
      "pname": "tectonic-wrapped-0.15.0",
      "pname": "tergel-theme-47.01",
      "pname": "travis-1.9.1",
      "pname": "tsm-client-8.1.27.1",
      "pname": "unicode-emoji-17.0.0",
      "pname": "usbip-linux-5.10.248",
      "pname": "usbip-linux-5.15.198",
      "pname": "usbip-linux-6.1.161",
      "pname": "usbip-linux-6.12.67",
      "pname": "usbip-linux-6.18.7",
      "pname": "usbip-linux-6.6.121",
      "pname": "usbip-linux-hardened-6.12.66",
      "pname": "usbip-linux-lqx-6.18.6",
      "pname": "usbip-linux-xanmod-6.12.66",
      "pname": "usbip-linux-xanmod-6.18.6",
      "pname": "usbip-linux-zen-6.18.6",
      "pname": "util-linux-1003.1-2008",
      "pname": "vdr-epgtableid0-2.6.9",
      "pname": "vdr-hello-2.6.9",
      "pname": "vdrift-unstable-2021-09-05-with-data-1446",
      "pname": "vdr-osddemo-2.6.9",
      "pname": "vdr-pictures-2.6.9",
      "pname": "vdr-servicedemo-2.6.9",
      "pname": "vdr-skincurses-2.6.9",
      "pname": "vdr-status-2.6.9",
      "pname": "vdr-svdrpdemo-2.6.9",
      "pname": "vdr-with-plugins-2.6.9",
      "pname": "vettlingr-theme-47.05",
      "pname": "vscode-with-extensions-1.108.1",
      "pname": "wanderlust-theme-47.04",
      "pname": "waterfall-2022-04-19",
      "pname": "watersplash-2022-04-19",
      "pname": "wayfire-wrapped-0.10.1",
```

At least `flare` is in the list. I think most of these require a similar
fix.

## Parting words

`nixpkgs` now exposes slight less mangled data to `repology.org` for version
comparison. Unfortunately `nixpkgs` itself is not fully switched to
`pname` / `version` everywhere. And thus a small set of data is now lost.
It should be easy to find and to restore those with a fix similar to
[`flare`](https://github.com/NixOS/nixpkgs/pull/483476).

Have fun!
