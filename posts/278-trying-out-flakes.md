---
title: "trying out flakes"
date: February 05, 2023
---

## Tl;DR

If you are tempted to try `flake`-based system configuration on your
flake-less `NixOS` but are a bit afraid of breaking existing setup
it can be done in a few lines of `flake.nix` without breaking backwards
compatibility. I did it this way:

```nix
{
  description = "Desktop system config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";

  outputs = { self, nixpkgs }: {
    nixosConfigurations = {
      nz = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./configuration.nix

          # Remove the need for `<nixos>` / `<nixos-config>`
          # management by `root` channel.
          ({config, pkgs, ...}: {
            nix.nixPath = [
              "nixpkgs=${nixpkgs}"
              "nixos-config=/etc/nixos/configuration.nix"
            ]; }) ]; }; }; };
}
```

The only thing you need to do on top is to create `git` repository out
of `/etc/nixos` and you are done:

```
# cd /etc/nixos
# git init
# git add flake.nix # and maybe a few more files you have there
# nixos-rebuild --impure swith
```

Done! Chances are you don't even need `--impure`. One more touch is to
adapt automatic updates if you have those:

```nix
{ ... }:
{
  system.autoUpgrade.enable = true;
  # workaround unrecognized --no-build-output
  system.autoUpgrade.flake = "/etc/nixos";
  system.autoUpgrade.flags = [
    # did not move out home directory overlay definition yet
    "--impure"

    # why not all inputs then?
    "--update-input" "nixpkgs"
    "--commit-lock-file"
  ];
}
```

Now it's all done!

## Long story

When I started using `nix` daily I only heard of
[nix flakes](https://nixos.wiki/wiki/Flakes) a bit. I did not dare to
try using them. Either for local development or for system-wide
configuration. From the documentation and notes around it felt
incompatible to current channel-based system configuration approach.

My biggest achievement was to run packages right off `github` pull
requests like:

```
$ nix run github:NixOS/nixpkgs/pull/175618/merge#firefox
```

That felt like magic: no need to clone a repository or reconfigure
anything in the system. You just build-and-run the expression out of
internet. But I was not sure I wanted more of `flakes` :)

## The need arose

Things have changed when Sandro tried
[nix-olde](https://github.com/trofi/nix-olde/) on `flake`-based system
and got [cryptic backtraces](https://github.com/trofi/nix-olde/issues/2)
back.

I realized I completely forgot about `flakes` existence. By now I had
about a year of experience dealing with channel-based system
configuration. I took it as a good opportunity to have a more detailed
look at `flakes`.

Normally `nix-olde` instantiates system derivation out of `<nixpkgs>`
expression via:

```
$ nix-instantiate '<nixpkgs/nixos>' -A system
/nix/store/66db0cgpvcbdfmqaz86wfv264w7k63n8-nixos-system-nz-23.05pre-git.drv
```

And then parses the `.drv` to extract the details about outdated
packages. `<nixpkgs>` is usually maintained by `root` via
`nix-channel`.

But what about `flakes`? Now does the equivalent work there if we build
the whole system out of it?

I made the simplest conversion possible by defining my system flake out
of existing `/etc/nixos/configuration.nix`:

```nix
# $ cat /etc/nixos/flake.nix
{
  description = "Desktop system config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: {
    nixosConfigurations.nz = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix
      ]; }; };
}
```

Surprisingly that was enough to make `nixos-rebuild --impure switch`
build my system successfully. Note: I intentionally build it from
`nixpkgs` `master` branch and not an `unstable` channel.

Then I removed `<nixpkgs>` channel defined by `root` user. I'm not sure
if I'm holding it wrong but it was enough to break `nix-olde`. I
[updated](https://github.com/trofi/nix-olde/commit/96cade0106689918c455fa0c9b78a2079aaf29c6)
`nix-olde` to support `flake` configuration. The gist of it is to
instantiate the system with a new path:

```
$ nix eval --impure --raw /etc/nixos#nixosConfigurations.$(hostname).config.system.build.toplevel.drvPath
/nix/store/x93fsz8451b0vxyz07db9879gllaq7a5-nixos-system-nz-23.05.20230205.b030e4a.drv
```

Then I noticed that `nix-update` and even `nix develop -f.` relies on
`<nixpkgs>` variable to be present. I defined compatible variable to
match system's `nixpkgs` checkout:

```nix
# $ cat /etc/nixos/flake.nix
{
  description = "Desktop system config";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs }: {
    nixosConfigurations.nz = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./configuration.nix

        ({config, pkgs, ...}: {
          nix.nixPath = [
            "nixpkgs=${nixpkgs}"
            "nixos-config=/etc/nixos/configuration.nix"
          ]; }) ]; }; };
}
```

That way I could still rebuild my system with
`nix build -f nixos system` and similar commands. And I did not change
`flake.nix` since. I still can use all the existing tools that rely on
`<nixpkgs>` path.

To fix auto-upgrade I had to reconfigure it slightly:

```nix
# $ cat /etc/nixos/configuration.nix
{ config, pkgs, ... }:
{
  system.autoUpgrade.enable = true;
  # workaround unrecognized --no-build-output
  system.autoUpgrade.flake = "/etc/nixos";
  system.autoUpgrade.flags = [
    # did not move out home directory overlay definition yet
    "--impure"

    # why not all inputs then?
    "--update-input" "nixpkgs"
    "--commit-lock-file"

    # Give all the CPUs to one job at a time.
    "--max-jobs" "1"
  ];
}
```

Here `--impure` allows file access outside explicit `input`s (I have a
few overlays lying all over the system). And `--update-input nixpkgs`
pulls `nixpkgs` update every time the system tries to update.

The result is not as scary as I initially suspected. This setup did not
break most of my workflows around local `nixpkgs` checkouts.

Now I can run other packages (similar to `nixpkgs` input) right from
`git` state. For example my typical `uselex` package is defined via git
snapshot:

```nix
# $ cat cat ~/overlays/uselex/default.nix
{ lib
, stdenv
, fetchFromGitHub

, ruby_3_1
, binutils-unwrapped-all-targets

, unstableGitUpdater
}:

stdenv.mkDerivation rec {
  pname = "uselex";
  version = "unstable-2022-08-29";

  src = fetchFromGitHub {
    owner = "trofi";
    repo = "uselex";
    rev = "5cf79a872f3331ce87171e66cf27c430585f65af";
    sha256 = "sha256-0aFJaGLcrrEkOH3cFs2uHjkCUw9ndckngfnb0J1FK7c=";
  };
# ... more stucff
}
```

I was able to define it's `live` version by passing an input explicitly:

```nix
# $ cat ~/overlays/flake.nix
{
  description = "trofi's overlay";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    uselex = {
      url = "github:trofi/uselex";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, uselex }@inputs: let
    np = import nixpkgs { system = "x86_64-linux"; };
  in {
    packages."x86_64-linux" = rec {
      uselex = np.callPackage ./uselex {};
      uselex_live = uselex.overrideAttrs (oa: {
        version = inputs.uselex.shortRev;
        src = inputs.uselex;
      }); }; };
}
```

Here I defined 2 packages:

- `uselex`: a package as it's defined in `uselex/default.nix`
- `uselex_live`: a package that uses `master` branch from `git` and
  reuses rest of `uselex` definition.

Now I can build both as:

```
$ cd ~/overlays
$ nix build .#uselex .#uselex_live

$ nix flake info
warning: 'nix flake info' is a deprecated alias for 'nix flake metadata'
warning: Git tree '/home/slyfox/.config/nixpkgs' is dirty
Resolved URL:  git+file:///home/slyfox/overlays/nixpkgs
Locked URL:    git+file:///home/slyfox/overlays/nixpkgs
Description:   trofi's overlay
Path:          /nix/store/s2b6pagz9i55jq71jfp3ml9y2dyl0mlr-source
Last modified: 2023-01-26 22:07:27
Inputs:
├───nixpkgs: github:NixOS/nixpkgs/f69c8b761a683940edeed0c23da1a5b8bd50bed3
└───uselex: github:trofi/uselex/5cf79a872f3331ce87171e66cf27c430585f65af
```

Using `--override-input` flag we can redirect default target commit to
locally modified tree or any other commit:

```
# no overrides:
$ nix build .#uselex_live

$ ls -ld result result-1
result -> /nix/store/76qkbdna1y4adbkk9k2g7znw2v3yyr7s-uselex-5cf79a8

# redirect to the local tree:
$ nix build .#uselex_live --override-input uselex ~/dev/git/uselex

• Updated input 'uselex':
    'github:trofi/uselex/5cf79a872f3331ce87171e66cf27c430585f65af' (2022-08-29)
  → 'git+file:///home/slyfox/dev/git/uselex?ref=refs%2fheads%2fmaster&rev=5cf79a872f3331ce87171e66cf27c430585f65af' (2022-08-29)
$ ls -ld result result-1
result -> /nix/store/76qkbdna1y4adbkk9k2g7znw2v3yyr7s-uselex-5cf79a8

# redirect to the arbitrary commit or branch:
$ nix build .#uselex_live --override-input uselex github:trofi/uselex/fe54bc12013a2a28f1638bdd5faa2f81d4d8fd1c

• Updated input 'uselex':
    'github:trofi/uselex/5cf79a872f3331ce87171e66cf27c430585f65af' (2022-08-29)
  → 'github:trofi/uselex/fe54bc12013a2a28f1638bdd5faa2f81d4d8fd1c' (2017-07-28)
$ ls -ld result result-1
result -> /nix/store/82maym4hks1nfcprharxwrpvf7ck6hz4-uselex-fe54bc1
```

Note how input reports new commit it switches to and encodes it into
package name (as we requested it with `version = inputs.uselex.shortRev;`).

One can even add a short alias for an arbitrary flake repository:

```
$ nix registry add ul github:trofi/uselex-flake
$ nix run ul#uselex_live --no-write-lock-file
...
 == SYNOPSIS (uselex-0.0.1)

      uselex.rb - look for USEless EXports in object files

$ nix registry remove ul
```

And for completeness here is how `flake.lock` looks like:

```
{
  "nodes": {
    "nixpkgs": {
      "locked": {
        "lastModified": 1675628371,
        "narHash": "sha256-KsAGKX6R5OZ4mvX0v9I8rXoQD62NG8bNq2vDh731fUk=",
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "461ef24010bec9df46f9f982e27441d83a856563",
        "type": "github"
      },
      "original": {
        "owner": "NixOS",
        "repo": "nixpkgs",
        "type": "github"
      }
    },
    "root": {
      "inputs": {
        "nixpkgs": "nixpkgs",
        "uselex": "uselex"
      }
    },
    "uselex": {
      "flake": false,
      "locked": {
        "lastModified": 1661761258,
        "narHash": "sha256-0aFJaGLcrrEkOH3cFs2uHjkCUw9ndckngfnb0J1FK7c=",
        "owner": "trofi",
        "repo": "uselex",
        "rev": "5cf79a872f3331ce87171e66cf27c430585f65af",
        "type": "github"
      },
      "original": {
        "owner": "trofi",
        "repo": "uselex",
        "type": "github"
      }
    }
  },
  "root": "root",
  "version": 7
}
```

## Parting words

You don't have to start from scratch if you are migrating from a typical
`/etc/nixos/configuration.nix` to `flake.nix`.

While slightly verbose `flakes` are not too scary as a concept. They
allow one to encode (and persist) all external inputs into `/nix/store`
and optionally write it into the `flake.lock` to be able to restore
exact build environment when needed.

`flakes` still provide flexibility of switching the revisions back and
forth for individual inputs for test purposes.

Have fun!

