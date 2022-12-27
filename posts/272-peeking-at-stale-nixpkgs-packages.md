---
title: "peeking at stale nixpkgs packages"
date: December 27, 2022
---

As a `nixpkgs` user I want it to help me solve my and others' problems.
The upstream package bugs are frequently already fixed upstream and we
only need to update to get `nixpkgs` into better state.

How many outdated packages does my system have? Can I just list them all
and maybe work on one or two to update them?

## On repology

Probably the most popular package info database is
<https://repology.org/>. It contains package details across various
package repositories. `nixpkgs` is not an exception.

Let's have a look at example `re2c` package.
`repology` [page](https://repology.org/project/re2c/versions) shows us a
few things:

- `3.0` is the latest release available
- `nixpkgs-unstable` (`master` branch), `22.05` and `21.11`
  releases do provide latest `3.0`.
- `nixpkgs-stable-21.11` (2 releases back) provides older `2.2` version
- `Debian unstable` and `12` release also provide latest `3.0`.
- `Fedora rawhide` provides very old `2.1.1` release (whoops!)

`repology.org` API [provides](https://repology.org/api) a `json`
table we can fetch and inspect directly.

Joining this data against the installed system should yield something
useful. Let's see how hard it is.

To get data related to `matser` branch of `nixpkgs` we can filter on
`nix_unstable` repo with `inrepo=nix_unstable` parameter:

```
$ curl --compressed -s \
  "https://repology.org/api/v1/projects/?inrepo=nix_unstable&outdated=1" | jq

{
...
  "a52dec": [
    {
      "repo": "adelie_current",
      "subrepo": "user",
      "srcname": "a52dec",
      "binname": "a52dec-dev",
      "visiblename": "a52dec-dev",
      "version": "0.8.0",
      "maintainers": [
        "me@zv.io"
      ],
      "licenses": [
        "GPL-2.0+"
      ],
      "summary": "Library for decoding ATSC A/52 streams (development files)",
      "status": "newest",
      "origversion": "0.8.0-r0"
    },
    ...
    {
      "repo": "nix_unstable",
      "name": "a52dec",
      "visiblename": "a52dec",
      "version": "0.7.4",
      "maintainers": [
        "fallback-mnt-nix@repology"
      ],
      "licenses": [
        "GPL-2.0-or-later"
      ],
      "summary": "ATSC A/52 stream decoder",
      "status": "outdated",
      "origversion": null
    },
  ...
  "azure-cli": [
    {
      "repo": "scoop",
      "subrepo": "main",
      "binname": "azure-cli",
      "visiblename": "azure-cli",
      "version": "2.43.0",
      "licenses": [
        "MIT"
      ],
      "status": "newest",
      "origversion": null
    },
  ...
    }
  ]
}
```

I piped the output through `jq` to make it slightly more readable.

We see a lot here:

- `repology`'s **project name** comes as a key here

- **values** are arrays of per-repository details for package status:
  repository name, package name, version, version status and so on.

- **status** field tells us outright if the package is stale or not.

- data is paginated: only the range from `"a52dec"` to `"azure-cli"` is
  covered.

To get more data we can call the same API by passing the project key
as part of `projects/` path to continue from there:

```
# page 1:
$ curl --compressed -s \
  "https://repology.org/api/v1/projects/?inrepo=nix_unstable&outdated=1" >p1
# page2
$ curl --compressed -s \
  "https://repology.org/api/v1/projects/azure-cli/?inrepo=nix_unstable&outdated=1" >p2

$ jq --sort-keys --raw-output 'keys|last' <p1
azure-cli
$ jq --sort-keys --raw-output 'keys|last' <p2
cli11
```

Now we can build the list of outdated packages in `nixpkgs`. I'll use
the following `jq` hack to pick latest-everywhere vs latest-in-nixpkgs:

```
$ jq --sort-keys '
map_values({
  "newest": map(
    select(.status|in({"newest":1}))
  )|first(.[].version),
  "nix_unstable_version": map(
    select(.repo|in({"nix_unstable":1}))
  )|first(.[].version),
  "nix_name": map(
    select(.repo|in({"nix_unstable":1}))
  )|first(.[].name),
})' < p1 | jq --sort-keys '
  map_values(
    "\(.nix_name): \(.nix_unstable_version) -> \(.newest)"
  )
'

{
  "1password-cli": "1password-cli: 2.11.0 -> 2.12.0",
  "389-ds-base": "389-ds-base: 2.3.0 -> 2.3.1",
  "7kaa": "7kaa: 2.15.4p1 -> 2.15.5",
  "a52dec": "a52dec: 0.7.4 -> 0.8.0",
  "abuse": "abuse: 0.8 -> 0.9.1",
  "ace-framework": "ace: 7.0.8 -> 7.0.11",
  "acorn": "acorn: 0.4.2 -> 8.8.1",
  "acpica": "acpica-tools: 20220331 -> 20221022",
  "acpitool": "acpitool: 0.5.1 -> 0.5.2",
  "actor-framework": "actor-framework: 0.18.5 -> 0.18.6",
  ...
  "azure-cli": "azure-cli: 2.37.0 -> 2.43.0"
}
```

You should be able to come up with a more reasonable query.

The above quary is not distribution-specific: you can swap
`nix_unstalble` for your distro of choice to fish for things you care
about.

Or you can use web UI to skim through the same data:
<https://repology.org/projects/?inrepo=nix_unstable&outdated=1>.

Web UI is not very handy to grep through as it takes multiple pages.

## On derivations

The above hack gives us the whole list of stale packages in `nixpkgs`.
I would still still like to narrow it down to set of packages relevant
to my system.

Luckily the whole `NixOS` system is normally described by a single build
"rule" (a single `derivation`). By inspecting that we can find all the
used packages:

```
$ nix show-derivation --derivation -r $(nix-instantiate '<nixpkgs/nixos>' -A system)

{
...
  "/nix/store/1vb6bjnkrwyj94d87ps1z9wa3i1fzia8-re2c-3.0.drv": {
    "args": [
      "-e",
      "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
    ],
    "builder": "/nix/store/4xw8n979xpivdc46a9ndcvyhwgif00hz-bash-5.1-p16/bin/bash",
    "env": {
      "buildInputs": "",
      "builder": "/nix/store/4xw8n979xpivdc46a9ndcvyhwgif00hz-bash-5.1-p16/bin/bash",
      "cmakeFlags": "",
      "configureFlags": "",
      "depsBuildBuild": "",
      "depsBuildBuildPropagated": "",
      "depsBuildTarget": "",
      "depsBuildTargetPropagated": "",
      "depsHostHost": "",
      "depsHostHostPropagated": "",
      "depsTargetTarget": "",
      "depsTargetTargetPropagated": "",
      "doCheck": "1",
      "doInstallCheck": "",
      "enableParallelBuilding": "1",
      "enableParallelChecking": "1",
      "mesonFlags": "",
      "name": "re2c-3.0",
      "nativeBuildInputs": "/nix/store/nxxd4bhq41ix50n532vkdx6vp5p5hir3-hook /nix/store/al6g1zbk8li6p8mcyp0h60d08jaahf8c-python3-3.10.9",
      "out": "/nix/store/5mf1k9jy94ji20xcy77z58qaw7w4izrp-re2c-3.0",
      "outputs": "out",
      "patches": "",
      "pname": "re2c",
      "preCheck": "patchShebangs run_tests.py\n",
      "propagatedBuildInputs": "",
      "propagatedNativeBuildInputs": "",
      "src": "/nix/store/sl98y4sk9vzxinydlbc9nyzlqr5az8sj-source",
      "stdenv": "/nix/store/cp65c8nk29qq5cl1wyy5qyw103cwmax7-stdenv-linux",
      "strictDeps": "",
      "system": "x86_64-linux",
      "version": "3.0"
    },
    "inputDrvs": {
      "/nix/store/6z1jfnqqgyqr221zgbpm30v91yfj3r45-bash-5.1-p16.drv": [
        "out"
      ],
      "/nix/store/7k290ai5pfv6zw25ymank65dks86g64h-source.drv": [
        "out"
      ],
      "/nix/store/ap9g09fxbicj836zm88d56dn3ff4clxl-stdenv-linux.drv": [
        "out"
      ],
      "/nix/store/b2p151ilwqpd47fbmzz50a5cmj12ixbf-hook.drv": [
        "out"
      ],
      "/nix/store/vgpv5w9lxnrxkdvb4hx1llxp811fd8pk-python3-3.10.9.drv": [
        "out"
      ]
    },
    "inputSrcs": [
      "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
    ],
    "outputs": {
      "out": {
        "path": "/nix/store/5mf1k9jy94ji20xcy77z58qaw7w4izrp-re2c-3.0"
      }
    },
    "system": "x86_64-linux"
  },
...
```

Here one of numerous derivations used to produce final `system`
derivation is `re2c`. Most derivations have `pname` and `version` in
their environemnts. Thus the simplest hack would be to extract them with
`jq` again:

```
$ nix show-derivation --derivation -r $(nix-instantiate '<nixpkgs/nixos>' -A system) |
    jq -r '.[] | "\(.env.pname) \(.env.version)"' | fgrep -v "null" | sort -u

a52dec 0.7.4
aalib 1.4rc5
abseil-cpp 20210324.2
accounts-qt 1.16
...
re2c 3.0
```

## Collecting list of outdated packages

Now we have everything to match our system against the stale list. I
used simple `grep` for that. Full list of used scripts is at
<https://discourse.nixos.org/t/a-tool-for-looking-if-installed-packages-are-up-to-date/21630/6>.

The typical output looks like that:

```
$ ./print_local_outdated.bash | nl
     1  # installed(staging): unstable(master) -> latest(others)
     2  a52dec-0.7.4:   "a52dec": "a52dec: 0.7.4 -> 0.8.0",
     3  afdko-3.9.0:   "afdko": "python3.9-afdko: 3.9.0 -> 3.9.1",
     4  alsa-lib-1.2.7.2:   "alsa-lib": "alsa-lib: 1.2.7.2 -> 1.2.8",
     5  alsa-ucm-conf-1.2.7.1:   "alsa-ucm-conf": "alsa-ucm-conf: 1.2.7.1 -> 1.2.8",
     6  appstream-0.15.5:   "appstream": "appstream-qt: 0.15.5 -> 0.15.6",
...
   223  xhost-1.0.8:   "xhost": "xhost: 1.0.8 -> 1.0.9",
   224  xkbcomp-1.4.5:   "xkbcomp": "xkbcomp: 1.4.5 -> 1.4.6",
   225  xkeyboard-config-2.33:   "xkeyboard-config": "xkeyboard-config: 2.33 -> 2.37",
   226  xlsclients-1.1.4:   "xlsclients": "xlsclients: 1.1.4 -> 1.1.5",
   227  xmlrpc-c-1.51.07:   "xmlrpc-c": "xmlrpc-c: 1.51.07 -> 1.51.08",
   228  xorgproto-2021.5:   "xorgproto": "xorgproto: 2021.5 -> 2022.2",
   229  xorg-server-1.20.14:   "xorg-server": "xorg-server: 1.20.14 -> 21.1.6",
   230  xprop-1.2.5:   "xprop": "xprop: 1.2.5 -> 1.2.6",
   231  xrandr-1.5.1:   "xrandr": "xrandr: 1.5.1 -> 1.5.2",
   232  xset-1.2.4:   "xset": "xset: 1.2.4 -> 1.2.5",
   233  xsetroot-1.1.2:   "xsetroot": "xsetroot: 1.1.2 -> 1.1.3",
```

My system has at least 232 outdated packages (of 1505 detected). That is
a lot. I'm slowly going through them one by one and check why automated
upgrades do not work for them. One of the obvious examples here is `xorg`
related packages.

## Next steps

The source data that `repology` and package derivations provide
are enough to tie freshness together. My `jq` hacks don't handle
corner cases well. Some obvious deficiencies are:

- packages intentionally kept as multiple versions
- clearly wrong package versions reported by repology
- something else?

I'll attemt to write more robust tool with friendlier UI available for
daily use.

## Parting words

Package version information exposed by `repology.org` is great. It does
not require much of data processing to get basic details of how fresh
the package version you are looking at is.

`nixpkgs` has quite a few stale packages worth updating. If you are
thinking of contributing to `nixpkgs` then list such as this might be
a good inspiration.

Have fun!
