---
title: "nix-build in tmpfs"
date: August 22, 2025
---

I build a lot of `nix` packages locally. Until `nix-2.30` release
`nix-build` command triggered builds in `/tmp` directory by default.
As it's not a `tmpfs` by default I used to enable
`boot.tmp.useTmpfs = true;` to force all of `/tmp` into `tmpfs` to get
slightly faster builds.

In `nix-2.30` `nix` [switched](https://discourse.nixos.org/t/nix-2-30-0-released/66449)
its default build directory to `/nix/var/nix/builds`:

```
... `build-dir` no longer defaults to `$TMPDIR` ...
```

This made my builds slow again. It's especially noticeable when a few
huge tarballs start unpacking on disk in parallel. Here is my new
workaround to get that directory to `tmpfs` as well:

```nix
# cat /etc/nixos/tmpfs.nix
{ lib, ... }:
{
  systemd.mounts = [{
    wantedBy = [ "nix-daemon.service" ];
    what = "tmpfs";
    where = "/nix/var/nix/builds";
    type = "tmpfs";
    mountConfig.Options = lib.concatStringsSep "," [
      "mode=0755"
      "strictatime"
      "rw"
      "nosuid"
      "nodev"
      "size=100G" # WARNING: you might want to change this value
    ];
  }];
}
```

It creates a `systemd` `mount` unit and makes it a pre-requisite of
`nix-daemon` and mounts just before `nix-daemon` start.

Have fun!
