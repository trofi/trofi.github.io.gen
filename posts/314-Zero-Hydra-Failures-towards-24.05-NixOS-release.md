---
title: "Zero Hydra Failures towards 24.05 NixOS release"
date: May 25, 2024
root: "http://trofi.github.io"
---

I somehow missed the beginning of `ZHF` phase
[this release cycle](https://github.com/NixOS/nixpkgs/issues/309482).

For those who don't know `ZHF` (or Zero Hydra Failures) is the time when
most build failures are squashed before final `NixOS-24.05` release
(see [full release schedule](https://github.com/NixOS/nixpkgs/issues/303285)).

To follow the tradition let's fix one bug for `ZHF`.

I picked [`miniupnpc`](https://hydra.nixos.org/build/261188699) build
failure. Surprisingly it blocks about 60 packages!

The failure looks trivial:

```
trying https://miniupnp.tuxfamily.org/files/miniupnpc-2.2.7.tar.gz
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
  0     0    0     0    0     0      0      0 --:--:--  0:02:14 --:--:--     0
curl: (28) Failed to connect to miniupnp.tuxfamily.org port 443 after 134794 ms: Couldn't connect to server
Warning: Problem : timeout. Will retry in 1 seconds. 3 retries left.
  0     0    0     0    0     0      0      0 --:--:--  0:02:14 --:--:--     0
curl: (28) Failed to connect to miniupnp.tuxfamily.org port 443 after 134159 ms: Couldn't connect to server
Warning: Problem : timeout. Will retry in 2 seconds. 2 retries left.
  0     0    0     0    0     0      0      0 --:--:--  0:02:13 --:--:--     0
curl: (28) Failed to connect to miniupnp.tuxfamily.org port 443 after 133158 ms: Couldn't connect to server
Warning: Problem : timeout. Will retry in 4 seconds. 1 retries left.
  0     0    0     0    0     0      0      0 --:--:--  0:02:15 --:--:--     0
curl: (28) Failed to connect to miniupnp.tuxfamily.org port 443 after 135253 ms: Couldn't connect to server
error: cannot download miniupnpc-2.2.7.tar.gz from any mirror
```

Upstream source is unavailable and `curl` times out fetching it.

With great suggestions from others to switch the package to `github`
source fetch I came up with [PR#314510](https://github.com/NixOS/nixpkgs/pull/314510):

```diff
--- a/pkgs/tools/networking/miniupnpc/default.nix
+++ b/pkgs/tools/networking/miniupnpc/default.nix
@@ -1,6 +1,6 @@
 { lib
 , stdenv
-, fetchurl
+, fetchFromGitHub
 , cmake
 }:

@@ -8,14 +8,15 @@ stdenv.mkDerivation rec {
   pname = "miniupnpc";
   version = "2.2.7";

-  src = fetchurl {
-    urls = [
-      "https://miniupnp.tuxfamily.org/files/${pname}-${version}.tar.gz"
-      "http://miniupnp.free.fr/files/${pname}-${version}.tar.gz"
-    ];
-    sha256 = "sha256-sMOicFaED9DskyilqbrD3F4OxtLoczNJz1d7CqHnCsE=";
+  src = fetchFromGitHub {
+    owner = "miniupnp";
+    repo = "miniupnp";
+    rev = "miniupnpc_${lib.replaceStrings ["."] ["_"] version}";
+    hash = "sha256-cIijY1NcdF169tibfB13845UT9ZoJ/CZ+XLES9ctWTY=";
   };

+  sourceRoot = "${src.name}/miniupnpc";
+
   nativeBuildInputs = [ cmake ];

   doCheck = !stdenv.isFreeBSD;
```

The fix is slightly larger than the average one-liner as we have to
fiddle with the source-fetching helper. But otherwise it's simple.

Testing:

```
$ nix build -f. miniupnpc
```

All good!

## Parting words

As `24.05` branch was already created the fix will have to be backported
to it by adding a specific label. One of the maintainers will have to do
it.

Otherwise contributing to `ZHF` is very easy. Give it a try!

Have fun!
