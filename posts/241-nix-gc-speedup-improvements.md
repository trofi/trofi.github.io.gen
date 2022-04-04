---
title: "nix gc speedup improvements"
date: April 04, 2022
---

I wondered the other day: why my **nix store gc** takes an hour to run
on a system with a few terabytes of garbage packages to clean up. Is it
a typical time or could something be optimised to make it slightly
faster?

I ran **top** while **nix store gc** was churning and noticed it was a
CPU-bound task. That was unusual.

# Background

**nix** package manager never changes files inplace when it installs new
packages into **/nix/store**. This makes installation and uninstallation
operations cheap: no need for **sync()**-style calls or file locks.

Single package installation could be viewed as 2 steps:

1. unpack new package to **/nix/store/...-package-version** (if it does
   not already exist)
2. refer to unpacked files (usually via symlinks, via
   scripts, configs or embedded binary)

Uninstallation is just one step of undoing "[2.]":

1. stop referring to the package (by creating a new closure of needed packages)

This procedure does not require any file or directory removal.
On **NixOS** it's a matter of updating a single **/run/current-system**
symlink.

As a result over normal lifetime the system accumulates unreferenced
packages as files and directories in **/nix/store/**. If you have enough
storage you can completely ignore these unreferenced files.

But sometimes it's useful to delete unreferenced data to free up some
space or to check that there are no missing references in current package
closure. Such cleaning is done with **nix-collect-garbage**
(or **nix store gc**). Or their lower level sibling
**nix-store \-\-delete ...** (or **nix store delete ...**).

# Actual GC speed

All the above sounds nice, but how fast that garbage collection is in
practice? Should it take 1 second, 1 minute or 1 hour on an average
system? On a heavily cluttered system?

Let's find out! I have 2 systems:

1. **i7** host: **HDD**-based 10+ years old web server with daily updates
   which never saw a garbage collection run. It's whole **/nix/store**
   is 26GB.

2. **nz** host: **NVMe**-based 1 year old desktop where I build A Lot of
   packages daily and run garbage collection once a month. It's whole
   **/nix/store** is **380GB**.

**i7** dry run:

```
$ time nix store gc --dry-run

real 3m18.522s
...
```

~200 seconds. Is it a lot? The process touches no disk. According to
**top** it's a CPU-bound problem. What does it do? Runnig
**perf top -p $pid** shows the following:

```
$ sudo perf top -p 6217
<wait 5 seconds>
   6.82%  libstdc++.so.6.0.28  [.] std::istream::get
   6.12%  libc-2.33.so         [.] __memcmp_sse4_1
   4.51%  libcrypto.so.1.1     [.] sha256_block_data_order_avx
   4.18%  libstdc++.so.6.0.28  [.] std::istream::sentry::sentry
   3.91%  libnixstore.so       [.] nix::parseString
   2.73%  libc-2.33.so         [.] malloc
   2.55%  libsqlite3.so.0.8.6  [.] sqlite3VdbeExec
   2.31%  [kernel]             [k] syscall_exit_to_user_mode
```

Looks like some string parsing and **sqlite3** reading. Could be
optimised a bit around that strange **std::istream::get** but
otherwise looks reasonable. Not exactly an interactive latency,
but not too bad if ran infrequently. Maybe there is just a lot
to do in this case?

**nz** run:

```
$ time nix store gc --dry-run

real    28m24,295s
...
```

~1800 seconds. Almost half an hour! That is already way outside
interactive use of the tool. But maybe it's fine for a system
with large amount of clutter? Let's find out!

Checking for profile picture:

```
$ sudo perf top -p 2531652
<wait 5 seconds>
  10,15%  libstdc++.so.6.0.28  [.] std::istream::get
   5,42%  libstdc++.so.6.0.28  [.] std::istream::sentry::sentry
   5,16%  libc-2.33.so         [.] __memcmp_avx2_movbe
   4,14%  [kernel]             [k] copy_user_enhanced_fast_string
   3,90%  libc-2.33.so         [.] _int_malloc
   3,03%  libc-2.33.so         [.] malloc
   2,38%  libnixstore.so       [.] nix::parseString
   2,30%  libsqlite3.so.0.8.6  [.] sqlite3VdbeExec
```

The profile is very close to **i7** one: same **std::istream::get**
at the top and a **memcmp()** implementation that follows.

# The first attempt

Profile clearly tells us that **istream** related parsing is the
bottleneck.

Quick quiz: does it?

The parser is supposed to extract **/nix/store** references out of
**.drv** files which usually look like the example below:

```
$ nix show-derivation /nix/store/6xcvz9zp757knf005g4q1p451p6wslpk-ninja-1.10.2.drv

{
  "/nix/store/6xcvz9zp757knf005g4q1p451p6wslpk-ninja-1.10.2.drv": {
    "outputs": {
      "out": {
        "hashAlgo": "r:sha256"
      }
    },
    "inputSrcs": [
      "/nix/store/27axb57ya2ddmaa9m5fv700ww65z5hcd-docbook-xml-4.5",
      "/nix/store/2wald1adsn6bxzmvypzjk5z2zk9dlgwh-stdenv-linux",
      "/nix/store/34v3hflhcklq44jigh2banjwkcyqjc5k-python3-3.9.9",
      "/nix/store/7z1wzf51i7jxi9nl2dg3yqmsfb70s4fm-docbook-xsl-nons-1.79.2",
      "/nix/store/8m2rhn4jgz0bj52m7fx53jbj9qiswy6q-re2c-2.2",
      "/nix/store/8x4aqaqaiyylvcpnhn1phypkb4n6ady0-source",
      "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh",
      "/nix/store/mcm0l38vgh9l24dgl94gj4hjvxp91g8j-libxslt-1.1.34-bin",
      "/nix/store/v3rml3r7liky77iwipjg9bxmm874ra6g-bash-5.1-p12",
      "/nix/store/y9yz15h8mlpggmr3jidjdhjpddsx7d25-setup-hook.sh",
      "/nix/store/yyaxnsqm849wwi901m6g7nbi1hqy7vsp-asciidoc-9.1.0"
    ],
    "inputDrvs": {},
    "system": "x86_64-linux",
    "builder": "/nix/store/v3rml3r7liky77iwipjg9bxmm874ra6g-bash-5.1-p12/bin/bash",
    "args": [
      "-e",
      "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"
    ],
    "env": {
      "buildInputs": "",
      "buildPhase": "python configure.py --bootstrap\n# \"./ninja -vn manual\" output copied here to support cross compilation.\nasciidoc -b docbook -d book -o build/manual.xml doc/manual.asciidoc\nxsltproc --nonet doc/docbook.xsl build/manual.xml > doc/manual.html\n",
      "builder": "/nix/store/v3rml3r7liky77iwipjg9bxmm874ra6g-bash-5.1-p12/bin/bash",
...
```

Here **inputDrvs**, **inputSrcs** and some **env** references are
interesting edges for garbage collector. But some are possibly not
(like build-only **env.builder**).

The parsing format looked trivial and I tried a quick hack to
substitute **istream** for a manual parser in <https://github.com/NixOS/nix/pull/6266>.

This gave a 25% speedup (7 minutes faster on **nz**). Sounds like a
lot for a simple 80-line change.

But Eelco did not see any speedup improvement on his system. Moreover
Eelso's GC times were ridiculously short: 17 seconds instead of minutes.

That made me think: what is parser's parse speed if I see so large
an improvement? My system has to parse A Lot of **.drv** files to make
it measurable.

To put the example into some abstract numbers:
if parse speed is even as low as 100MB/s then running it for 7 minutes
would be able to parse ~40GB of **.drv** files. I don't have that many.
All my **/nix/store/\*.drv** files are ~1GB (~270K files).

Does it mean my system parses the same files multiple times? Is my system
somehow special to have many of them?

Running **strace -f** against the **nix-daemon** confirmed that some of
**.drv** files were opened and read repeatedly. That's strange.

# The second attempt

My naive understanding of graph traversal for garbage collection purposes
told me that each node should be traversed once.

I added a few **debug()** calls to
[src/libstore/gc.cc](https://github.com/NixOS/nix/blob/master/src/libstore/gc.cc)
around repeated **.drv** visits and found ... a bug!

For some **nix.conf** setups (like the ones with
**keep-derivations = true**) **.drv** files are themselves considered
(implicit) referrers. They should be retained on garbage
collection (live paths). By keeping related **.drv** files around
we keep possible prerequisites in case we want to rebuild a derivation.
Which is handy for package development.

The bug was in treatment of the **.drv** files: they were correctly traversed
as referrers, but they were not added to **alive** set of visited nodes.
As a result **.drv** file was visited every time some path pulled a **.drv** in.

Thus the fix was a two-liner: <https://github.com/NixOS/nix/commit/d58453f72ea584cac2e3362fd6a73fcf0e3b615e>

```diff
--- a/src/libstore/gc.cc
+++ b/src/libstore/gc.cc
@@ -678,7 +678,8 @@ void LocalStore::collectGarbage(const GCOptions & options, GCResults & results)
                 alive.insert(start);
                 try {
                     StorePathSet closure;
-                    computeFSClosure(*path, closure);
+                    computeFSClosure(*path, closure,
+                        /* flipDirection */ false, gcKeepOutputs, gcKeepDerivations);
                     for (auto & p : closure)
                         alive.insert(p);
                 } catch (InvalidPath &) { }
```

Before the change **computeFSClosure()** was called with implicit default
**gcKeepDerivations = false** argument. After the change it started honoring
user's configuration.

The fun thing is that **keep-derivations = true** is a default enabled option!
Unless you switched it off explicitly you probably have it enabled on **NixOS**.

# Benchmarks

Let's benchmark this change on the same setup as above. I plugged the patch
into **configuration.nix** as:

```nix
  nixpkgs.overlays = [
    (final: prev: { nix = prev.nix.overrideAttrs (oa: {
      patches = (oa.patches or []) ++ [ (prev.fetchpatch ({
        name = "fix-gc-drv-re-parse.patch";
        url = "https://github.com/NixOS/nix/commit/d58453f72ea584cac2e3362fd6a73fcf0e3b615e.patch";
        hash = "sha256-QiBFJVRWNTXyVFch1zoNJL3ZfF4ZD9JKNlID4v+eUVc=";
      })) ];
    });})
  ];
```

**i7**:

```
$ time nix store gc --dry-run

real    0m7.403s
...
```

7 seconds compared to previous 200 seconds. ~29x speedup.

**nz**:


```
$ time nix store gc --dry-run

real    1m0,140s
...
```

60 seconds compared to previous 1800 seonds. ~30x speedup as well.

# Full run

Time to run the actual garbage collection that includes file removal
from disk and path unregistration from database:

**i7**:

```
# dry run
$ time nix store gc --dry-run

real    0m7.403s
...

# full run
$ time nix store gc

2250 store paths deleted, 18751.92 MiB freed

real    0m32.695s
...

# full re-run
$ time nix store gc

0 store paths deleted, 0.00 MiB freed

real    0m7.708s
...
```

Note that actual package removal is 5 times slower than
GC dry run. This ratio depends a lot on how many files are
to delete, what filesystem and what mode you are using.
But the gist of it is that the difference is not that big
compared to dry run.

Dry-run before and after garbage collection did not change
much. This means we did not delete that many packages
compared to alive set of packages. After the cleanup
**/nix/store** is only 2.3GB with 2300 store paths.

Thus we deleted about the half the store paths and about
90% of content size.

The speed of removal is about 70 store paths (packages)
per second.

**nz**:

```
# dry run
$ time nix store gc --dry-run

real    1m0,140s
...

# full run
$ time nix store gc

319698 store paths deleted, 332762.52 MiB freed

real    5m54,990s
...

# full re-run
$ time nix store gc

0 store paths deleted, 0.00 MiB freed

real    0m4,528s
...
```

Again, actual file deletion is only 6 times slower than GC dry run.
Alive set after GC is 15K packages with 34GB storage.

The speed of removal is about ~800 store paths (packages)
per second.

How many packages per second can your distribution delete?
Does it scale well with amount of packages already installed in system?

# Parting words

Garbage collection should take no more than one minute :)

It was surprisingly easy to get ~30x speedup of garbage collector with
a two-liner patch. The improvement will help systems with default
**nix.conf** configuration. Upcoming **nix-2.8** will contain the
improvement.

Initially I was fooled by **perf top** output and optimised the parser
to get 25% speedup. The real bug was in tracking visited nodes.

Have fun!
