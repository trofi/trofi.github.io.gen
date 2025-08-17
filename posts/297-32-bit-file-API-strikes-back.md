---
title: "32-bit file API strikes back"
date: September 7, 2023
root: "http://trofi.github.io"
---

It was another day of me trying a new `gcc` version.

## The problem

This time build failure was in `gcc` itself:

```
$ nix build -f. pkgsi686Linux.stdenv
       > /nix/store/bxvqx767s4gwry9km5c3cmflskmparyf-bootstrap-stage-xgcc-stdenv-linux/setup: line 167: type: install_name_tool: not found
       > preFixupLibGccPhase
       > stat: Value too large for defined data type
       For full logs, run 'nix log /nix/store/v3cr2nghg1s4bmm30r1vnq1124qqvv9m-xgcc-14.0.0.drv'.
```

## Debugging the failure

The actual error here is `stat: Value too large for defined data type`.
While `type: install_name_tool: not found` is an unrelated distraction.
Note that this failure happens a bit earlier than `pkgsi686Linux.stdenv`
build itself. If we poke a bit around the action build failure happens
in `pkgsi686Linux.stdenv.__bootPackages.stdenv.__bootPackages.stdenv.__bootPackages.stdenv.cc.cc` package in early bootstrap phases.
From the error message failure happens in `preFixupLibGccPhase` phase.
Let's look at its definition:

```
$ nix repl '<nixpkgs>'
...
nix-repl> builtins.trace pkgsi686Linux.stdenv.__bootPackages.stdenv.__bootPackages.stdenv.__bootPackages.stdenv.cc.cc.preFixupLibGccPhase ""
trace: # move libgcc from lib to its own output (libgcc)
mkdir -p $libgcc/lib
mv    $lib/lib/libgcc_s.so      $libgcc/lib/
mv    $lib/lib/libgcc_s.so.1    $libgcc/lib/
ln -s $libgcc/lib/libgcc_s.so   $lib/lib/
ln -s $libgcc/lib/libgcc_s.so.1 $lib/lib/
patchelf --set-rpath "" $libgcc/lib/libgcc_s.so.1

""
```

One of these commands did fail, not clear which one. Let's add a bit of
debugging by adding `set -x` into the phase:

```
$ nix develop --impure --expr 'with import ./. {};
  pkgsi686Linux.stdenv.__bootPackages.stdenv.__bootPackages.stdenv.__bootPackages.stdenv.cc.cc.overrideAttrs (oa: {
    preFixupLibGccPhase = "set -x\n" + oa.preFixupLibGccPhase; })'
$$ genericBuild
...
++ patchelf --set-rpath '' /home/slyfox/dev/git/nixpkgs/outputs/libgcc/lib/libgcc_s.so.1
stat: Value too large for defined data type
```

Yay! It was the `patchelf` call! We can re-enter the environment and
poke a bit more at the environment:

```
$ nix develop --impure --expr 'with import ...'
$$ patchelf --set-rpath '' $libgcc/lib/libgcc_s.so.1
stat: Value too large for defined data type
```

The error is still there. What kind of `stat` call does `patchelf` use?

```
$ which patchelf
/nix/store/i9v173g8a5wwi8i8fd2wmdyr8ix6mla1-bootstrap-tools/bin/patchelf

$ nm -DC /nix/store/i9v173g8a5wwi8i8fd2wmdyr8ix6mla1-bootstrap-tools/bin/patchelf |& fgrep stat
         U __xstat@GLIBC_2.0
```

Note that this `patchelf` comes from `bootstrapTools`.
`pkgs/stdenv/linux/bootstrap-files/i686.nix` says it was updated last
time in `2019` (4 years ago).
For comparison currently built `patchelf` built on `i686` system does
use `stat64` call:

```
$ nm -DC $(nix-build --no-link '<nixpkgs>' -A patchelf --argstr system i686-linux )/bin/patchelf |& fgrep stat
         U stat64@GLIBC_2.33
```

And it runs the patch just fine:

```
$$ $(nix-build --no-link '<nixpkgs>' -A patchelf --argstr system i686-linux)/bin/patchelf  --set-rpath '' $libgcc/lib/libgcc_s.so.1
```

## Refreshing `bootstrapFiles`

The fix is as simple as regenerating `bootstrapFiles` for `i686`:

```
$ nix-build '<nixpkgs/pkgs/stdenv/linux/make-bootstrap-tools.nix>' -A bootstrapFiles      --arg pkgs 'import <nixpkgs> { system = "i686-linux"; }'
/nix/store/713cyy66gkxqmi1wpdswd4llq1qzikr5-bootstrap-tools.tar.xz
/nix/store/cvdfhnwjbbfjbv6ibgcl8rz47giy771v-busybox
```

I did not have to build anything. Hydra has it cached today.
We can point our seed binaries to freshly built version of those:

```diff
--- a/pkgs/stdenv/linux/bootstrap-files/i686.nix
+++ b/pkgs/stdenv/linux/bootstrap-files/i686.nix
@@ -1,12 +1,4 @@
 {
-  busybox = import <nix/fetchurl.nix> {
-    url = "http://tarballs.nixos.org/stdenv-linux/i686/4907fc9e8d0d82b28b3c56e3a478a2882f1d700f/busybox";
-    sha256 = "ef4c1be6c7ae57e4f654efd90ae2d2e204d6769364c46469fa9ff3761195cba1";
-    executable = true;
-  };
-
-  bootstrapTools = import <nix/fetchurl.nix> {
-    url = "http://tarballs.nixos.org/stdenv-linux/i686/c5aabb0d603e2c1ea05f5a93b3be82437f5ebf31/bootstrap-tools.tar.xz";
-    sha256 = "b9bf20315f8c5c0411679c5326084420b522046057a0850367c67d9514794f1c";
-  };
+  busybox = ./i686-linux/busybox;
+  bootstrapTools = ./i686-linux/bootstrap-tools.tar.xz;
 }
```

Now `pkgsi686Linux.stdenv` builds just fine:

```
$ nix build -f. pkgsi686Linux.stdenv
```

Unfortunately the change is not usable for upstream as is: uploading new
bootstrap binaries is a strange rarely exercised process that requires
privileged user to upload tarballs to `s3`. Filed
<https://github.com/NixOS/nixpkgs/issues/253274> to do it correctly.

I would say it's a waste of time to debug issues in outdated binaries
like that. The bootstrap tarballs should be updated at least every
`NixOS` release (every 6 months). Or more frequently :) Filed
<https://github.com/NixOS/nixpkgs/issues/253713> for that.

Periodic updates would also make tarballs more homogeneous across
architectures. Today we ship different `glibc` and `gcc` versions in
bootstrap tarballs which adds another dimension of bugs.

## Why did upgrade work?

`patchelf` itself was fixed in 2016 (7 years ago) as
<https://github.com/NixOS/patchelf/commit/a4d21661d510ccf7ff72bb0e4ccd3f087e9086ad>:

```diff
--- a/src/Makefile.am
+++ b/src/Makefile.am
@@ -1,4 +1,4 @@
-AM_CXXFLAGS = -Wall -std=c++11
+AM_CXXFLAGS = -Wall -std=c++11 -D_FILE_OFFSET_BITS=64
```

We just happened to pull in a fix for it along with `glibc` that
supports `stat64`.

## How should fix usually looks like?

Setting `-D_FILE_OFFSET_BITS=64` explicitly should be a safe workaround.
`autoconf`-based systems usually use
[`AC_SYS_LARGEFILE`](https://www.gnu.org/software/autoconf/manual/autoconf-2.67/html_node/System-Services.html).
It should set both `_FILE_OFFSET_BITS` and `_LARGE_FILES` where needed
and provides a nice `--disable-largefile` knob. Other build systems
have the equivalent or always enable it by default.

`glibc` implements `stat` switch in `io/sys/stat.h` as:

```c
#ifndef __USE_FILE_OFFSET64
/* Get file attributes for FILE and put them in BUF.  */
extern int stat (const char *__restrict __file,
                 struct stat *__restrict __buf) __THROW __nonnull ((1, 2));
#else
# ifdef __USE_TIME_BITS64
extern int __REDIRECT_NTH (stat, (const char *__restrict __file,
                                  struct stat *__restrict __buf),
                                  __stat64_time64)
# endif
#else
extern int stat64 (const char *__restrict __file,
                   struct stat64 *__restrict __buf) __THROW __nonnull ((1, 2));
#endif
```

(I skipped a bit of `#define` boilerplate where `stat` gets redirected to
`__xstat`.)
The above hints that we will soon have a similar problem of switching
to 64-bit `time_t` on 32-bit systems.

## Why did `patchelf` fail at all?

I hear you ask: "why did `patchelf` fail at all"? Is `libgcc.so` such a
large file by any definition? Its size is unlikely to overflow 32 bits
(4GB). Why does `stat()` implementation matter here?
And you are right: `libgcc_s.so.1` is only `139KB` large.

Here is the full structure `man 2 stat` knows about:

```c
struct stat {
    dev_t     st_dev;         /* ID of device containing file */
    ino_t     st_ino;         /* Inode number */
    mode_t    st_mode;        /* File type and mode */
    nlink_t   st_nlink;       /* Number of hard links */
    uid_t     st_uid;         /* User ID of owner */
    gid_t     st_gid;         /* Group ID of owner */
    dev_t     st_rdev;        /* Device ID (if special file) */
    off_t     st_size;        /* Total size, in bytes */
    blksize_t st_blksize;     /* Block size for filesystem I/O */
    blkcnt_t  st_blocks;      /* Number of 512B blocks allocated */

    /* Since Linux 2.6, the kernel supports nanosecond
       precision for the following timestamp fields.
       For the details before Linux 2.6, see NOTES. */

    struct timespec st_atim;  /* Time of last access */
    struct timespec st_mtim;  /* Time of last modification */
    struct timespec st_ctim;  /* Time of last status change */

#define st_atime st_atim.tv_sec      /* Backward compatibility */
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec
};
```

`stat()` has to fill all the fields. It does not known which fields
user space is going to need. The man page also tells us the overflowing
condition:

```
ERRORS
...
   EOVERFLOW
          pathname  or fd refers to a file whose size, inode number,
          or number of blocks cannot be represented in, respectively,
          the types off_t, ino_t, or blkcnt_t.  This error can occur
          when, for example, an application compiled on a 32-bit
          platform without -D_FILE_OFFSET_BITS=64 calls stat() on a
          file whose size exceeds (1<<31)-1 bytes.
```

Note that it's 2GB limit and not a 4GB limit. And it is not just about
the file size. In my case it's the `inode` number `ino_t st_ino;` field:

```
$ ls -li foo
4404087433 -rw-r--r-- 1 slyfox users 0 Sep  7 09:25 foo
```

Here `inode` number overflows our 2GB limit. Let's use this trivial
program to make sure it fails to `stat()`:

```c
#include <errno.h>
#include <stdio.h>
#include <string.h>

#include <sys/stat.h>

int main() {
    struct stat s;
    int r;

    errno = 0;
    r = stat("foo", &s);

    if (r == -1)
        printf ("stat() = -1: error: %s\n", strerror(errno));
    else
        printf("stat() succeeded\n");
}
```

Running it against both APIs:

```
$ nix develop -f '<nixpkgs>' patchelf --argstr system i686-linux

$$ gcc a.c -o a && ./a
stat() = -1: error: Value too large for defined data type

$$ gcc a.c -o a -D_FILE_OFFSET_BITS=64 && ./a
stat() succeeded
```

Yep! This is it.

## Why are my `inode` numbers so big?

4 billion `inodes` is a lot. Why such a big number? Do I have so many
files on disk? No, `find /` tells me that I have around 25 million files
(~100 times smaller than 2 billion mark).
It comes down to the fact how exactly `btrfs` filesystem allocates `inode`
numbers.
Compared to `ext4` (which uses first available `inode` number in `inode`
table of fixed size) `btrfs` does not use a single `inode` table but uses
B-tree of "objects".
`btrfs` strategy to allocate `inodes` is to increment the global counter
(per filesystem, number is populated in `btrfs_create_new_inode()`):

```c
int btrfs_create_new_inode(struct btrfs_trans_handle *trans,
                           struct btrfs_new_inode_args *args)
{
    // ...
    int ret;
    ret = btrfs_get_free_objectid(root, &objectid);
    if (ret)
        goto out;
    inode->i_ino = objectid;
    // ...
    return ret;
}
// ...
int btrfs_get_free_objectid(struct btrfs_root *root, u64 *objectid)
{
    int ret;
    // skipped locking and error handling
    *objectid = root->free_objectid++;
    ret = 0;
    return re;
}
// ...
#define BTRFS_FIRST_FREE_OBJECTID 256ULL
#define BTRFS_LAST_FREE_OBJECTID -256ULL
int btrfs_init_root_free_objectid(struct btrfs_root *root)
{
    int ret;
    // ...
    search_key.objectid = BTRFS_LAST_FREE_OBJECTID;
    search_key.type = -1;
    search_key.offset = (u64)-1;
    ret = btrfs_search_slot(NULL, root, &search_key, path, 0, 0);

    if (path->slots[0] > 0) {
        slot = path->slots[0] - 1;
        l = path->nodes[0];
        btrfs_item_key_to_cpu(l, &found_key, slot);
        root->free_objectid = max_t(u64, found_key.objectid + 1,
                                        BTRFS_FIRST_FREE_OBJECTID);
    } else {
        root->free_objectid = BTRFS_FIRST_FREE_OBJECTID;
    }
    ret = 0;
    return ret;
}
```

In the code above `btrfs` literally increments `root->free_objectid` as
a way to generate new `inode` number. On fresh filesystems `inode` numbers
for files and directories start from `256` (`BTRFS_FIRST_FREE_OBJECTID`).
On used filesystem they start from the next after largest already
allocated `inode`.

Note that file removal does not normally reclaim the `inode` numbers.
Let's poke a bit at it in action:

```
# create empty btrfs filesystem:
$ fallocate -l 10G fs.raw
$ mkfs.btrfs fs.raw
$ mkdir m
$ mount fs.raw m
$ cd m

# first file on disk:
$ touch first
$ ls -li first
257 -rw-r--r-- 1 root root 0 Sep  7 15:10 first

# second file on disk:
$ rm first && touch first
$ ls -li first
258 -rw-r--r-- 1 root root 0 Sep  7 15:10 first
```

Despite the same filename being deleted and recreated in place it's
`inode` number increases.

There is one exception to "always increasing" rule: if we delete files
with highest `inode` numbers and unmount/remount the filesystem we will be
able to unwind `free_objectid` back a bit:

```
# remount empty and try again:
$ rm first

$ cd ..
$ umount m
$ mount fs.raw m
$ cd m

$ touch first
$ ls -li first
257 -rw-r--r-- 1 root root 0 Sep  7 15:11 first
```

Note: after the remount the `inode` number is back to `257` (and not `259`).
`256` `inode` is taken by `/` root directory.

Back to the question why my filesystem has `inode` numbers above 4
billion: apparently I managed to create that many files throughout the
lifetime of this filesystem. It's a 2 years old `btrfs`. This means
filesystem sees about 70 files per second being created and deleted.

## More failures

After fixing `patchelf` locally I tried to build more `i686` packages
(mainly `wine` dependencies) and discovered a few more similar failures.
One of them was in `which` command:

```
$ bison
bison: missing operand
Try 'bison --help' for more information.

$ which bison
which: no bison in (... long list of PATHs here, one of them with `bison`)
```

If we look `bison` up manually it's there:

```
$ for p in ${PATH//:/ }; do [ -f $p/bison ] && ls -li $p/bison; done
4386192903 -r-xr-xr-x 2 root root 678408 Jan  1  1970 /nix/store/mf37crpkvz388nmqqvkbnmvp21663w26-bison-3.8.2/bin/bison
```

Proposed `which` fix for `nixpkgs` as
<https://github.com/NixOS/nixpkgs/pull/253382> and upstream as
<https://github.com/CarloWood/which/pull/1>.

`which` fix allowed `i686` to progress a bit more and now it stumbled on
`fontconfig` and `tpm2-tss`. To be debugged.

## Parting words

32-bit file APIs are not just about handling of files larger than 4GB
in size. Nowadays' filesystems can easily have other fields that don't
fit into 32-bit counters. One of them is `inode` number. Next in the queue
will probably be 64-bit `time_t`.

The 64-bit interfaces are opt-in for many 32-bit targets and will remain
such for the foreseeable future. Each individual project will have to
adapt to it by adding `-D_FILE_OFFSET_BITS=64` (and soon
`-D_TIME_BITS=64`).

While projects gradually migrate to new APIs `bootstrapTools` should be
rebuilt to get the updates. I hope some form of
<https://github.com/NixOS/nixpkgs/issues/253713> process will be in
place to make it smoother. Otherwise, one-off
<https://github.com/NixOS/nixpkgs/issues/253274> update will have to do.

If you see a project that still uses 32-bit APIs please send a patch
upstream to use 64-bit API if possible. Chances are it will fix real
breakage on filesystems with 64-bit `inodes`.

Have fun!
