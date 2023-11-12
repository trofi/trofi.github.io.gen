---
title: "duperemove speedups"
date: November 12, 2023
---

[`duperemove`](https://github.com/markfasheh/duperemove/) is a great
tool to reduce on-disk file redundancy for file systems that support
data sharing across files. Currently `duperemove` supports only `btrfs`
and `xfs`.

## initial user experience

I started using `duperemove` around version `0.9` when I started keeping
multiple `chroot` environments for various Linux distributions. I had
about 15 instances of various systems:

```
/chroot/gentoo-amd64-multilib/{bin,lib,usr,...}
/chroot/gentoo-amd64-O3/{bin,lib,usr,...}
/chroot/gentoo-amd64-lto/{bin,lib,usr,...}
/chroot/gentoo-i686/{bin,lib,usr,...}
/chroot/debian-sid/{bin,lib,usr,...}
...
```

Some of the files had identical or mostly identical content across
systems. The typical source of duplicates would be a
`/usr/lib/locale/locale-archive` file. Today it contains `~200MB` worth
of locales on systems with complete set of locales. Another source is
huge dumps of translation files in `/usr/share/locale`.

As I used `btrfs` I ran `duperemove` time to time on a `/chroot`
directory to claw back a bit of storage eaten by these duplicates. I had
a spinning disk at the time and I was amazed by the speed that
`duperemove` took to do it's magic.

## more recent usage attempt

A few years later I got an `NVMe` `SSD` storage and started using `nix`
extensively to build a lot of packages. `nix` can produce quite a bit of
duplication when building package with minimal changes:

```
$ ls -1d /nix/store/????????????????????????????????-glibc-2.38-27
/nix/store/4jyz743dan9fn8b53cdxl5fyld2hkaby-glibc-2.38-27
/nix/store/as01fdk2w1605lr3lmpqpwa1xan17gbd-glibc-2.38-27
/nix/store/d9bhmzah59gzbm7bkki7i8fm0p4nyiyv-glibc-2.38-27
/nix/store/iwnxvprzvymiigxds0pw53sxg11m4azk-glibc-2.38-27
/nix/store/ldavvsk4f57lmw816ch0c4v82hf6ww8g-glibc-2.38-27
/nix/store/mk28ys1qrw7mb04psrnmr0p56bxw3g54-glibc-2.38-27
/nix/store/pnnqn1gh3jd7imjjyibgf8r0n0zjrxf5-glibc-2.38-27
/nix/store/ps99gh0kq7yar94ap6ya6d2av2rfa8dz-glibc-2.38-27
/nix/store/qn3ggz5sf3hkjs2c797xf7nan3amdxmp-glibc-2.38-27
/nix/store/qpspr9zw4vxq6fq3rc1izqsglk497m67-glibc-2.38-27
/nix/store/wrv286x4aldgbj6gjl15qn8pl233zrsx-glibc-2.38-27
/nix/store/y2yvsr2gb27ixz8mc42ry4q6lpasl0fk-glibc-2.38-27

$ ls -1d /nix/store/????????????????????????????????-firefox-119.0.1
/nix/store/c3c387alqga9b9s0r4n064d6kkan07dy-firefox-119.0.1
/nix/store/ck5k36nn46vbpc534hvncbana2rmdpxj-firefox-119.0.1
/nix/store/qx36d083630w1ksp3n38avsyk52zxf9j-firefox-119.0.1
/nix/store/r4cjmc042q18bi7xg2jmcxqs8nzl4fr9-firefox-119.0.1
/nix/store/s2qiq9xszj4k7z64ri3lrl1hwqa48v3p-firefox-119.0.1
/nix/store/zns8fsz0c7adk7aw1x11kal6235jxxya-firefox-119.0.1
```

Sometimes two versions of a package build only differ in the embedded
paths of their dependencies: it's a 32 byte difference between `ELF`
files. For the example above `duperemove` quickly spots the similarity:

```
$ duperemove -b4096 --batchsize=0 -q --dedupe-options=partial -rd /nix/store/????????????????????????????????-glibc-2.38-27

Simple read and compare of file data found 1174 instances of files that might benefit from deduplication.
Comparison of extent info shows a net change in shared extents of: 266229126
Found 133 identical extents.
[########################################]
Search completed with no errors.
Simple read and compare of file data found 114 instances of extents that might benefit from deduplication.
Comparison of extent info shows a net change in shared extents of: 2183168
Total files scanned:  10636
```

Here `duperemove` managed to deduplicate `70%` (`253MB` out of `357MB`
considered) in the extent pass comparison. And the extracted extra `2MB`
of duplicates when considered `4KB` blocks within different extents.

I decided to try `duperemove` on the whole of my `/nix/store` directory.
I ran `duperemove-0.11` and got failures related to exhausted file
descriptors: `duperemove` ran with `32x` parallelism and was able to hit
`4096` open files. That was easy to fix by
[raising the file limit](https://github.com/markfasheh/duperemove/pull/269).
I think it worked as fast as before.

But a while after I ran `duperemove-0.13` against `/nix/store`. 2 hours
later I found that it did not finish and ate `100%` of the CPU. That was
unexpected.

I was not sure if it was a particular file that is causing trouble or
the sheer load on `duperemove` that made it degrade so much under the
load.

I attempted to run `duperemove` in incremental mode and found out that
it rescans all the files on the database on each run effectively making
the incremental mode quadratic. I filed [a bug](https://github.com/markfasheh/duperemove/issues/303)
to see if it could be fixed.

Jack implemented incremental mode the same day! I tried it and saw an
improvement. But the result was still too slow to run on the whole of
`/nix/store` within a day. I could not easily pin point the problem of
`100%` CPU usage on my workloads.

## `duperemove` complexity intuition

What are the `duperemove`'s scaling limits? I had about `4 million`
files taking `300GB` of storage in `/nix/store` on `NVMe` device.

Quick quiz: How long should it take to dedupe that data you would say? A
minute, an hour, a day?

In theory all it takes to do is to read all the data out, checksum it
and attempt the deduplication on identified candidates. Should be an
IO-bound problem without too many random reads.

Given that `duperemove` has an optional `sqlite` database to persist
details about previous runs it even skips data read of the files it
already processed.

If we have a reasonable fast IO storage capable of 1GB/s of sequential
read throughput then it should ideally take about `300GB / 1GB/s = ~5
minutes`. And on top of that there should be some minor overhead to
calculate checksums and store some state in `sqlite` database. That was
my naive reasoning :)

Practice shown that `duperemove` found the CPU-heavy work to do for
hours on my machine.

## synthetic tests

I ran `perf top` and noticed that `duperemove` shown unusual reading on
various stages of a run: at one point most of the time was spent in
`sqlite` internals, at another one some `rb_next()` function took most
of the time. I did not expect such things in an IO-mostly workload.

As it was not very convenient to experiment with `duperemove`'s
behaviour on real data I tried to throw synthetic workloads at it.

I started simple: created `100 thousands` files of `1KB` size and ran
`duperemove` at it:

```bash
echo "Creating directory structure, will take a minute"
mkdir dd
for d in `seq 1 100`; do
    mkdir dd/$d
    for f in `seq 1 1000`; do
        printf "%*s" 1024 "$f" > dd/$d/$f
    done
done
sync
```

How long should it take to run? Maybe 1-2 seconds? Alas running it for
real shown the following:

```
$ time ./duperemove -q -rd dd/
...
Nothing to dedupe.
Total files scanned:  100000
real    0m39,835s
user    1m54,903s
sys     0m8,922s
```

Almost `40 seconds` of real time and almost `2 minutes` of user time
(`duperemove` runs some actions in parallel) to process `100MB` of data.

But what did `duperemove` do all that time? Let's ask `perf`:

```
$ perf record ./duperemove -q -rd dd/
$ perf report

# Overhead  Command       Shared Object            Symbol
# ........  ............  .......................  ...........................................
#
    70.81%  pool          libc.so.6                [.] __memset_avx2_unaligned_erms
     2.14%  duperemove    libsqlite3.so.0.8.6      [.] sqlite3VdbeExec
     0.97%  pool          libsqlite3.so.0.8.6      [.] sqlite3VdbeExec
     0.58%  pool          libc.so.6                [.] __memmove_avx_unaligned_erms
...
```

Vast majority of the CPU time it spent in `memset()`!

## `memset()` fix

There are various ways to find the `memset()` call. I took the lazy
approach to check where `memset()` is called with a large value and did
not find any offenders. Then I checked all `calloc()` calls and found
huge `calloc(8MB)` allocating temporary space to read files out. This
buffer was allocated at each new opened file.
[The fix](https://github.com/markfasheh/duperemove/pull/318) was simple:

```diff
--- a/file_scan.c
+++ b/file_scan.c
@@ -887,7 +887,7 @@ static void csum_whole_file(struct filerec *file,
        struct block_csum *block_hashes = NULL;

        memset(&csum_ctxt, 0, sizeof(csum_ctxt));
-       csum_ctxt.buf = calloc(1, READ_BUF_LEN);
+       csum_ctxt.buf = malloc(READ_BUF_LEN);
        assert(csum_ctxt.buf != NULL);
        csum_ctxt.file = file;
```

After the fix profile looked a bit better:

```
$ time perf record ./duperemove -q -rd dd/
real    0m13,046s
user    0m11,194s
sys     0m2,581s

$ perf report
...
# Overhead  Command       Shared Object            Symbol
# ........  ............  .......................  ..........................................
#
    18.71%  duperemove    libsqlite3.so.0.8.6      [.] sqlite3VdbeExec
     3.18%  pool          libsqlite3.so.0.8.6      [.] sqlite3VdbeExec
     1.95%  pool          libsqlite3.so.0.8.6      [.] sqlite3WhereBegin
     1.66%  pool          libsqlite3.so.0.8.6      [.] resolveExprStep
     1.64%  pool          libsqlite3.so.0.8.6      [.] whereLoopAddBtreeIndex
     1.52%  pool          libc.so.6                [.] __memmove_avx_unaligned_erms
     1.17%  pool          libsqlite3.so.0.8.6      [.] sqlite3_str_vappendf
     1.13%  duperemove    duperemove               [.] populate_tree
     1.10%  pool          libc.so.6                [.] _int_malloc
```

That was a lot better: almost `3x` speed up just for removing a single
redundant `memset()`. It's a safe change as `duperemove` guarantees that
it initializes the area with data from the file before calculating the
hash.

Now `sqlite` is at the top of our profile. Looks like the rest of `80%`
samples goes to IO wait time.

## needless work on small files

While the test seemed to run quickly I noticed that it complains about
dedupe attempts all the time:

```
$ ./duperemove -q -rd dd/
...
Dedupe for file "dd/20/426" had status (1) "data changed".
Dedupe for file "dd/20/427" had status (1) "data changed".
Dedupe for file "dd/20/240" had status (1) "data changed".
Dedupe for file "dd/20/439" had status (1) "data changed".
Dedupe for file "dd/20/377" had status (1) "data changed".
Dedupe for file "dd/20/378" had status (1) "data changed".
Dedupe for file "dd/20/452" had status (1) "data changed".
Dedupe for file "dd/20/453" had status (1) "data changed".
```

Why does `duperemove` think data has changed? Those are static files
I just created for test.

The answer was surprising:

- [good] `duperemove` skips inline extents in files as file systems can't
  deduplicate data embedded in metadata blocks
- [bad] `duperemove` stores checksum of such files as if they were zero
  bytes
- [very bad] `duperemove` tries to deduplicate all these files as they
  have identical checksum

That was [easy to fix](https://github.com/markfasheh/duperemove/pull/322)
as well:

```diff
--- a/file_scan.c
+++ b/file_scan.c
@@ -937,10 +937,19 @@ static void csum_whole_file(struct filerec *file,
                }
        }

-       ret = dbfile_store_file_digest(db, file, csum_ctxt.file_digest);
-       if (ret) {
-               g_mutex_unlock(&io_mutex);
-               goto err;
+       /* Do not store files with zero hashable extents. Those are
+        * usually small files inlined with extent type
+        * FIEMAP_EXTENT_DATA_INLINE. We avoid storing them as all these
+        * files have the same zero bytes checksum. Attempt to
+        * deduplicate those will never succeed and will produce a lot
+        * of needless work: https://github.com/markfasheh/duperemove/issues/316
+        */
+       if (nb_hash > 0) {
+               ret = dbfile_store_file_digest(db, file, csum_ctxt.file_digest);
+               if (ret) {
+                       g_mutex_unlock(&io_mutex);
+                       goto err;
+               }
        }

        ret = dbfile_commit_trans(db);
```

A few lines above `duperemove` calculates count of non-inline extents
with data in `nb_hash` variable. If it's zero then checksum is zero.

The fix as is speeds the scan quite a bit:

```
$ time ./duperemove -q -rd dd/
...
Simple read and compare of file data found 0 instances of files that might benefit from deduplication.
Nothing to dedupe.
Found 0 identical extents.
Simple read and compare of file data found 0 instances of extents that might benefit from deduplication.
Nothing to dedupe.
Simple read and compare of file data found 0 instances of files that might benefit from deduplication.
Nothing to dedupe.
Found 0 identical extents.
Simple read and compare of file data found 0 instances of extents that might benefit from deduplication.
Nothing to dedupe.
Total files scanned:  100000

real    0m7,844s
user    0m7,116s
sys     0m1,686s
```

This is an extra `2x` speed up on tiny files. But we can squeeze a bit
more speed out of it. Notice repetitive `Found 0 identical extents`
entries. This happens because `duperemove` batches deduplication
attempts every `1024` files (controlled by `--batchsize=` flag). We can
crank up that flag as well:

```
$ time ./duperemove --batchsize=1000000 -q -rd dd/
Simple read and compare of file data found 0 instances of files that might benefit from deduplication.
Nothing to dedupe.
Found 0 identical extents.
Simple read and compare of file data found 0 instances of extents that might benefit from deduplication.
Nothing to dedupe.
Total files scanned:  100000

real    0m5,995s
user    0m4,605s
sys     0m1,677s
```

Compared to our initial `50s` runtime we got `~10x` speed up.

Question for the reader: what does `duperemove` do in those 5 seconds?
Is there any room for improvement here?

The above set of fixes sped up `duperemove` on my real `300GB` dataset
to finish in 2 minutes!

All done?

## `--dedupe-options=partial` mode

When I skimmed through existing bugs on `duperemove` bug tracker I
noticed that `duperemove` only deduplicates extents of identical size
and does not try to look into individual blocks for performance reason.

That sounded a bit strange to me as break up of a file on extents is
quite arbitrary at least on `btrfs`. You can easily have one extent for
a huge file or a ton of really small ones.

I threw `--dedupe-options=partial` at `300GB` and got a CPU-bound hangup
again. An hour later I had to interrupt the process.

This time most of the time was spent in `sqlite` extent queries for each
individual file. Let's looks at
[the fix](https://github.com/markfasheh/duperemove/pull/324) to get idea
where the problem was hiding:

```diff
--- a/dbfile.c
+++ b/dbfile.c
@@ -1424,9 +1424,8 @@ int dbfile_load_nondupe_file_extents(sqlite3 *db, struct filerec *file,
        struct file_extent *extents = NULL;

 #define NONDUPE_JOIN                                                   \
-       "FROM extents JOIN (SELECT digest FROM extents GROUP BY digest "\
-       "HAVING count(*) = 1) AS nondupe_extents on extents.digest = "  \
-       "nondupe_extents.digest where extents.ino = ?1 and extents.subvol = ?2;"
+       "FROM extents where extents.ino = ?1 and extents.subvol = ?2 and " \
+       "(1 = (SELECT COUNT(*) FROM extents as e where e.digest = extents.digest));"
 #define GET_NONDUPE_EXTENTS                                            \
        "select extents.loff, len, poff, flags " NONDUPE_JOIN
```

The change switches from this query:

```sql
SELECT extents.loff, len, poff, flags
FROM extents JOIN (
    SELECT digest
    FROM extents
    GROUP BY digest
    HAVING count(*) = 1) AS nondupe_extents
ON extents.digest = nondupe_extents.digest
WHERE extents.ino = ?1 AND extents.subvol = ?2;
```

To this query:

```sql
SELECT extents.loff, len, poff, flags
FROM extents
WHERE extents.ino = ?1 AND extents.subvol = ?2 AND (
    1 = (SELECT COUNT(*)
         FROM extents as e
         WHERE e.digest = extents.digest));
```

Both queries should produce identical result and both do two things:

1. Fetch all entries from `extents` table for a given `inode` (`ino`
   and `subvol`)
2. Pick only those extents that have unique digest: so that extents are
   not shared and thus splitting them into smaller extents should not be
   a problem.

Here are both plans for both queries told by `sqlite`. Enabling plan
dump:

```
$ sqlite3 /tmp/foo.db
sqlite> .eqp on
```

First plan:

```
sqlite> SELECT extents.loff, len, poff, flags
       FROM extents JOIN (
         SELECT digest
         FROM extents
         GROUP BY digest
         HAVING count(*) = 1) AS nondupe_extents
       on extents.digest =  nondupe_extents.digest
       where extents.ino = ?1 and extents.subvol = ?2;

QUERY PLAN
|--CO-ROUTINE nondupe_extents
|  `--SCAN extents USING COVERING INDEX idx_extent_digest
|--SEARCH extents USING INDEX idx_extents_inosub (ino=? AND subvol=?)
|--BLOOM FILTER ON nondupe_extents (digest=?)
`--SEARCH nondupe_extents USING AUTOMATIC COVERING INDEX (digest=?)
```

Second plan:

```
sqlite> SELECT extents.loff, len, poff, flags
        FROM extents
        where extents.ino = ?1 and extents.subvol = ?2 and (
          1 = (SELECT COUNT(*)
              FROM extents as e
              where e.digest = extents.digest));

QUERY PLAN
|--SEARCH extents USING INDEX idx_extents_inosub (ino=? AND subvol=?)
`--CORRELATED SCALAR SUBQUERY 1
   `--SEARCH e USING COVERING INDEX idx_extent_digest (digest=?)
```

The first plan is more complicated. One of it's problems is the use of
`SCAN` (full table scan) in `CO-ROUTINE nondupe_extents`. As I understand
the output here full `extents` table scan is performed at least once for
this whole query.

Reading the second plan is easy: all searches use existing indexes in
the tables. We fetch all extents for the `inode` and then leave only
those that match a subquery. Subquery also uses only index lookup.

Now the whole non-incremental `duperemove` run on my `300GB` dataset
takes 9 minutes. And in incremental mode it takes about 4 minutes.

Yay!

## Parting words

It took me a few steps to get `duperemove` to work on my machines:

- [Increase file descriptor limit](https://github.com/markfasheh/duperemove/pull/269)
- [Avoid redundant `memset(8MB)` on file read](https://github.com/markfasheh/duperemove/pull/318)
- [Do not deduplicate inline-only files](https://github.com/markfasheh/duperemove/pull/322)
- [Avoid full table scan in `partial` mode](https://github.com/markfasheh/duperemove/pull/324)

This sped up `duperemove` run from multiple hours down to under 10
minutes on a few hundreds of gigabytes of small files.

`duperemove` still has quite a bit room for improvement to get even more
performance.

Fun fact: `duperemove` uses two simple `ioctl()` interfaces:

- `FS_IOC_FIEMAP` to get on-disk layout for a file:
  <https://docs.kernel.org/filesystems/fiemap.html>.
- `FIDEDUPERANGE` to deduplicate file range between two file
  descriptors.

Have fun!
