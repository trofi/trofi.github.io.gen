---
title: "a signal from the stars"
date: March 04, 2023
---

## The distress beacon

A few days ago after another reboot into a new kernel I noticed that
disk led on my desktop started blinking incessantly even when my system
was idle:

![suspicious shimmer](/posts.data/281-a-signal-from-the-stars/01-shimmer.gif)

I was not sure if it was a real thing caused by workload or a
glitch in the led. Flashing looked a bit too frequent for a machine that
does nothing. I was worried if it was trying to wear out my SSD as
quickly as possible.

## Getting the clues

So I started debugging ... a led /o\\.
I closed all the applications including browser and window manager. I
ran `sync` to commit all the in-flight writes to disk. Disk led was
still flashing rapidly.
I ran `iotop -a`. It claimed there was no visible I/O happening. Does it
mean it's just a led problem?
I tried heavyweight hammer and ran `perf ftrace` to see if kernel is
doing anything related to `NVME`:

```
# perf ftrace -a -T 'nvme*' | cat

# tracer: function
#
# entries-in-buffer/entries-written: 0/0   #P:16
#
#           TASK-PID     CPU#     TIMESTAMP  FUNCTION
#              | |         |         |         |
    kworker/6:1H-298     [006]   2569.645201: nvme_setup_cmd <-nvme_queue_rq
    kworker/6:1H-298     [006]   2569.645205: nvme_setup_discard <-nvme_setup_cmd
    kworker/6:1H-298     [006]   2569.749198: nvme_setup_cmd <-nvme_queue_rq
    kworker/6:1H-298     [006]   2569.749202: nvme_setup_discard <-nvme_setup_cmd
    kworker/6:1H-298     [006]   2569.853204: nvme_setup_cmd <-nvme_queue_rq
    kworker/6:1H-298     [006]   2569.853209: nvme_setup_discard <-nvme_setup_cmd
    kworker/6:1H-298     [006]   2569.958198: nvme_setup_cmd <-nvme_queue_rq
    kworker/6:1H-298     [006]   2569.958202: nvme_setup_discard <-nvme_setup_cmd
```

Here we see that every `100ms` kernel runs `nvme_setup_discard` function
from kernel's `kworker` thread. These requests looked suspicious.
This trace was from `linux-6.2`. When I booted back to `linux-6.1` this
`discard` storm disappeared. All was quiet.
In case you are not familiar with SSD `discard` (or `trim`) is an
operation that gives a hint to device that a particular block of data
does not contain useful data and can be recycled for other uses.
`discard` is neither read nor write operation. That's why `iotop -a` did
not see it.

Are these discards useful or harmful? Are they intentional? Why
`linux-6.1` was unaffected? No idea!

## Bisecting the kernel

Given that it's seemingly a behavior change between `6.1` and `6.2`
kernel versions I attempted to bisect the kernel.
Bisecting it was easy: I redirected local `linux` kernel package
definition to local `linux.git` checkout and rebuilt my system against
it.
Here is a diff against `nixpkgs` I used at some point:

```diff
--- a/pkgs/os-specific/linux/kernel/common-config.nix
+++ b/pkgs/os-specific/linux/kernel/common-config.nix
@@ -839,8 +833,6 @@ let
       DVB_DYNAMIC_MINORS = option yes; # we use udev

       EFI_STUB            = yes; # EFI bootloader in the bzImage itself
-      EFI_GENERIC_STUB_INITRD_CMDLINE_LOADER =
-          whenOlder "6.2" (whenAtLeast "5.8" yes); # initrd kernel parameter for EFI
       CGROUPS             = yes; # used by systemd
       FHANDLE             = yes; # used by systemd
       SECCOMP             = yes; # used by systemd >= 231
--- a/pkgs/os-specific/linux/kernel/linux-6.2.nix
+++ b/pkgs/os-specific/linux/kernel/linux-6.2.nix
@@ -3,7 +3,7 @@
 with lib;

 buildLinux (args // rec {
-  version = "6.2";
+  version = "6.1.0-rc8";

   # modDirVersion needs to be x.y.z, will automatically add .0 if needed
   modDirVersion = versions.pad 3 version;
@@ -11,8 +11,9 @@ buildLinux (args // rec {
   # branchVersion needs to be x.y
   extraMeta.branch = versions.majorMinor version;

-  src = fetchurl {
+  src_ = fetchurl {
     url = "mirror://kernel/linux/kernel/v6.x/linux-${version}.tar.xz";
     sha256 = "sha256-dIYvqKtA7a6FuzOFwLcf4QMoi85RhSbWMZeACzy97LE=";
   };
+  src = builtins.fetchGit /home/slyfox/linux.git;
 } // (args.argsOverride or { }))
--- a/pkgs/top-level/linux-kernels.nix
+++ b/pkgs/top-level/linux-kernels.nix
@@ -171,9 +171,9 @@ in {

     linux_6_2 = callPackage ../os-specific/linux/kernel/linux-6.2.nix {
       kernelPatches = [
-        kernelPatches.bridge_stp_helper
-        kernelPatches.request_key_helper
-        kernelPatches.fix-em-ice-bonding
+        #kernelPatches.bridge_stp_helper
+        #kernelPatches.request_key_helper
+        #kernelPatches.fix-em-ice-bonding
       ];
     };

```

Here I did a few things:

- removed explicit `EFI_GENERIC_STUB_INITRD_CMDLINE_LOADER` `.config`
  setting as it disappeared somewhere between `6.1` and `6.2`
- redirected kernel source to local checkout with
  `src = builtins.fetchGit /home/slyfox/linux.git;`
- dropped any backported patches as they failed to apply (and were not
  relevant to storage changes) 

That was enough for me to build the system against that kernel with:

```
$ sudo nixos-rebuild switch --impure --override-input nixpkgs .
```

The minor complication was in the fact that just booting into a bad
kernel was not always enough to trigger instant `discard` storm.
Sometimes I had to run an I/O-heavy application.

In my case running `firefox` for 30 minutes was a solid way to trigger
the problem.

After a few evenings of slow bisect I ended up at
[this commit](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=63a7cb13071842966c1ce931edacbc23573aada5):

```
$ git bisect good
63a7cb13071842966c1ce931edacbc23573aada5 is the first bad commit
commit 63a7cb13071842966c1ce931edacbc23573aada5
Author: David Sterba
Date:   Tue Jul 26 20:54:10 2022 +0200

    btrfs: auto enable discard=async when possible

    There's a request to automatically enable async discard for capable
    devices. We can do that, the async mode is designed to wait for larger
    freed extents and is not intrusive, with limits to iops, kbps or latency.

    The status and tunables will be exported in /sys/fs/btrfs/FSID/discard .

    The automatic selection is done if there's at least one discard capable
    device in the filesystem (not capable devices are skipped). Mounting
    with any other discard option will honor that option, notably mounting
    with nodiscard will keep it disabled.

    Link: https://lore.kernel.org/linux-btrfs/CAEg-Je_b1YtdsCR0zS5XZ_SbvJgN70ezwvRwLiCZgDGLbeMB=w@xxxxxxxxxxxxxx/
    Reviewed-by: Boris Burkov
    Signed-off-by: David Sterba

 fs/btrfs/ctree.h   |  1 +
 fs/btrfs/disk-io.c | 14 ++++++++++++++
 fs/btrfs/super.c   |  2 ++
 fs/btrfs/volumes.c |  3 +++
 fs/btrfs/volumes.h |  2 ++
 5 files changed, 22 insertions(+)
```

It's a seemingly benign `btrfs` change: it only enables `discard=async`
mount option by default for good enough devices. It does not change
anything about `btrfs` implementation:

```diff
--- a/fs/btrfs/volumes.c
+++ b/fs/btrfs/volumes.c
@@ -641,6 +641,9 @@ static int btrfs_open_one_device(struct btrfs_fs_devices *fs_devices,
 	if (!bdev_nonrot(bdev))
 		fs_devices->rotating = true;

+	if (bdev_max_discard_sectors(bdev))
+		fs_devices->discardable = true;
+
 	device->bdev = bdev;
 	clear_bit(BTRFS_DEV_STATE_IN_FS_METADATA, &device->dev_state);
 	device->mode = flags;

```

## The workaround

To restore previous behavior (until we find out if it's expected) I
added `"nodiscard"` mount option at startup and got an old behavior on
`6.2`!

I also sent [the question](https://www.spinics.net/lists/linux-btrfs/msg133128.html)
to `linux-btrfs@` ML to see if it's an expected behavior.

## Digging deeper

Now that we have a workaround let's try to explore where these `discard`
requests come from.
I re-enabled `discard=async` with `sudo mount -oremount,discard=async /`
and spent some time in `firefox` to trigger the storm condition again
when disk led started flashing again.
I checked discards still get generated (and while at it confirmed it's
related to `btrfs`):

```
$ sudo perf ftrace -a -T '*btrfs*discard*' -T '**nvme*' | cat

  kworker/u64:10-1437018 [010]  34878.171198: btrfs_discard_update_discardable <-__btrfs_add_free_space
  kworker/u64:10-1437018 [010]  34878.171198: __btrfs_discard_schedule_work <-btrfs_discard_workfn
  kworker/u64:10-1437018 [010]  34878.275039: btrfs_discard_workfn <-process_one_work
  kworker/u64:10-1437018 [010]  34878.275049: btrfs_discard_extent <-do_trimming
  kworker/u64:10-1437018 [010]  34878.275050: btrfs_map_discard <-btrfs_discard_extent
  kworker/u64:10-1437018 [010]  34878.275055: btrfs_issue_discard <-btrfs_discard_extent
   kworker/11:1H-320     [011]  34878.275095: nvme_queue_rq <-blk_mq_dispatch_rq_list
   kworker/11:1H-320     [011]  34878.275096: nvme_setup_cmd <-nvme_queue_rq
   kworker/11:1H-320     [011]  34878.275097: nvme_setup_discard <-nvme_setup_cmd
   kworker/11:1H-320     [011]  34878.275098: nvme_prep_rq.part.0 <-nvme_queue_rq
          <idle>-0       [011]  34878.275183: nvme_irq <-__handle_irq_event_percpu
          <idle>-0       [011]  34878.275184: nvme_pci_complete_batch <-nvme_irq
          <idle>-0       [011]  34878.275185: nvme_unmap_data <-nvme_pci_complete_batch
          <idle>-0       [011]  34878.275187: nvme_complete_batch_req <-nvme_pci_complete_batch

  kworker/u64:10-1437018 [011]  34878.275204: btrfs_discard_update_discardable <-__btrfs_add_free_space
  kworker/u64:10-1437018 [011]  34878.275204: __btrfs_discard_schedule_work <-btrfs_discard_workfn
  kworker/u64:10-1437018 [000]  34878.379054: btrfs_discard_workfn <-process_one_work
  kworker/u64:10-1437018 [000]  34878.379067: btrfs_discard_extent <-do_trimming
  kworker/u64:10-1437018 [000]  34878.379068: btrfs_map_discard <-btrfs_discard_extent
  kworker/u64:10-1437018 [000]  34878.379073: btrfs_issue_discard <-btrfs_discard_extent
    kworker/1:1H-193     [001]  34878.379215: nvme_queue_rq <-blk_mq_dispatch_rq_list
    kworker/1:1H-193     [001]  34878.379216: nvme_setup_cmd <-nvme_queue_rq
    kworker/1:1H-193     [001]  34878.379216: nvme_setup_discard <-nvme_setup_cmd
    kworker/1:1H-193     [001]  34878.379218: nvme_prep_rq.part.0 <-nvme_queue_rq
          <idle>-0       [002]  34878.379313: nvme_irq <-__handle_irq_event_percpu
          <idle>-0       [002]  34878.379314: nvme_pci_complete_batch <-nvme_irq
          <idle>-0       [002]  34878.379315: nvme_unmap_data <-nvme_pci_complete_batch
          <idle>-0       [002]  34878.379318: nvme_complete_batch_req <-nvme_pci_complete_batch
```

It looks like `btrfs` keeps seeing free space being returned back to the
system which triggers extent discard worker thread.
Ideally I would expect `free` / `discard` / `free` loop to cease at some
point. But it never does.
Let's try to find where does `__btrfs_add_free_space` come from:

```
$ sudo perf ftrace -a -T '__btrfs_add_free_space' | cat

   kworker/u64:1-2379115 [001]  35176.238428: __btrfs_add_free_space <-do_trimming
   kworker/u64:1-2379115 [001]  35176.341720: __btrfs_add_free_space <-do_trimming
   kworker/u64:1-2379115 [001]  35176.446448: __btrfs_add_free_space <-do_trimming
   kworker/u64:1-2379115 [001]  35176.550321: __btrfs_add_free_space <-do_trimming
   kworker/u64:1-2379115 [001]  35176.653996: __btrfs_add_free_space <-do_trimming
   kworker/u64:1-2379115 [001]  35176.758335: __btrfs_add_free_space <-do_trimming
```

If I read it correctly it's initiated by
[`do_trimming()`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/free-space-cache.c?h=v6.2#n3630):

```c
static int do_trimming(struct btrfs_block_group *block_group,
		       u64 *total_trimmed, u64 start, u64 bytes,
		       u64 reserved_start, u64 reserved_bytes,
		       enum btrfs_trim_state reserved_trim_state,
		       struct btrfs_trim_range *trim_entry)
{
	struct btrfs_space_info *space_info = block_group->space_info;
	struct btrfs_fs_info *fs_info = block_group->fs_info;
	struct btrfs_free_space_ctl *ctl = block_group->free_space_ctl;
	int ret;
	int update = 0;
	const u64 end = start + bytes;
	const u64 reserved_end = reserved_start + reserved_bytes;
	enum btrfs_trim_state trim_state = BTRFS_TRIM_STATE_UNTRIMMED;
	u64 trimmed = 0;

	spin_lock(&space_info->lock);
	spin_lock(&block_group->lock);
	if (!block_group->ro) {
		block_group->reserved += reserved_bytes;
		space_info->bytes_reserved += reserved_bytes;
		update = 1;
	}
	spin_unlock(&block_group->lock);
	spin_unlock(&space_info->lock);

	ret = btrfs_discard_extent(fs_info, start, bytes, &trimmed);
	if (!ret) {
		*total_trimmed += trimmed;
		trim_state = BTRFS_TRIM_STATE_TRIMMED;
	}

	mutex_lock(&ctl->cache_writeout_mutex);
	if (reserved_start < start)
		__btrfs_add_free_space(block_group, reserved_start,
				       start - reserved_start,
				       reserved_trim_state);
	if (start + bytes < reserved_start + reserved_bytes)
		__btrfs_add_free_space(block_group, end, reserved_end - end,
				       reserved_trim_state);
	__btrfs_add_free_space(block_group, start, bytes, trim_state);
	list_del(&trim_entry->list);
	mutex_unlock(&ctl->cache_writeout_mutex);

	if (update) {
		spin_lock(&space_info->lock);
		spin_lock(&block_group->lock);
		if (block_group->ro)
			space_info->bytes_readonly += reserved_bytes;
		block_group->reserved -= reserved_bytes;
		space_info->bytes_reserved -= reserved_bytes;
		spin_unlock(&block_group->lock);
		spin_unlock(&space_info->lock);
	}

	return ret;
}
```

It's a long function, but not too complicated:

- block groups are `~256MB` chunk of bytes on disk that contains various
  items
- there are 3 types of block groups: `DATA` (user's bytes), `MEATADATA`
  (file system metadata) and `SYSTEM` (tiny amount of metadata that
  spans multiple devices).
- `btrfs` tracks two ranges per block group: used range and reserved
  range.
- `btrfs` uses
  [`btrfs_discard_extent()`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/extent-tree.c?h=v6.2#n1319)
  to mark extent as freed.
- `btrfs` uses
  [`__btrfs_add_free_space()`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/free-space-cache.c?h=v6.2#n2609) to cache free space
  info on disk.

Let's repeat the exercise of chasing what initiates the trim to see if
this thing some sort of internal cycle:

```
$ sudo perf ftrace -a -T 'do_trimming' | head -n 10 | tail -n 1
   kworker/u64:7-2381087 [009]  36596.861571: do_trimming <-trim_no_bitmap

$ sudo perf ftrace -a -T 'trim_no_bitmap' | head -n 10 | tail -n 1
   kworker/u64:6-2379320 [015]  36627.125062: trim_no_bitmap <-btrfs_trim_block_group_extents

   kworker/u64:2-2379316 [010]  36650.500676: btrfs_trim_block_group_extents <-btrfs_discard_workfn
   worker/u64:2-2379316 [005]  36676.812243: btrfs_discard_workfn <-process_one_work
   worker/6:1-2382788 [006]  36709.360688: process_one_work <-worker_thread
```

We hit a dead end: `worker_thread()` pulls in work items from somewhere
and processes them. Let's find what queues those up!

[`btrfs_discard_workfn()`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/discard.c?h=v6.2#n446)
definition is also not too complicated:

```c
/*
 * Discard work queue callback
 *
 * @work: work
 *
 * Find the next block_group to start discarding and then discard a single
 * region.  It does this in a two-pass fashion: first extents and second
 * bitmaps.  Completely discarded block groups are sent to the unused_bgs path.
 */
static void btrfs_discard_workfn(struct work_struct *work)
{
	struct btrfs_discard_ctl *discard_ctl;
	struct btrfs_block_group *block_group;
	enum btrfs_discard_state discard_state;
	int discard_index = 0;
	u64 trimmed = 0;
	u64 minlen = 0;
	u64 now = ktime_get_ns();

	discard_ctl = container_of(work, struct btrfs_discard_ctl, work.work);

	block_group = peek_discard_list(discard_ctl, &discard_state,
					&discard_index, now);
	if (!block_group || !btrfs_run_discard_work(discard_ctl))
		return;
	if (now < block_group->discard_eligible_time) {
		btrfs_discard_schedule_work(discard_ctl, false);
		return;
	}

	/* Perform discarding */
	minlen = discard_minlen[discard_index];

	if (discard_state == BTRFS_DISCARD_BITMAPS) {
		u64 maxlen = 0;

		/*
		 * Use the previous levels minimum discard length as the max
		 * length filter.  In the case something is added to make a
		 * region go beyond the max filter, the entire bitmap is set
		 * back to BTRFS_TRIM_STATE_UNTRIMMED.
		 */
		if (discard_index != BTRFS_DISCARD_INDEX_UNUSED)
			maxlen = discard_minlen[discard_index - 1];

		btrfs_trim_block_group_bitmaps(block_group, &trimmed,
				       block_group->discard_cursor,
				       btrfs_block_group_end(block_group),
				       minlen, maxlen, true);
		discard_ctl->discard_bitmap_bytes += trimmed;
	} else {
		btrfs_trim_block_group_extents(block_group, &trimmed,
				       block_group->discard_cursor,
				       btrfs_block_group_end(block_group),
				       minlen, true);
		discard_ctl->discard_extent_bytes += trimmed;
	}

	/* Determine next steps for a block_group */
	if (block_group->discard_cursor >= btrfs_block_group_end(block_group)) {
		if (discard_state == BTRFS_DISCARD_BITMAPS) {
			btrfs_finish_discard_pass(discard_ctl, block_group);
		} else {
			block_group->discard_cursor = block_group->start;
			spin_lock(&discard_ctl->lock);
			if (block_group->discard_state !=
			    BTRFS_DISCARD_RESET_CURSOR)
				block_group->discard_state =
							BTRFS_DISCARD_BITMAPS;
			spin_unlock(&discard_ctl->lock);
		}
	}

	now = ktime_get_ns();
	spin_lock(&discard_ctl->lock);
	discard_ctl->prev_discard = trimmed;
	discard_ctl->prev_discard_time = now;
	discard_ctl->block_group = NULL;
	__btrfs_discard_schedule_work(discard_ctl, now, false);
	spin_unlock(&discard_ctl->lock);
}
```

Here worker thread expects items of `struct btrfs_discard_ctl` type to
process. Scrolling the file around
[`btrfs_discard_queue_work()`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/discard.c?h=v6.2#n324)
seems to be most plausible candidate we are looking for:

```c
static void __btrfs_discard_schedule_work(struct btrfs_discard_ctl *discard_ctl,
					  u64 now, bool override)
{
	struct btrfs_block_group *block_group;

	if (!btrfs_run_discard_work(discard_ctl))
		return;
	if (!override && delayed_work_pending(&discard_ctl->work))
		return;

	block_group = find_next_block_group(discard_ctl, now);
	if (block_group) {
		u64 delay = discard_ctl->delay_ms * NSEC_PER_MSEC;
		u32 kbps_limit = READ_ONCE(discard_ctl->kbps_limit);

		/*
		 * A single delayed workqueue item is responsible for
		 * discarding, so we can manage the bytes rate limit by keeping
		 * track of the previous discard.
		 */
		if (kbps_limit && discard_ctl->prev_discard) {
			u64 bps_limit = ((u64)kbps_limit) * SZ_1K;
			u64 bps_delay = div64_u64(discard_ctl->prev_discard *
						  NSEC_PER_SEC, bps_limit);

			delay = max(delay, bps_delay);
		}

		/*
		 * This timeout is to hopefully prevent immediate discarding
		 * in a recently allocated block group.
		 */
		if (now < block_group->discard_eligible_time) {
			u64 bg_timeout = block_group->discard_eligible_time - now;

			delay = max(delay, bg_timeout);
		}

		if (override && discard_ctl->prev_discard) {
			u64 elapsed = now - discard_ctl->prev_discard_time;

			if (delay > elapsed)
				delay -= elapsed;
			else
				delay = 0;
		}

		mod_delayed_work(discard_ctl->discard_workers,
				 &discard_ctl->work, nsecs_to_jiffies(delay));
	}
}
```

Note that this handler does not execute the discard requests as soon as
possible! It has at least one rate limiter based on
`discard_ctl->kbps_limit`.

And there are even more rate limiters defined by
[`btrfs_discard_calc_delay()`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/discard.c?h=v6.2#n545)

```c
void btrfs_discard_calc_delay(struct btrfs_discard_ctl *discard_ctl)
{
	s32 discardable_extents;
	s64 discardable_bytes;
	u32 iops_limit;
	unsigned long delay;

	discardable_extents = atomic_read(&discard_ctl->discardable_extents);
	if (!discardable_extents)
		return;

	spin_lock(&discard_ctl->lock);

	/*
	 * The following is to fix a potential -1 discrepancy that we're not
	 * sure how to reproduce. But given that this is the only place that
	 * utilizes these numbers and this is only called by from
	 * btrfs_finish_extent_commit() which is synchronized, we can correct
	 * here.
	 */
	if (discardable_extents < 0)
		atomic_add(-discardable_extents,
			   &discard_ctl->discardable_extents);

	discardable_bytes = atomic64_read(&discard_ctl->discardable_bytes);
	if (discardable_bytes < 0)
		atomic64_add(-discardable_bytes,
			     &discard_ctl->discardable_bytes);

	if (discardable_extents <= 0) {
		spin_unlock(&discard_ctl->lock);
		return;
	}

	iops_limit = READ_ONCE(discard_ctl->iops_limit);
	if (iops_limit)
		delay = MSEC_PER_SEC / iops_limit;
	else
		delay = BTRFS_DISCARD_TARGET_MSEC / discardable_extents;

	delay = clamp(delay, BTRFS_DISCARD_MIN_DELAY_MSEC,
		      BTRFS_DISCARD_MAX_DELAY_MSEC);
	discard_ctl->delay_ms = delay;

	spin_unlock(&discard_ctl->lock);
}
```

Thus here are rate limits we see:

- `discard_ctl->kbps_limit`: discard bytes/sec rate limit
- `discard_ctl->iops_limit`: discard requests/sec rate limit
- `BTRFS_DISCARD_MIN_DELAY_MSEC=1ms` to `BTRFS_DISCARD_MAX_DELAY_MSEC=1s`:
  allowed delay range between discards

Some of these we can inspect and change at runtime:

```
$ cd /sys/fs/btrfs/<UUID>/discard
$ for f in *; do echo -ne "$f:\t"; cat $f; done

discard_bitmap_bytes:   98213888
discard_bytes_saved:    27716325376
discard_extent_bytes:   38577287168
discardable_bytes:      19484499968
discardable_extents:    228442
iops_limit:     10
kbps_limit:     0
max_discard_size:       67108864
```

I'm not sure I believe `discardable_bytes=19484499968` value. This is
supposed to be a discard backlog queued but I'm skeptical. It never goes
down to zero. Looks more like broken accounting. Worse this
(invalid) value is being used to calculate latency of a next request.

## Discard requests timing patterns

So, it looks like 10 discards/sec are expected default on `linux-6.2`.
Let's find the source of those discards. Looking at
[`discard.c`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/btrfs/discard.c)
these functions looked plausible:

```
$ sudo perf ftrace -a -T 'btrfs_discard_workfn' -T 'btrfs_issue_discard' -T 'btrfs_discard_queue_work'
btrfs-transacti-407     [011]  42800.424027: btrfs_discard_queue_work <-__btrfs_add_free_space
btrfs-transacti-407     [011]  42800.424070: btrfs_discard_queue_work <-__btrfs_add_free_space
...
btrfs-transacti-407     [011]  42800.425053: btrfs_discard_queue_work <-__btrfs_add_free_space
btrfs-transacti-407     [011]  42800.425055: btrfs_discard_queue_work <-__btrfs_add_free_space
```

I saw 193 entries of `btrfs_discard_queue_work` above. It took `1ms` to
enqueue all the work into the work queue. Very quick and not too
large. Right after it we see actual discards being sent to the device:

```
kworker/u64:1-2379115 [000]  42800.487010: btrfs_discard_workfn <-process_one_work
kworker/u64:1-2379115 [000]  42800.487028: btrfs_issue_discard <-btrfs_discard_extent
kworker/u64:1-2379115 [005]  42800.594010: btrfs_discard_workfn <-process_one_work
kworker/u64:1-2379115 [005]  42800.594031: btrfs_issue_discard <-btrfs_discard_extent
...
kworker/u64:15-2396822 [007]  42830.441487: btrfs_discard_workfn <-process_one_work
kworker/u64:15-2396822 [007]  42830.441502: btrfs_issue_discard <-btrfs_discard_extent
kworker/u64:15-2396822 [000]  42830.546497: btrfs_discard_workfn <-process_one_work
kworker/u64:15-2396822 [000]  42830.546524: btrfs_issue_discard <-btrfs_discard_extent
```

286 pairs of `btrfs_discard_workfn` / `btrfs_issue_discard`.
Each pair takes `100ms` to process, which seems to match `iops_limit=10`.

And `30s` is also a `btrfs` commit interval where the next batch of
discard work gets landed:

```
btrfs-transacti-407     [002]  42830.634216: btrfs_discard_queue_work <-__btrfs_add_free_space
btrfs-transacti-407     [002]  42830.634228: btrfs_discard_queue_work <-__btrfs_add_free_space
...
```

That means I can get about 300 discards per second max. Also, given that
discards were being sent over full span of `30s` I think that work queue
was not exhausted and there still was backlog in the queue.

I think `discardable_bytes` / `discardable_extents` is the backlog
metric, but I'm not sure as it never gets down to zero.

## Another workaround

Now it's clear we can manipulate the pace by changing the delay between
discards. To speed up the discard pace we can drop `IO` limit with:

```
# echo 10000 > /sys/fs/btrfs/<UUID>/discard/iops_limit
```

That allows getting rid of discard backlog. But I don't know if it's a
reasonable fix or it's better to keep discards be delayed for a while.

## Parting words

`btrfs` uses `discard` to mark extents as free for an underlying device.

`linux-6.2` enabled automatic async discard for `btrfs` on appropriate
SSD devices. This manifests as a constant device activity if you have
any reasonable amount of `IO` on your device (even trivial super block
commits are enough).

Default async discard rate limits `linux` has today are:

- `discard_ctl->kbps_limit = 0`: discard bytes/sec rate limit
- `discard_ctl->iops_limit = 10`: discard requests/sec rate limit
- `BTRFS_DISCARD_MIN_DELAY_MSEC = 1ms` to `BTRFS_DISCARD_MAX_DELAY_MSEC = 1s`:
  allowed delay range between discards

Some of the defaults can be changed at runtime. Just `echo` a new value
to `/sys/fs/btrfs/<UUID>/discard/iops_limit` or `kbps_limit`.

Some of the counter metrics in `/sys/fs/btrfs/<UUID>/discard/*` look
inaccurate.

`perf ftrace` (and `perf trace`!) are nice tools to quickly peek at what
the kernel is doing right now.

Have fun!
