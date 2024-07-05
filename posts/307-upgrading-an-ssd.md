---
title: "upgrading an ssd"
date: December 15, 2023
root: "http://trofi.github.io"
---

In search of bugs I build a lot of software locally. About ~20000
packages per day. I usually keep all builds around to speed up
regression debugging.

That way (and with help of filesystem compression, `duperemove` and
identical file hardlinking) I manage to fill up my `512G` SSD with build
results within 2-3 weeks.

Once the disk is full I have to trigger garbage collection that frees
all that space and start over.

I decided to switch to a larger `2T` SSD to expand the time budget to
1-2 months.

This is my boot disk on `btrfs` and I would like to preserve most of
it's properties without too much of mountpoint juggling or machine
downtime. AFAIU `rsync` does not handle advanced filesystem features
like subvolume layouts and already deduplicated data.

I ended up plugging in a new device and did two commands to transfer all
the data live from one device to another:

```
$ btrfs device add    /dev/nvme1n1p2 /
$ btrfs device remove /dev/nvme0n1p3 /
```

15 minutes later all the data was on the new `SSD`! Magic!

## The actual procedure

It required a tiny bit of extra work to handle partitioning on a new
device and `EFI` `vfat` partition move.

Here is the sequence I used:

1. Plug a new device in, it detected as `/dev/nvme1n1`.
2. Partition new device:
   ```
   # fdisk /dev/nvme1n1
   g; n; 1; 2048 (default); +4G; t; 1 (EFI); n; 2; w
   ```

   Here we create 2 partitions: 4G `EFI` and the rest on Linux.

3. Format `EFI` partition:

   ```
   # mkfs.fat -F 32 /dev/nvme1n1p1
   ```
4. Sync `EFI` data to the new partition:

   ```
   # mkdir /new-boot
   # mount /dev/nvme1n1p1 /new-boot
   # rsync -av /boot/ /new-boot/
   # umount /new-boot
   # rmdir /new-boot
   ```
5. Update `/etc/nixos/hardware-configuration.nix` to point `EFI`
   partition to the new `device = "/dev/disk/by-uuid/ABCD-1234";`
6. **Migrate the root filesystem**:

   ```
   # btrfs device add    /dev/nvme1n1p2 /
   # btrfs device remove /dev/nvme0n1p3 /
   ```
7. Rebuild boot loader configuration and reinstall it:
   ```
   # nixops-rebuild switch --install-bootloader
   ```
8. Reboot the machine.

Done!

It took 15 minutes to remove the device and evacuate all the data out.

A snapshot of migration state somewhere in the middle of the process:

```
# btrfs fi show /
Label: none  uuid: abcdef12-...
        Total devices 2 FS bytes used 201.63GiB
        devid    1 size 0.00B used 64.03GiB path /dev/nvme0n1p3
        devid    2 size 1.86TiB used 141.00GiB path /dev/nvme1n1p2
```

Note how to-be-removed device had `size 0.00B` while it still had to
drain `64G` of data.

## Parting words

`btrfs` device handling is magic! It does not matter if the new device
is smaller or larger than existing one: you add bytes to the pool and
new block groups get allocated there. Deleting old devices is also
straightforward: evacuated device stops being used for new object
allocation and existing block groups are evacuated to other devices.

`btrfs device remove` wipes filesystem superblock and removes the device
from device tree of filesystem once data is fully drained. There is no
easy way to access data on the old device after the move. It is slightly
scary but has it's charm as well: there is no chance to accidentally
mount old device and use it as new for a while.

By default `NixOS` uses `/dv/disk/by-uuid/...` device paths:

```
$ cat /etc/fstab
/dev/disk/by-uuid/abcdef12-1dbb-... / btrfs x-initrd.mount,subvol=nixos,noatime,compress=zstd 0 0
/dev/disk/by-uuid/ABCD-1234 /boot vfat umask=1022,quiet,codepage=866,iocharset=utf8,dmask=1022,fmask=1133 0 2
```

That means device rename and move on `btrfs` is transparent to the
configuration as `UUID` gets preserved on new device addition.

Have fun!
