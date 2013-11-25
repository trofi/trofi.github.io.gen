---
title: turbo boost emerge --sync and cvs up on gentoo-x86
date: November 17, 2012
---

:PostID: 176
:Title: turbo boost emerge --sync and cvs up on gentoo-x86
:Keywords: emerge, portage, gentoo, gentoo-x86, btrfs, loop, speed, english
:Categories: notes

As I wrote `before <http://hackie.blog.tut.by/2012/03/10/seekwatcher/>`_
**portage** operations are seek-bound and better handled by small loop device.

I had some problems creating such **btrfs** loop device, due to nonworking
**nocow** loop devices, but now I have the solution!

.. raw:: html

   <!--more-->

The recipe:

1. [for btrfs host FS] Make sure you have at least
   `>=sys-fs/e2fsprogs-1.42.6 <https://bugs.gentoo.org/show_bug.cgi?id=420925>`_
   which has **chattr**/**lsattr** with **nocow** support.

2. **[for btrfs host FS]**
   Mark a directory with loop device as nocow::

       $ mkdir -p /subvolumes/nocow-images/
       $ chattr +C /subvolumes/nocow-images/
       $ sudo lsattr -ld /subvolumes/nocow-images/
       /subvolumes/nocow-images/    No_COW

3. Build small btrfs loop device::

       dd if=/dev/zero of="$image" bs=1M count=1K
       mkfs.btrfs           \
        -d single -m single \
        -L "$label"         \
        -l 32k -n 32k       \
        "$image"

4. **[for btrfs host FS]**
   Make sure created file is nocow::

       $ lsattr -l /subvolumes/nocow-images/gentoo-32k.img
       /subvolumes/nocow-images/gentoo-32k.img No_COW
       $ /usr/sbin/filefrag /subvolumes/nocow-images/gentoo-32k.img
       /subvolumes/nocow-images/gentoo-32k.img: 1 extent found

5. And mount it::

       /subvolumes/nocow-images/gentoo-32k.img         /gentoo-32k     btrfs           loop,nodatasum,noatime,nodiratime    0       0

6. Amend **/etc/portage/make.conf** like that::

      PORTDIR=/gentoo-32k/portage

   and so on.

7. Have fun!

Some notes:

- Larger leaf sizes allow better inlining of small files
  and speeding up read pattern
- Smaller loop device allow faster reading-out the whole
  loop file in RAM reducing all the I/O.
- Disabling datasums and atimes for loop contents we leave
  only raw data there.

Some numbers:

.. code-block:: bash

    $ time emerge --sync # 72 seconds
    ...
    real    1m12.111s
    user    0m1.706s
    sys     0m1.819s
    
    gentoo-x86 $ time cvs up # ~4 minutes
    ...
    real    4m35.208s
    user    0m3.656s
    sys     0m14.226s
