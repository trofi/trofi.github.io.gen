---
title: seekwatcher
date: March 10, 2012
---

:PostID: 169
:Title: seekwatcher
:Keywords: gentoo, seekwatcher, btrfs, io, english, seek, cvs
:Categories: notes

`seekwatcher <http://oss.oracle.com/~mason/seekwatcher/>`_ is a nice
tool written by **btrfs** author and primary maintainer Chris Mason.

It is designed to visualise seek-bound tasks.

Amount of **seek()** operations is about ~**100-200** per second on rotational
drives (**5ms** to reposition heads).
The more your **HDD** is - the more seek latency will bite you.

Let's look at a fun workload: **cvs update** on a large project: **gentoo-x86**
**CVS** tree.

.. raw:: html

   <!--more-->

.. code-block:: bash

    ~/portage/gentoo-x86:find -type f | wc -l
    165532

It means, that every time I issue **cvs update** on that directory
I get **160K** **stat()** calls first, and then about **50K** reads of
**CVS/** metadata.

Sample session to get seekwatcher images and movies!

.. code-block:: bash

    # record trace
    sudo seekwatcher -t cvs-up.trace -p 'sleep 300' -d /dev/sda2
    # make a movie
    seekwatcher -t cvs-up.trace -o cvs-up-trace.mpg --movie
    # rerender .png:
    seekwatcher -t cvs-up.trace -o cvs-up.png

I've performed the test for 2 cases:

1. **gentoo-x86** resides on **640GB** btrfs filesystem. Quite fragmented. Thus seeks are large.
2. **gentoo-x86** resides on **1GB** btrfs loopback file. The file is defragmented, so seeks
   generally smaller (until we access loop's metadata).
   Loop's Contents is fragmented as well (but located within 1GB area, that way we enforce data locality).

Images!

**640GB** case:

- image: `|640GB-img| <http://slyfox.ath.cx:8081/btrfs/seekwatcher/20120310/640GB-fs-portage-cvs-up.png>`_
- movie: `~2MB MPG <http://slyfox.ath.cx:8081/btrfs/seekwatcher/20120310/640GB-fs-portage-cvs-up.mpg>`_

.. |640GB-img| image:: http://slyfox.ath.cx:8081/btrfs/seekwatcher/20120310/640GB-fs-portage-cvs-up.png

**1GB** case:

- image: `|1GB-img| <http://slyfox.ath.cx:8081/btrfs/seekwatcher/20120310/1G-loop-portage-cvs-up-trace.png>`_
- movie: `~2MB MPG <http://slyfox.ath.cx:8081/btrfs/seekwatcher/20120310/1G-loop-portage-cvs-up-trace.mpg>`_

.. |1GB-img| image:: http://slyfox.ath.cx:8081/btrfs/seekwatcher/20120310/1G-loop-portage-cvs-up-trace.png

Lessons learned:

- seekwatcher is a fun tool to play with. Make sure you've grabbed
  latest hg version (or gentoo's **0.12_p20091015** one :]).
- **seek()** I/O pattern is surprisingly important for **btrfs** due to
  scattered data and metadata across the drive.
- data and metadata locality matters on certain workloads and currently
  there is no sane way to enforce it (small loop device is good enough hack though).
  Second case gave **4** times performance boost.
 