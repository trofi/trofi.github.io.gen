---
title: cvsps gets an update part 2
date: January 12, 2013
---

:PostID: 181
:Title: cvsps gets update part 2
:Keywords: gentoo, gentoo-x86, cvs, git, fast, mad skillz, english
:Categories: gentoo

Well, it occured to be a horrible memory leak in **CVS**:

`one-liner patch <http://sources.gentoo.org/cgi-bin/viewvc.cgi/gentoo-x86/dev-vcs/cvs/files/cvs-1.12.12-fix-massive-leak.patch?revision=1.1>`_

Now it takes only 190 minutes to convert **gentoo-x86** to **git**.
Final repo size is around **1.2GB**.

Due to erroneous "optimization check" fully sent to network
buffers were freed "a bit incorrectly".
