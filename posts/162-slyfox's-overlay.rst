---
title: slyfox's overlay
date: November 16, 2011
---

:PostID: 162
:Title: slyfox's overlay
:Keywords: gentoo, slyfox, overlay, mc, valgrind, xmms2, opentyrian
:Categories: notes

Запилил вчера в **layman** свой оверлей:

.. raw:: html

   <!--more-->

.. code-block:: bash

    $ layman -f
    $ layman -a slyfox

Из весёлого там всякие **live** ебилды:

- **net-fs/btrfs-nocow** - позволяет пофайлово отключать **COW** и сжатие файлов на **btrfs**
- **sys-fs/libeatmydata** - позволяет отключить вызов **fsync()** для конкретного процесса через **LD_PRELOAD** hack (**eatmydata liferea**)
- **dev-util/cppcheck**
- **dev-util/coccinelle**
- **dev-lang/tcc**
- **media-sound/xmms2**
- **app-misc/mc**
- **net-fs/smbnetfs**
- **games-arcade/opentyrian** - классная аркада-леталка из **DOS**а :]

Have fun!
