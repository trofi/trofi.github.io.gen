---
title: profiling your boot
date: July 8, 2012
---

:PostID: 174
:Title: profiling your boot
:Keywords: bootchart2, btrfs, grub, gentoo
:Categories: notes

Как-то недавно выпиливая очередной патч для **btrfs** я наткнулся на
`сообщение одного сэра <http://www.mail-archive.com/linux-btrfs@vger.kernel.org/msg16042.html>`_
о проблеме, которую я только-только `пофиксил <http://www.mail-archive.com/linux-btrfs@vger.kernel.org/msg16045.html>`_ :]

Меня заинтересовала его картинка и я решил запилить себе такую-же.
Это оказалось не только красиво, но и познавательно:

.. raw:: html

   <!--more-->

Как всегда, всё просто как грабли:

.. code-block:: bash

    emerge app-benchmarks/bootchart2

Для более детальной статистики **emerge** попросит включить в ядре
всякие счётчики, трейсеры и проберы для более детальной инфы.
В первый раз можно не париться.

Для **grub** добавляюм строку с **init=/sbin/bootchartd**:

.. code-block:: bash

    title Gentoo Linux
        root (hd0,0)
        kernel /boot/vmlinuz root=/dev/sda2 init=/sbin/bootchartd

Для **grub2**:

.. code-block:: bash

    menuentry 'Gentoo Linux' {
        insmod part_msdos
        insmod ext2
        set root='hd0,msdos1'
        linux /boot/vmlinuz root=/dev/sda2 init=/sbin/bootchartd
    }

Перезагружаемся и получаем лог загрузки в **/var/log/bootchart.tgz**.

Вся статистика собирается в **tmpfs** и сбрасывается по завершению
указанного процесса в **/etc/bootchartd.conf**:

.. code-block:: bash

    # The processes we have to wait for
    EXIT_PROC="agetty mgetty mingetty"

Можно что-нибудь более высокоуровневое впилить, если хочется посмотреть на загрузку **KDE**/**GNOME** частей.

После загрузки можно сгенерить красивый график. Мы сгенерим аж 2 из них:

.. code-block:: bash

    $ pybootchartgui && mv bootchart.png bootchart-default.png
    $ pybootchartgui --show-all && mv bootchart.png bootchart-all.png
    $ pybootchartgui -i # а тут можно руками потыкать в gui приложении

Это результат для моей системы (**Осторожно**! **они здоровенные**):

- `Обычный вывод <http://slyfox.ath.cx:8081/i/bootchart-default.png>`_ (800KB)
- `Детальный вывод <http://slyfox.ath.cx:8081/i/bootchart-all.png>`_ (1.2MB)

По картинкам хорошо видно, что:

- юзерспейс получает управление на **6-й** секунде.
- больше всего ввода-вывода кушает процесс **systemd-udev**, запущенный на **12-й** секунде (грузит модуля, пробит железо, пинает usb_modeswitch и прочее)
- первые **agetty**/**login** стартанули на **47-й** секунде. Это и есть время загрузки.
- **alsactl** вытворяет очень много ввода-вывода (скорее всего просто первая грузит кучу левых либ, с которыми она слинкована)
- удаление всяких временных файлов занимает аж **4** секунды (у меня всегда помойка в **/tmp**)
- в детальном выводе видны даже аргументы, с которыми запускается каждая программа при загрузке (крайне познавательно!)

Have fun! :]
