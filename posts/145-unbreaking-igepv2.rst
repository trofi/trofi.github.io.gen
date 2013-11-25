---
title: unbreaking igepv2
date: July 16, 2011
---

:PostID: 145
:Title: unbreaking igepv2
:Keywords: arm, igepv2, serial, sd, u-boot, tftp, omap, x-loader, linux, embedded
:Categories: notes

Не успел я получить новую **arm** коробку igepv2, как тут-же ее сломал :]

Сначала собрал ядро, это было просто. Выгуглил какой-то древний **defconfig** для
**igepv2** и включил всё, чего мне не хватало. Главным для меня было отсутствие поддержки
**btrfs** и **USB**, который без напильника `не собрался <https://lkml.org/lkml/2011/7/17/68>`_.
Правда, не факт, что именно этот **USB** драйвер мне вообще нужен. И вообще кому-нибудь,
раз он так классно собирается :]. Ядро загрузилось (почти) с первого раза по **tftp**.

.. code-block:: bash

    Kernel command line: mem=512M console=ttyO2,115200n8 root=/dev/sda2 rw rootwait

Распаковал **stage3** для **armv7** и получил нормальную **gentoo** :]

Потом я подумал, что неплохо бы обновить и **u-boot**. Собрал **mainline u-boot** и долго (но не очень :])
пытался его загрузить из существующего. Команда **g <addr>** должна была бы это делать, но
**u-boot** где-то рано вис. Скорее всего какие-то компоненты не выживают повторной инициализации.
Та же фигня, что и на **sheevaplug**. Но **JTAG** у меня не было и я рискнул просто писануть в
**nand** новый **u-boot**. Он загрузился, но отвалилась сеть и, как выяснилось позже, отвалилась
возможность сохранять переменные окружения в самом **u-boot**.

Я предварительно забэкапил (ну это я так думал) старый **u-boot** командой **nanddump** и позже
попробовал его восстановить. В результате вписал полный мусор и сломал загрузку **igepv2** :]

Весь следующий день я изучал как это дело можно восстановить. Загрузка на многих **OMAP** от
**Texas Instruments** начинается с исполнения кастрированного **u-boot** (зовется **x-loader**)
в **ROM**. `Картинка <http://www.omappedia.org/wiki/Bootloader_Project#OMAP_Boot_Sequence>`_,
из которой нихрена не понятно, гласит следующее: **x-loader** пытается загрузиться с 3 типов носителей:

- serial console (**UART**), через которую обычно смотрят, что ж там творится на плате
- **SD** карточка особого формата (на плате есть слот для **microSD**)
- загрузка из 512MB **NAND**

**SD** карточки у меня не было (точнее не хотелось отбирать у владельцев приличных телефонов :]),
так что я попробовал впихать загрузчик через **serial console**. `В теории это не сложно <http://markmail.org/message/ajwqor7dzvkc2yl3>`_.
На практике **u-boot-utils** без `хака <https://github.com/trofi/omap-u-boot-utils/commit/e8978e3a9aa3b81bf6270fa951549c56c4450f3a>`_
не хотел даже передавать данные, а с хаком так и не заставил шайтанское поделие загрузить новый загрузчик.

Вообще это странно и обидно. Это самый красивый способ проверить свежесобранный **u-boot**
и ядро не инициализируя никакие лишние подсистемы (типа **ethernet**, **mmc** или **usb**).
Так что я или осилю **serial** или точно выясню, что он сломан на **igep**.

День повозился с **serial**, узнал что такое **8N1**, как весело программировать эти поганые
последовательные устройства и поплёлся в магазин за **microSD** :]

Открыл страницу по созданию `boot sd <http://code.google.com/p/beagleboard/wiki/LinuxBootDiskFormat>`_
и "всё сделал" (на самом деле таких "страниц" больше одной и все они используют разные **stage-2** **x-loader**).
Естественно, не загрузилось :]. Оказывается, **fdisk** с недавних пор начал считать **units** в секторах,
а не цилиндрах. Из-за этого я создавал невыровненные разделы и **x-loader** посылал меня .

В конце концов с помощью **rob_w** на **#igep** и немного покурив man **fdisk** я решил собрать самый
древний вид раздела:

.. code-block:: bash

    $ fdisk -c=dos -u=cylinders -C 976 -H 255 -S 63 /dev/mmcblk0
         o   # новая таблица разделов
         n   # новый раздел
         p   # primary типа
         1   # первый
         +50 # 50 цилиндров (63 * 255 * 50 * 512 = 400 мегов?)
         a   # загрузочный
         w   # записать таблицу разделов
         q
    $ mount -t msdos /dev/mmcblk0p1 /mnt/igep_boot
    $ cd /mnt/igep_boot
    $ wget http://downloads.igep.es/binaries/x-loader/v1.4.4-0/x-load-1.4.4-0.igep0020-sdcard.bin.ift -O MLO
    $ wget http://downloads.igep.es/binaries/u-boot-arm/v2010.06-0/u-boot-arm-2010.06-0.igep0020.bin -o u-boot.bin

И о чудо! Я получил **prompt** **u-boot**. Загрузился по **tftp** в свое новое ядро и прошил этот
старый **u-boot** в **NAND**. Ура! Можно дальше ломать ядро :].

Кстати, это не так уж и сложно. Я точно не знаю, какие мне нужны драйвера, по сему включаю
в **.config** всякую ерунду типа **RTC**, которого, наверное, вообще нет на **igep** :]

.. code-block:: bash

    [   10.344787] ------------[ cut here ]------------
    [   10.344848] WARNING: at /home/slyfox/linux-2.6/kernel/irq/handle.c:130 handle_irq_event_percpu+0x180/0x19c()
    [   10.344879] irq 379 handler twl_rtc_interrupt+0x0/0x98 enabled interrupts
    [   10.344909] [<c0036e84>] (unwind_backtrace+0x0/0xfc) from [<c00537dc>] (warn_slowpath_common+0x48/0x60)
    [   10.344940] [<c00537dc>] (warn_slowpath_common+0x48/0x60) from [<c0053888>] (warn_slowpath_fmt+0x30/0x40)
    [   10.344970] [<c0053888>] (warn_slowpath_fmt+0x30/0x40) from [<c0080b14>] (handle_irq_event_percpu+0x180/0x19c)
    [   10.345001] [<c0080b14>] (handle_irq_event_percpu+0x180/0x19c) from [<c0080b58>] (handle_irq_event+0x28/0x38)
    [   10.345031] [<c0080b58>] (handle_irq_event+0x28/0x38) from [<c0082894>] (handle_edge_irq+0x7c/0x138)
    [   10.345031] [<c0082894>] (handle_edge_irq+0x7c/0x138) from [<c00806a4>] (generic_handle_irq+0x34/0x40)
    [   10.345062] [<c00806a4>] (generic_handle_irq+0x34/0x40) from [<c0268040>] (handle_twl4030_sih+0x84/0xc4)
    [   10.345092] [<c0268040>] (handle_twl4030_sih+0x84/0xc4) from [<c00806a4>] (generic_handle_irq+0x34/0x40)
    [   10.345123] [<c00806a4>] (generic_handle_irq+0x34/0x40) from [<c0267d5c>] (twl4030_irq_thread+0xa0/0x128)
    [   10.345153] [<c0267d5c>] (twl4030_irq_thread+0xa0/0x128) from [<c006b9c0>] (kthread+0x84/0x8c)
    [   10.345184] [<c006b9c0>] (kthread+0x84/0x8c) from [<c0033818>] (kernel_thread_exit+0x0/0x8)
    [   10.345184] ---[ end trace 5e27d437a975fbd3 ]---

Или вот результат работы ядра после **mainline u-boot + tftp kernel** (видать,
контроллер попался ядру не в очень хорошем состоянии) :

.. code-block:: bash

    [    1.165222] smsc911x: Driver version 2008-10-21
    [    1.840698] irq 336: nobody cared (try booting with the "irqpoll" option)
    [    1.847930] [<c0036e84>] (unwind_backtrace+0x0/0xfc) from [<c0081f58>] (__report_bad_irq+0x20/0xb0)
    [    1.857452] [<c0081f58>] (__report_bad_irq+0x20/0xb0) from [<c00821b0>] (note_interrupt+0x1c8/0x224)
    [    1.867095] [<c00821b0>] (note_interrupt+0x1c8/0x224) from [<c0080a3c>] (handle_irq_event_percpu+0xa8/0x19c)
    [    1.877471] [<c0080a3c>] (handle_irq_event_percpu+0xa8/0x19c) from [<c0080b58>] (handle_irq_event+0x28/0x38)
    [    1.887847] [<c0080b58>] (handle_irq_event+0x28/0x38) from [<c0082cac>] (handle_level_irq+0x80/0xe8)
    [    1.897491] [<c0082cac>] (handle_level_irq+0x80/0xe8) from [<c00806a4>] (generic_handle_irq+0x34/0x40)
    [    1.907318] [<c00806a4>] (generic_handle_irq+0x34/0x40) from [<c02301b8>] (gpio_irq_handler+0x16c/0x1b0)
    [    1.917297] [<c02301b8>] (gpio_irq_handler+0x16c/0x1b0) from [<c00806a4>] (generic_handle_irq+0x34/0x40)
    [    1.927307] [<c00806a4>] (generic_handle_irq+0x34/0x40) from [<c002d030>] (asm_do_IRQ+0x30/0x84)
    [    1.936584] [<c002d030>] (asm_do_IRQ+0x30/0x84) from [<c0032af4>] (__irq_svc+0x34/0xa0)
    [    1.945037] Exception stack(0xdf82de48 to 0xdf82de90)
    [    1.950347] de40:                   00000000 00010000 00000001 00000000 df97f9c0 c041f584
    [    1.958984] de60: 00000150 00000000 60000013 df8846c0 00000013 df881608 00000000 df82de90
    [    1.967590] de80: c022fdf0 c00815f4 40000013 ffffffff
    [    1.972930] [<c0032af4>] (__irq_svc+0x34/0xa0) from [<c00815f4>] (__setup_irq+0x180/0x344)
    [    1.981658] [<c00815f4>] (__setup_irq+0x180/0x344) from [<c0081888>] (request_threaded_irq+0xd0/0x128)
    [    1.991485] [<c0081888>] (request_threaded_irq+0xd0/0x128) from [<c034c744>] (smsc911x_drv_probe+0x484/0xce4)
    [    2.001922] [<c034c744>] (smsc911x_drv_probe+0x484/0xce4) from [<c025c850>] (platform_drv_probe+0x18/0x1c)
    [    2.012115] [<c025c850>] (platform_drv_probe+0x18/0x1c) from [<c025b5a0>] (driver_probe_device+0x98/0x1a4)
    [    2.022308] [<c025b5a0>] (driver_probe_device+0x98/0x1a4) from [<c025b738>] (__driver_attach+0x8c/0x90)
    [    2.032226] [<c025b738>] (__driver_attach+0x8c/0x90) from [<c025adfc>] (bus_for_each_dev+0x60/0x8c)
    [    2.041748] [<c025adfc>] (bus_for_each_dev+0x60/0x8c) from [<c025a6d8>] (bus_add_driver+0xa0/0x228)
    [    2.051300] [<c025a6d8>] (bus_add_driver+0xa0/0x228) from [<c025bd2c>] (driver_register+0x78/0x13c)
    [    2.060852] [<c025bd2c>] (driver_register+0x78/0x13c) from [<c0008530>] (do_one_initcall+0x94/0x164)
    [    2.070495] [<c0008530>] (do_one_initcall+0x94/0x164) from [<c00086a8>] (kernel_init+0x74/0x118)
    [    2.079742] [<c00086a8>] (kernel_init+0x74/0x118) from [<c0033818>] (kernel_thread_exit+0x0/0x8)
    [    2.089019] handlers:
    [    2.091400] [<c02976c8>] smsc911x_irqhandler
    [    2.095916] Disabling IRQ #336
    [    2.100738] smsc911x-mdio: probed

Если "довольно необычно" работают такие низкоуровневые вещи, то как насчет простого юзерспейса?
Как ни странно, всё работает: мир обновляется, **ssh** пускает, сейчас **mc** дособерется. Но иногда...

.. code-block:: bash

    slyfox@igepv2 ~ $ tftp 192.168.1.5
    tftp> get uImage
    *** buffer overflow detected ***: tftp terminated
    Aborted

Что с него взять, embedded :]
