---
title: flash u-boot to sheevaplug
date: August 7, 2010
---

Потиху обновляю софт, который стоит на недавно вдруг появившейся у меня
[Marvell `sheevaplug`](http://en.wikipedia.org/wiki/SheevaPlug).
Последним в списке глючного старья был форк `u-boot` от Marvell.

Как оказалось, официально <!--more--> `u-boot`
[нифига не поддерживает](http://www.denx.de/wiki/view/DULG/CanUBootBeConfiguredSuchThatItCanBeStartedInRAM)
передачу управления с одного `u-boot` на другой. Не хотелось флешить
заведомо неработающий `u-boot`.
Но на некоторых бордах это (запись `u-boot` в память и последующий его запуск)
возможно даже без изменения исходников `u-boot`,
[здесь](http://www.openplug.org/plugwiki/index.php/Setting_Up_OpenOCD_Under_Linux)
описано как это сделать.

У меня сразу это дело не получилось, по-этому подробно опишу как это сделал я.

1. поставил `USE="ftdi usb" =dev-embedded/openocd-9999` на мою хост машину,
  с которой проводятся все удалённые действия.
2. собрал на `sheevaplug` `u-boot` ([всё должно быть просто](http://www.plugcomputer.org/plugwiki/index.php/Das_U-boot_plug_support))
3. присоединился к `sheevaplug` через `minicom` и перезагрузил ее в `u-boot`
  (нужно нажать какую-нибудь клавишу, пока она не начала грузить ядро)
4. с хост машины запустил:

~~~~
# это один терминал. Присоединяемся в sheevaplug по JTAG
$ sudo openocd -f /usr/share/openocd/scripts/board/sheevaplug.cfg
~~~~

~~~~
# другой терминал
$ nc localhost 4444
Open On-Chip Debugger
> halt
target state: halted
target halted in ARM state due to debug-request, current mode: Supervisor
cpsr: 0x400000d3 pc: 0x0060a5d4
MMU: disabled, D-Cache: disabled, I-Cache: enabled
> sheevaplug_init
sheevaplug_init
Halt timed out, wake up GDB.
timed out while waiting for target halted
Command handler execution failed
in procedure 'wait_halt' called at file "/usr/share/openocd/scripts/board/sheevaplug.cfg", line 30
in procedure 'sheevaplug_init'
~~~~

  Тут `sheevaplug` начинает грузиться и ее надо быро стопить, пока она не загрузила ядро.
  Налицо полная херь (может это баг `openocd`, фиг его знает). По сему я сделал чуть по-другому:

~~~~
# всё тот же другой терминал (тем временем sheevaplug висит в промпте u-boot)
$ nc localhost 4444
Open On-Chip Debugger
> reset
reset
JTAG tap: feroceon.cpu tap/device found: 0x20a023d3 (mfg: 0x1e9, part: 0x0a02, ver: 0x2)
# ловим промпт u-boot в minicom
> sheevaplug_init
sheevaplug_init
target state: halted
target halted in ARM state due to debug-request, current mode: Supervisor
cpsr: 0x000000d3 pc: 0xffff0000
MMU: disabled, D-Cache: disabled, I-Cache: disabled
~~~~

  `target state: halted` <- ура. После сборки `u-boot`  у нас есть несколько файлов:

  - `u-boot.kwb` - образ `u-boot`, флэшабельный в `nand`
  - `u-boot` - `elf`-образ с символами. По нем очень интересно лазить
    дизассемблером и отладчиком. Его мы сейчас и загрузим в наш девайс

~~~~
# обязательно нужно задавать абсолютный путь,
# иначе openocd будет вести себя очень неадекватно
> load_image /home/slyfox/u-boot/u-boot
load_image /home/slyfox/u-boot/u-boot
330616 bytes written at address 0x00600000
downloaded 330616 bytes in 2.880506s (112.087 KiB/s)
> resume 0x00600000
resume 0x00600000
~~~~

  Ловим `sheevaplug` в `minicom` и можем проверить базовую работоспособность:

~~~~
Marvell>> version
U-Boot 2010.03-00176-g42f7128 (Aug 06 2010 - 22:24:34)
Marvell-Sheevaplug
# грузим ядро (оно у меня уже в NAND, в принципе его тоже можно записать в память или NAND через JTAG)
Marvell>> nand read 0x01000000 0x00100000 0x00400000

NAND read: device 0 offset 0x100000, size 0x400000
 4194304 bytes read: OK

# параметры ядру
Marvell>> setenv bootargs console=ttyS0,115200 root=/dev/sda3 rw rootwait
# boot
Marvell>> bootm 0x01000000
~~~~

  То есть вся проблема - словить `sheevaplug` в состоянии `target state: halted`.
  Я этого сразу не просёк и писал `u-boot` пока `sheevaplug` работает. Пока я грузил
  туда образ нового загрузчика оригинальный сканил `NAND` и вылетал со страшными
  ошибками, так как я его затирал находу.

5. флешим сам загрузчик из уже загруженной операционки.

~~~~
sh-4.0# cd /boot/
sh-4.0# ls
System.map-2.6.35      config-2.6.35.old    vmlinuz-2.6.35
System.map-2.6.35.old  lost+found           vmlinuz-2.6.35.old
boot                   u-boot.kwb-20100806
config-2.6.35          uImage-2.6.35

sh-4.0# cat /proc/mtd 
dev:    size   erasesize  name
mtd0: 00100000 00020000 "u-boot"
mtd1: 00400000 00020000 "uImage"
mtd2: 1fb00000 00020000 "root"

# mtd0 - наш NAND кусок с u-boot, туда и пишем:
sh-4.0# flash_eraseall /dev/mtd0; nandwrite -p /dev/mtd0 /boot/u-boot.kwb-20100806
sh-4.0# /sbin/reboot
# перезагружаемся и перепроверяем, что всё хорошо
~~~~

Мне не хотелось флэшить `u-boot` из него-же самого через `TFTP`, потому что там надо
было всякие выравнивания высчитывать. Не хотелось ошибиться. Можно было загрузить/записать
новый образ и с `ext2`/`fat`.
