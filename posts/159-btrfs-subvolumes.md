---
title: btrfs subvolumes
date: November 7, 2011
---

Немного порассуждаем о явлении фрагментации во всяких файловых системах
и как их можно избежать при разработке файловой системы и при ее
использовании.

Фрагментация бывает
[внутренней](http://en.wikipedia.org/wiki/Fragmentation_%28computer%29#Internal_Fragmentation)
(несоответствие выделенного блока на диске реально занимаемым данным) и
[внешней](http://en.wikipedia.org/wiki/Fragmentation_%28computer%29#External_Fragmentation)
(непоследовательное расположение данных на диске).
Внутренняя фрагментация полностью определяется механизмом выделения
блоков под данные на уровне архитектуры файловой системы.
В `FAT32`/`ext2`/`ext3` это блоки постоянного размера (причем их число
ограничено), так что средняя внутренняя фрагментация - пол блока на
файл.

Реальные числа можно рассматривать через прогу `filefrag`:

```
$ /usr/sbin/filefrag -v /boot/vmlinuz # ext2
Filesystem type is: ef53
Filesystem cylinder groups is approximately 35
File size of /boot/vmlinuz is 2476176 (2419 blocks, blocksize 1024)
 ext logical physical expected length flags
   0       0    55412              12 merged
   1      12    55425    55423    256 merged
   2     268    55683    55680    256 merged
   3     524    55940    55938    256 merged
   4     780    56197    56195    256 merged
   5    1036    56454    56452    256 merged
   6    1292    56711    56709    256 merged
   7    1548    56968    56966    256 merged
   8    1804    57225    57223    120 merged
   9    1924    57857    57344    136 merged
  10    2060    57994    57992    256 merged
  11    2316    58251    58249    103 merged,eof
/boot/vmlinuz: 12 extents found
$ filefrag -v /usr/lib/ghc-7.0.4/hsc2hs # ext4
Filesystem type is: ef53
File size of /usr/lib/ghc-7.0.4/hsc2hs is 8852016 (2162 blocks, blocksize 4096)
 ext logical physical expected length flags
   0       0  2414592            2048
   1    2048  2413894  2416639    114 eof
/usr/lib/ghc-7.0.4/hsc2hs: 2 extents found
```

Чем меньше размер блока - тем меньше внутренняя фрагментация, но тем
больше нужно хранить данных об этих блоках (так как блоков нужно всё
больше).

Одно из решений - использовать блоки переменного размера. В `ext4`
используются блоки, кратные базовому:

```
# e2freefrag /dev/sda2
Device: /dev/sda2
Blocksize: 4096 bytes
Total blocks: 12207391
Free blocks: 3951711 (32.4%)
Min. free extent: 4 KB 
Max. free extent: 1346200 KB
Avg. free extent: 2896 KB
Num. free extent: 8766
HISTOGRAM OF FREE EXTENT SIZES:
Extent Size Range :  Free extents   Free Blocks  Percent
    4K...    8K-  :          2268          2268    0.06%
    8K...   16K-  :          1733          3999    0.10%
   16K...   32K-  :          1379          7037    0.18%
   32K...   64K-  :           872          9102    0.23%
   64K...  128K-  :           620         13654    0.35%
  128K...  256K-  :           511         22690    0.57%
  256K...  512K-  :           315         28042    0.71%
  512K... 1024K-  :           223         39064    0.99%
    1M...    2M-  :           200         74682    1.89%
    2M...    4M-  :           176        128285    3.25%
    4M...    8M-  :           182        276171    6.99%
    8M...   16M-  :            91        257498    6.52%
   16M...   32M-  :            50        277533    7.02%
   32M...   64M-  :            60        673687   17.05%
   64M...  128M-  :            47       1036845   26.24%
  128M...  256M-  :            24       1060240   26.83%
  256M...  512M-  :             5        432677   10.95%
  512M... 1024M-  :             9       1669131   42.24%
    1G...    2G-  :             1        336550    8.52%
```

В `btrfs` и `reiserfs` на минимальный размер блоков вообще не
накладывается никаких ограничений.

```
# /usr/sbin/filefrag -v /usr/lib/ghc-7.0.4/hsc2hs # btrfs
Filesystem type is: 9123683e
File size of /usr/lib/ghc-7.0.4/hsc2hs is 1155640 (283 blocks, blocksize 4096)
 ext logical physical expected length flags
   0       0 70825707             283 eof
/usr/lib/ghc-7.0.4/hsc2hs: 1 extent found
```

В традиционных файловых системах есть 2 операции, изменяющая
фрагментацию:

- запись в конец файла (`append`, `open(file,"a")`)
- сокращение размера (`truncation`)

Чем больше размер блока - тем реже нам нужно выделять новые блоки для
увеличения файла (особенно актуально для файлов журнала, которые
изменяются часто и понемногу).

В нетрадиционных `CoW` (aka. `copy-on-write`) файловых системах
(`btrfs`, `zfs`, возможно, `reiser4`) любая модификация
содержимого файла ведет к тому, что эта модификация записывается в новое
место на диске, а оригинальные данные некоторое время остаются на старом
месте (хоть и помечаются неиспользуемыми). То есть эффекта перезаписи
данных нет.

`CoW` можно выключить в `btrfs` для:

- отдельных файлов установив `nodatacow`:
  [чудопрогой](http://dave.jikos.cz/nocow.c) ([она же, но пакованная в
  `gentoo`](http://repo.or.cz/w/slyfox-gentoo.git/commitdiff/fd978ba60981b940b155722121e877c3dde7d216))
- всей файловой системы: `mount -o nodatacow`.

В `btrfs` `CoW` семантика обновлений изменений на файловой системе
проявляется не только при обновлении содержимого **файла**, но и **при
изменени каталогов** (добавление/удаление файла, смена атрибутов файлов
и т.п.).

Это приводит к тому, что метаданные, хранящиеся в каталоге расползаются
по диску даже при обновлении существующих записей о элементах каталогов
(даже без добавления новых). Метаданные каталогов могут обновляться по
разным причинам:

- `mtime`/`ctime`/`atime` - модификация метаданных, данных и времени
  доступа к файлу
- добавление файлов/подкаталогов

К счастью в `btrfs` есть `on-line` дефрагментатор. Он копирует весь
файл в другую область на диске. Юзать так: `btrfs fi de /bin/busybox`
(или `btrfs filesystem defragment /bin/busybox`).

Но дефрагментировать можно и каталог. Не все файлы в нём (`defrag`
этого не умеет) а именно `метаданные` каталога. Тут нужно представить
что же именно дефрагментируется.

В случае дефрагментации **файла** всё просто:

- данные файла - это дерево экстентов (блоков переменной длины). На
  каждый файл в `btrfs` заведено по отдельному дереву.
- создается новое дерево экстентов, в него заносятся новые блоки (в
  идеале 1 большой блок)
- новое дерево экстентов вписывается в метаданные каталога (атомарная
  операция, на которой дефрагментированный файл становится "виден"
  остальным)

С каталогом веселее:

- дерево каталогов - одно на всю файловую систему (о `subvolumes` чуть
  позже). Используется особая реализация **B-дерева**, которая не
  требует обновления всех ссылок до родителя на диске.
- Дефрагментация каталога - это перебалансировка поддерева **B-дерева**,
  в которое входит наш каталог
- Из-за особенностей обновления **B-дерева** в **B**-поддереве (не
  поддереве файловой системы) нашего каталога может находиться **Что
  Угодно**. Например, `/usr/portage` и `/var/tmp` могут (и скорее
  всего будут) сидеть в соседних узлах, если их часто обновляют.

В итоге в процессе использования файловой часто изменяемые каталоги
"размешиваются" по всей остальной редко изменяемой файловой системе.

Вчера `btrfs fi defrag /usr/portage` у меня заняла **40 минут**. Я бы
быстрее выкачал и перераспаковал новый снапшот.

Но есть средство, которое позволяет разбить файловую систему на
несколько независимых **B-деревьев** - `subvolumes`. Немного можно
читануть
[тут](http://www.funtoo.org/wiki/BTRFS_Fun#..._applied_to_BTRFS.21_.28or_what_is_a_volume.2Fsubvolume.29).

Вкратце про `subvolumes`:

- это самодостаточная файловая система (ее можно отдельно монтировать:
  `mount /some/device -osubvolume=subvol_name /mnt/foo`)
- `subvolumes` атомарно:
  - создаются (`btrfs subvolume create`)
  - копируются (`btrfs subvolume snapshot`)
  - удаляются (`btrfs subvolume delete`)
  - переключаются (`btrfs subvolume set-default`). Очень прикольная
    штука. Можно иметь несколько корней для разных дистрибутивов на
    одном разделе.

Я создал `/usr/portage` и `/var/tmp` как отдельные `subvolumes` и
теперь не парюсь фрагментацией в них.

```
# cd /
# mkdir subvolumes
# btrfs su cr gentoo-portage
# btrfs su cr gentoo-x86
# btrfs su cr var_tmp
#
# # дальше наставил симлинков, пока монтирование по сложным путям не починили :]
#
# btrfs su li /
    ID 664 top level 5 path subvolumes/gentoo-portage
    ID 666 top level 5 path subvolumes/gentoo-x86
    ID 673 top level 5 path subvolumes/var_tmp
# ls -ld /var/tmp /gentoo/gentoo-x86
    lrwxrwxrwx 1 root root 22 Ноя  6 21:34 /gentoo/gentoo-x86 -> /subvolumes/gentoo-x86
    lrwxrwxrwx 1 root root 19 Ноя  6 21:44 /var/tmp -> /subvolumes/var_tmp
```

Правда, всплыл небольшой косяк в ядре, который не позволяет монтировать
`subvolumes`, которые находятся не в корне `default subvolume`, но
[это было легко
исправить](http://www.mail-archive.com/linux-btrfs@vger.kernel.org/msg13284.html)
с помощью `usermode linux` и `debugging SLUB`.

`debugging SLUB` на мой тесткейс сказал следующее:

```
device fsid 4d55aa28-45b1-474b-b4ec-da912322195e devid 1 transid 4 /dev/ubda
kobject: 'btrfs-1' (00000000712ac4a0): kobject_add_internal: parent: 'bdi', set: 'devices'
kobject: 'btrfs-1' (00000000712ac4a0): kobject_uevent_env
kobject: 'btrfs-1' (00000000712ac4a0): fill_kobj_path: path = '/devices/virtual/bdi/btrfs-1'
btrfs: disk space caching is enabled
device fsid 4d55aa28-45b1-474b-b4ec-da912322195e devid 1 transid 7 /dev/ubda
=============================================================================
BUG kmalloc-2048: Object already free
-----------------------------------------------------------------------------
INFO: Allocated in btrfs_mount+0x389/0x7f0 age=0 cpu=0 pid=277
INFO: Freed in btrfs_mount+0x51c/0x7f0 age=0 cpu=0 pid=277
INFO: Slab 0x0000000062886200 objects=15 used=9 fp=0x0000000070b4d2d0 flags=0x4081
INFO: Object 0x0000000070b4d2d0 @offset=21200 fp=0x0000000070b4a968
<ПРОПУСТИМ>
Redzone 0000000070b4dad0: bb bb bb bb bb bb bb bb                          ........
Padding 0000000070b4db10: 5a 5a 5a 5a 5a 5a 5a 5a                          ZZZZZZZZ
Call Trace: 
70b31948:  [<6008c522>] print_trailer+0xe2/0x130
70b31978:  [<6008c5aa>] object_err+0x3a/0x50
70b319a8:  [<6008e242>] free_debug_processing+0x142/0x2a0
70b319e0:  [<600ebf6f>] btrfs_mount+0x55f/0x7f0
70b319f8:  [<6008e5c1>] __slab_free+0x221/0x2d0
70b31a18:  [<6018d55f>] __debug_check_no_obj_freed+0x16f/0x1f0
70b31a58:  [<60024d90>] set_signals+0x30/0x40
70b31a90:  [<600ebf6f>] btrfs_mount+0x55f/0x7f0
70b31aa8:  [<6008fed8>] kfree+0x108/0x150
70b31ab8:  [<6022e1f9>] mutex_unlock+0x9/0x10
70b31af8:  [<600ebf6f>] btrfs_mount+0x55f/0x7f0
70b31b38:  [<60091241>] __kmalloc_track_caller+0x191/0x1b0
70b31b40:  [<600af114>] alloc_vfsmnt+0x94/0x170
70b31bc8:  [<6009663b>] mount_fs+0x1b/0xf0
70b31bf8:  [<600afebe>] vfs_kern_mount+0x5e/0xd0
70b31c48:  [<600ebb77>] btrfs_mount+0x167/0x7f0
70b31c88:  [<60091241>] __kmalloc_track_caller+0x191/0x1b0
70b31c90:  [<600af114>] alloc_vfsmnt+0x94/0x170
70b31d18:  [<6009663b>] mount_fs+0x1b/0xf0
70b31d48:  [<600afebe>] vfs_kern_mount+0x5e/0xd0
70b31d98:  [<600b0baf>] do_kern_mount+0x4f/0x110
70b31de8:  [<600b147a>] do_mount+0x40a/0x800
70b31e28:  [<600b0fb1>] copy_mount_options+0xd1/0x150
70b31e88:  [<600b1973>] sys_mount+0x93/0xe0
70b31ee8:  [<60018cb6>] handle_syscall+0x76/0x80
70b31f08:  [<600272d6>] userspace+0x386/0x490
70b31f58:  [<60023d97>] save_registers+0x17/0x40
70b31fc8:  [<600157ad>] fork_handler+0x7d/0x90
FIX kmalloc-2048: Object at 0x0000000070b4d2d0 not freed
```

Самое интересное тут следующее:

```
INFO: Allocated in btrfs_mount+0x389/0x7f0 age=0 cpu=0 pid=277
INFO: Freed in btrfs_mount+0x51c/0x7f0 age=0 cpu=0 pid=277
70b31a90:  [<600ebf6f>] btrfs_mount+0x55f/0x7f0
```

Это одна точка выделения и две точки освобождения одной и той же памяти.
Смотрим в `gdb`, куда они показывают:

```
$ gdb vmlinux.o
...
(gdb) list *(btrfs_mount+0x389c)
0x600ebd99 is in btrfs_mount (/home/slyfox/linux-2.6/fs/btrfs/super.c:937).
932              * it for searching for existing supers, so this lets us do that and
933              * then open_ctree will properly initialize everything later.
934              */
935             fs_info = kzalloc(sizeof(struct btrfs_fs_info), GFP_NOFS);
936             tree_root = kzalloc(sizeof(struct btrfs_root), GFP_NOFS);
937             if (!fs_info || !tree_root) {
938                     error = -ENOMEM;
939                     goto error_close_devices;
940             }
941             fs_info->tree_root = tree_root;
(gdb) list *(btrfs_mount+0x51c)
0x600ebf2c is in btrfs_mount (/home/slyfox/linux-2.6/fs/btrfs/ctree.h:2500).
2495    static inline void free_fs_info(struct btrfs_fs_info *fs_info)
2496    {
2497            kfree(fs_info->delayed_root);
2498            kfree(fs_info->extent_root);
2499            kfree(fs_info->tree_root);
2500            kfree(fs_info->chunk_root);
2501            kfree(fs_info->dev_root);
2502            kfree(fs_info->csum_root);
2503            kfree(fs_info->super_copy);
2504            kfree(fs_info->super_for_commit);
(gdb) list *(btrfs_mount+0x55f)
```

Я не сохранил вывод последнего, но легко видеть, что в патче это
`kfree(tree_root);` сразу после вызова `free_fs_info(fs_info)`, в
котором происходит первое освобождение: `kfree(fs_info -> tree_root);`

Стоит заметить, что адреса в `gdb` показывают на адрес возврата (т.е.
на следующую после выделения строку).
