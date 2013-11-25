---
title: memcpy strikes again
date: April 16, 2011
---

:PostID: 138
:Title: memcpy strikes again
:Keywords: btrfs, linux, gentoo, memcpy, usermode linux, oops, bisect
:Categories: notes

За последнее время насилия над **btrfs** я умудрился завалить ее до
*kernel oops* `ненормальное количество раз <https://slyfox.ath.cx/btrfs/>`_.

На реальной машине получать отладочную информацию при падении ядра довольно
напряжно - нужно перезагружаться, так что я решил таки (снова) осилить
`usermode linux <http://user-mode-linux.sourceforge.net/>`_.

.. raw:: html

   <!--more-->

**usermode linux** - особым образом модифицированное ядро **linux**, которое собирается
как обычный исполняемый **ELF** файл. Его можно запустить непривилегированному пользователю
и отлаживать как обычный процесс: аттачиться **gdb**, вставлять **printk**, пересобирать и
тут же видеть результат без перезагрузок.

Особо это дело полезно при разработке файловых систем, распределителей памяти, и прочих штук,
которые сильно не привязаны к периферии (типа **RTC** или контроллеров прерывания).

Реализован **usermode linux** как отдельная архитектура в основном дереве ядра **linux-2.6**.
Собрать его можно так:

.. code-block:: bash

    $ ARCH=um SUBARCH=i386 make <обычные параметры>

В результате получается обычный исполняемый файл **vmlinux**. У него много всяких
параметров, которые задают внешние блочные устройства, настройки сети, размер ОЗУ гостя и прочее.

Пользовательские процессы запускаются там как потоки от процесса **vmlinux** (самого ядра).
Они отслеживаются **vmlinux** через **ptrace**, чтобы перехватывать:

- системные вызовы (чтобы они не уходили в хост ядро напрямую)
- исключения (падения, невалидные инструкции и прочее)

и обрабатывать их. Прерывания внутри самого процесса **vmlinux** реализованы через **posix** сигналы.

Одна из интересных особенностей **um** - монтирование файловой системы хоста.
То есть не нужно создавать **loopback** устройств, чтобы содать корневую ФС.
Хватит просто создать пару файлов в одном из пользовательских каталогов.

Мой скрипт для запуска **um** такой:

.. code-block:: bash

    #!/bin/sh
    ./vmlinux                                        \
        ubd0=$(pwd)/btr.img                          \
        root=/dev/root                               \
        rootflags="$(pwd)/root" rw rootfstype=hostfs \
        mem=256M init=/init                          \
                                                     \
        "$@"


В **"$(pwd)/root"** у меня лежит статически собранный **busybox** и минимальный
скрипт инициализации. Структура каталогов выглядит так:

.. code-block:: bash

    $ tree -a
    .
    |-- btr.img
    |-- root
    |   |-- bin
    |   |   `-- busybox
    |   |-- dev
    |   |-- etc
    |   |   |-- fstab
    |   |   `-- mtab -> ../proc/mounts
    |   |-- init
    |   |-- proc
    |   `-- sys
    |-- run
    `-- vmlinux -> ../linux-2.6-um-x86_64/vmlinux

.. code-block:: bash

    $ cat root/etc/fstab
    none            /               hostfs          defaults        1       1
    none            /proc           proc            defaults        0       0
    sysfs           /sys            sysfs           defaults        0       0
    tmpfs           /dev            tmpfs           defaults        0       0

.. code-block:: bash

    $ cat root/init
    #!/bin/busybox sh
    mount /proc
    mount /dev
    mount /sys
    mdev -s # populate /dev
    exec /bin/busybox sh

Такое окружение легко редактировать из хоста (даже когда гость работает).
**/init** мотирует **/proc**, **/sys**, **/dev** и запускает **sh**. Всё просто.

**root/init** зупукасется процессом **vmlinux**, который не требует(!) **root** привилегий.

При сборке **um** я нарвался на проблему:
ARCH=um `не собирался <http://www.spinics.net/lists/mm-commits/msg83315.html>`_, когда в конфиге ядра использовался **CONFIG_SLUB=y**

Разобравшись с ней я нарвался на падение **btrfs** в **um**. Меня удивило, что всё вроде
работает, но как только я пытаюсь возиться с **btrfs** данные на ней портятся практически сразу.

Я позадавал глупых вопросов в списке рассылки `usermode-linux <http://www.mail-archive.com/user-mode-linux-devel@lists.sourceforge.net/msg07085.html>`_
и начал штурмовать `btrfs-devel <http://thread.gmane.org/gmane.comp.file-systems.btrfs/10075>`_ и их **IRC** канал.

Кстати, падение ядра по **BUG_ON** не выводило красивого **backtrace** с номером строки.
Теперь `выводит <http://us.generation-nt.com/patch-1-2-um-fix-call-tracer-bug-handler-help-202910422.html>`_ :].

Мне посоветовали найти работающаю версию ядра с **btrfs** в **um** и `забисектить </2009/06/27/git-bisect/>`_ это дело.
Бисект "не самой популярной" архитектуры - дело непростое. Результат или не собирался или падал и без попытки
использования **btrfs**. Сначала я думал, что такие ситуации - редкость, и просто скипал (**git bisect skip**) кривые
состояния, но начиная с какого-то момента я не мог ничего собрать, а оставалось **3000** ревизий.

Я понял, что попал и начал **bisect** заново. Перед этим я выгреб все коммиты, относяшиеся к **um**, чтобы
заставить работать каждое состояние ядра:

.. code-block:: bash

    linux-2.6 $ git log --oneline --abbrev-commit --no-merges v2.6.36..v2.6.37 -- arch/um/
    69e83da uml: disable winch irq before freeing handler data
    451a3c2 BKL: remove extraneous #include <smp_lock.h>
    8818b67 um: fix ptrace build error
    0a3d763 ptrace: cleanup arch_ptrace() on um
    9b05a69 ptrace: change signature of arch_ptrace()
    be76d81 um: migrate from __do_IRQ() to generic_handle_irq()
    aa5fb4d uml: fix CONFIG_STATIC_LINK=y build failure with newer glibc
    d911202 uml: define CONFIG_NO_DMA
    ece0e2b mm: remove pte_*map_nested()
    dbec921 um: fix IRQ flag handling naming
    6915e04 um: remove PAGE_SIZE alignment in linker script causing kernel segfault.
    482db6d um: fix global timer issue when using CONFIG_NO_HZ
    9a181c5 uml: kill big kernel lock
    6038f37 llseek: automatically add .llseek fop
    d1ea13c genirq: Cleanup irq_chip->typename leftovers
    838a2e5 kbuild: migrate all arch to the kconfig mainmenu upgrade

Их не так много, но было подозрение, что один из них и является виновником.
Некоторые коммиты фиксили зависания (NO_HZ), некоторые - вылеты на старте (__do_IRQ),
некоторые - сборку из-за переименований (NO_DMA, ptrace, IRQ).

В этот раз я довольно быстро забисектил до `коммита с memcpy <http://git.kernel.org/?p=linux/kernel/git/torvalds/linux-2.6.git;a=commitdiff;h=59daa7>`_!
(плавно перешли к теме :]).

Учитывая, что сейчас почти **2.6.39**, а коммит аж из бородатого **2.6.37**,
то надо полагать, что **memcpy** работает. Вывод один - **btrfs** неправильно его использует.

На свете не так много способов лохануться с **memcpy**. Я предположил самый популярный:
область источника и назначения перекрываются. А как это проверить?

Сначала я думал пропатчить сам **memcpy** или написть для **um** свою реализацию (это легко сделать для
конкретной архитектуры), но почему-то забил на это.

Вместо этого написал свою **memcpy_debug** и с помощью **sed** заменил все **memcpy** в **fs/btrfs/** на свою:

.. code-block:: c

    #include <linux/kernel.h>
    #include "memcpy_debug.h"
    /*
     * Chew all the files this way:
     * 1. for f in *; do sed -i "$f" -e 's/\<memcpy\>/memcpy_debug/g'; done
     * 2. copy this file here
     * 3. spill #include "memcpy_debug.h" where needed
     * 4. add memcpy_debug.o to 'btrfs-y +=' in Makefile
     * 5. add '#include "memcpy_debug.h"' to popular headers, like 'ctree.h' and 'compression.h'
     */
    void * memcpy_debug(void * dst, const void * src, __kernel_size_t size)
    {
        char       * c_dst = dst;
        const char * c_src = src;
        __kernel_size_t delta;
        if (c_dst < c_src)
                delta = c_src - c_dst;
        else
                delta = c_dst - c_src;
        if (delta < size)
        {
            printk(KERN_CRIT" memcpy overlap detected: memcpy(dst=%p, src=%p, size=%lu) [delta=%lu]\n", dst, src, (long unsigned)size, (long unsigned)delta);
        }
        WARN_ON(delta < size);
        return memcpy(dst, src, size);
    }

Использовал **WARN_ON**, так как **BUG_ON** Richard еще не пофиксил :]. Я сразу нашел проблемное место:

.. code-block::

     memcpy overlap detected: memcpy(dst=0000000070654e8a, src=0000000070654ea9, size=171) [delta=31]
    ------------[ cut here ]------------
    WARNING: at /home/slyfox/linux-2.6/fs/btrfs/memcpy_debug.c:18 btrfs_memcpy+0x52/0x68()
    Call Trace:
    7064b748:  [<600eff46>] map_extent_buffer+0x62/0x9e
    7064b758:  [<60029ad9>] warn_slowpath_common+0x59/0x70
    7064b798:  [<60029b05>] warn_slowpath_null+0x15/0x17
    7064b7a8:  [<6011129e>] btrfs_memcpy+0x52/0x68
    7064b7d8:  [<600efa01>] memcpy_extent_buffer+0x18d/0x1da
    7064b858:  [<600efae2>] memmove_extent_buffer+0x94/0x208
    7064b8d8:  [<600bc4b0>] setup_items_for_insert+0x2b8/0x426
    7064b8e8:  [<600bb25a>] btrfs_leaf_free_space+0x62/0xa6
    7064b9c8:  [<600c13f3>] btrfs_insert_empty_items+0xa3/0xb5
    7064ba38:  [<600ce690>] insert_with_overflow+0x33/0xf1
    7064ba88:  [<600ce7d4>] btrfs_insert_dir_item+0x86/0x268
    7064bae8:  [<601b498b>] _raw_spin_unlock+0x9/0xb
    7064bb48:  [<600ddef1>] btrfs_add_link+0x10d/0x170
    7064bbc8:  [<600ddf7a>] btrfs_add_nondir+0x26/0x52
    7064bc08:  [<600de73f>] btrfs_create+0xf2/0x1c0
    7064bc18:  [<6007ccff>] generic_permission+0x57/0x9d
    7064bc68:  [<6007cf60>] vfs_create+0x6a/0x75

Как мы видим, сначала вызывается какая-то там **memmove_extent_buffer**, а из нее **memcpy_extent_buffer**.

Даже не зная, что такое **extent**, можно догадаться (экспериментально доказано, что можно),
что где-то в 'memmove_' неправильно определяется пересечение областей и вызывается **memcpy_** на
перекрывающиеся области. И пока никто не опомнился я это дело быстро зафиксил и заслал патч :].

Фиксы на все 3 проблемы уже почти у Линуса.
В одном из них `я - автор \\o/ <http://git.kernel.org/?p=linux/kernel/git/mason/btrfs-unstable.git;a=commitdiff;h=3387206f26e1b48703e810175b98611a4fd8e8ea>`_ ! :]

Проблема неправильного использования **memcpy** - серьезная штука. Я бы не хотел потерять данные из-за
какой-то микрооптимизации **memmove** в **memcpy**. Думаю добавить в ядро отключаемую на стадии компиляции
возможность проверки перекрытия областей в саму **memcpy** (секция **Kernel hacking** в **menuconfig**).

Самое смешное, что я начал копаться в **usermode linux** не из-за того, что у меня было повреждение данных,
а потому, что я неправильно пропатчил **btrfs-progs** и создавал заведомо сломанные файловые системы, которые
валили ядро (правда не сразу, а после того, как я их заполню).

Но проблема с **memcpy** повторялась и на нормальных **btrfs-progs**. Кругом случайности.
