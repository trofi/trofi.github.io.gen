---
title: profiling your boot
date: July 8, 2012
---

Как-то недавно выпиливая очередной патч для `btrfs` я наткнулся на
[сообщение одного
сэра](http://www.mail-archive.com/linux-btrfs@vger.kernel.org/msg16042.html)
о проблеме, которую я только-только
[пофиксил](http://www.mail-archive.com/linux-btrfs@vger.kernel.org/msg16045.html).

Меня заинтересовала его картинка и я решил запилить себе такую-же. Это
оказалось не только красиво, но и познавательно.

Как всегда, всё просто как грабли:

``` bash
emerge app-benchmarks/bootchart2
```

Для более детальной статистики `emerge` попросит включить в ядре
всякие счётчики, трейсеры и проберы для более детальной инфы. В первый
раз можно не париться.

Для `grub` добавляюм строку с `init=/sbin/bootchartd`:

```
title Gentoo Linux
    root (hd0,0)
    kernel /boot/vmlinuz root=/dev/sda2 init=/sbin/bootchartd
```

Для `grub2`:

```
menuentry 'Gentoo Linux' {
    insmod part_msdos
    insmod ext2
    set root='hd0,msdos1'
    linux /boot/vmlinuz root=/dev/sda2 init=/sbin/bootchartd
}
```

Перезагружаемся и получаем лог загрузки в `/var/log/bootchart.tgz`.

Вся статистика собирается в `tmpfs` и сбрасывается по завершению
указанного процесса в `/etc/bootchartd.conf`:

``` bash
# The processes we have to wait for
EXIT_PROC="agetty mgetty mingetty"
```

Можно что-нибудь более высокоуровневое впилить, если хочется посмотреть
на загрузку `KDE`/`GNOME` частей.

После загрузки можно сгенерить красивый график. Мы сгенерим аж 2 из них:

``` bash
$ pybootchartgui && mv bootchart.png bootchart-default.png
$ pybootchartgui --show-all && mv bootchart.png bootchart-all.png
$ pybootchartgui -i # а тут можно руками потыкать в gui приложении
```

Картинки потерял, но по ним было видно видно, что:

- юзерспейс получает управление на `6` секунде.
- больше всего ввода-вывода кушает процесс `systemd-udev`, запущенный
  на `12` секунде (грузит модуля, пробит железо, пинает
  `usb_modeswitch` и прочее)
- первые `agetty`/`login` стартанули на `47` секунде. Это и
  есть время загрузки.
- `alsactl` вытворяет очень много ввода-вывода (скорее всего просто
  первая грузит кучу левых либ, с которыми она слинкована)
- удаление всяких временных файлов занимает аж `4` секунды (у меня
  всегда помойка в `/tmp`)
- в детальном выводе видны даже аргументы, с которыми запускается каждая
  программа при загрузке (крайне познавательно!)

Have fun!
