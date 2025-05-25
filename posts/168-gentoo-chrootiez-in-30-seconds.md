---
title: gentoo chrootiez in 30 seconds
date: March 8, 2012
---

Сегодня получил доступ к `ppc64` коробке. Решил причесать ворох
хаков в отдельный проект и забросил на [`gihtub` как
`gentoo-chrootiez`](https://github.com/trofi/gentoo-chrootiez).

[Тут](https://github.com/trofi/gentoo-chrootiez/blob/master/HOWTO)
наиподробнейшая последовательность действий, чтобы развернуть
`chroot`.

Работает не только для `gentoo`, но в `gentoo` не нужно делать
вообще ничего, чтобы получить `rootfs`. Распаковали `stage3` - и
можно заходить.

Пример лога для развёртывания нового окружения:

``` bash
~ $ mkdir chrootiez
~ $ cd chrootiez/
~/chrootiez $ wget http://distfiles.gentoo.org/releases/alpha/current-stage3/stage3-alpha-20120303.tar.bz2
~/chrootiez $ mkdir alpha-stable
~/chrootiez $ cd alpha-stable/
~/chrootiez/alpha-stable $ sudo tar xvjpf ../stage3-alpha-20120303.tar.bz2
~/chrootiez/alpha-stable $ cd ..
~/chrootiez $ git clone git://github.com/trofi/gentoo-chrootiez
~/chrootiez $ cat >alpha-stable.sh <<EOF
#!/bin/sh
sudo gentoo-chrootiez/scripts/run_chroot.sh alpha-stable as-is "\$@"
EOF
~/chrootiez $ chmod +x alpha-stable.sh
# Тут начинается gentoo-специфичная дурь:
~/chrootiez $ cd gentoo-chrootiez/bound/
~/chrootiez/gentoo-chrootiez/bound $ ./make_typical_binds.sh
~/chrootiez/gentoo-chrootiez/bound $ ls -l
    drwxr-xr-x  conf
    lrwxrwxrwx  distfiles -> /usr/portage/distfiles
    -rwxr-xr-x  make_typical_binds.sh
    lrwxrwxrwx  portage -> /usr/portage
    -rw-r--r--  README
# Создали ссылки на portage и distfiles
# Я еще обычно добавляю разные оверлеи:
~/chrootiez/gentoo-chrootiez/bound $ ln -s ~/overlays/gentoo-haskell gentoo-haskell
~/chrootiez/gentoo-chrootiez/bound $ cd ../..
#
# Заходим в chroot:
~/chrootiez $ ./alpha-stable.sh
/ > echo 'source /bound/conf/make.conf' >> /etc/make.conf
/ > eselect profile set 1
/ > USE=binary emerge -1 ghc
```

Готово!

Кровываые детали кроются в двух маленьких скриптах.
