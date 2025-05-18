---
title: alpha journey
date: August 11, 2011
---

Сегодня решил проверить, не починили ли сборку на `ghc` альфе. [Патч
Саймона](https://github.com/gentoo-haskell/gentoo-haskell/blob/master/dev-lang/ghc/files/ghc-7.2.1-fix-exotic-unreg-builds.patch)
окончательно всё исправил, и теперь у нас есть надежда на `ghc-7.2` с
полным составом поддерживаемых архитектур.

На радостях я решил посмотреть, как там жизнь на `alpha` и увидел, что
демон `nrpe` забивает `dmesg` невыровненным доступом:

```
$ dmesg
...
do_entUnaUser: 2 callbacks suppressed
nrpe(1185): unaligned trap at 000002000011ab94: 00000001200401ec 2d 31
nrpe(1185): unaligned trap at 000002000011ab94: 00000001200401f4 2d 31
nrpe(1185): unaligned trap at 000002000011ab94: 00000001200401fc 2d 31
nrpe(1185): unaligned trap at 000002000011ab94: 0000000120040204 2d 31
nrpe(1185): unaligned trap at 000002000011ab94: 000000012004020c 2d 31
```

Решил попробовать исправить.
[Здесь](http://www.gentoo.org/proj/en/base/alpha/doc/alpha-porting-guide.xml#doc_chap3)
прочитал про хак с включением `SIGBUG` на `unaligned access` через
очень странный недокументированный и неэкспортируемый `glibc`
системный вызов `osf_setsysinfo`.

Естественно, программа не работала. Результат - [патч в
ядро](http://marc.info/?l=linux-alpha&m=131307612327236&w=2). Не знаю,
помогает или нет (ядро грузнуть негде), но найти ошибку заняло
минут 20 таращенья в `git log`. `mattst88` подсказал, что для поиска
таких штук еще есть `prctl` (и я дописал упоминание о нем в док).

На `arm` я `unaligned access` искал с помощью обычных `breakpoints` в
`gdb`:

```
$ gdb -p $pid
break *(0x${pc-of-faulty-insn})
continue
```

Тут это не сработало из-за того, что `nrpe`
постоянно форкающийся, снижающий привилегии маложивущий демон. Пришлось
немного повозиться, чтобы вычислить, что ошибка в `openssl`. Правда, я
ее еще не до конца отследил.

Зато нашел баг в `strace`. [Патч в
апстриме](http://strace.git.sourceforge.net/git/gitweb.cgi?p=strace/strace;a=commitdiff;h=1a53e34).

**UPDATE:**

Баг таки выловил и их там оказалось даже 2! Даже патч послал, но апстрим
[оказался быстрее](http://cvs.openssl.org/chngview?cn=21233).
