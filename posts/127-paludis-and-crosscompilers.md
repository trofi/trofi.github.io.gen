---
title: paludis and crosscompilers
date: November 24, 2010
---

Когда-то давным-давно у меня был `Pentium III`. У меня была мечта собрать на нем ядро
`linux` для архитектуры `amd64` и попробовать загрузить его в `qemu-system-x86_64`.

Для этого мне нужен был компилятор, который собирает 64-битные бинарники на 32-битной системе.
Даже будучи бывшим `lfs`ником сборка кросскомпилятора меня немного пугала.

В `gentoo` кросскомпиляторы собираются на удивление просто средствами `dev-util/crossdev`:

~~~~ { .shell }
$ crossdev mingw32 # соберет mingw32-gcc и mingw32-g++
$ crossdev x86_64-pc-linux-gnu # соберет x86_64-pc-linux-gnu-gcc и x86_64-pc-linux-gnu-g++
~~~~

Само ядро собирается так-же просто, как и в `native` среде. Единственное различие - надо
явно задавать кросспрефикс и архитектуру:

~~~~ { .shell }
$ ARCH=amd64 CROSS=x86_64-pc-linux-gnu- make menuconfig
$ ARCH=amd64 CROSS=x86_64-pc-linux-gnu- make
~~~~

Смотрим, что же там нам поставил `crossdev`:

~~~~ { .shell }
$ echo 'int main() { return 0; }' > test.c
$ mingw32-gcc test.c -o test.exe
$ file test.exe
test.exe: PE32 executable for MS Windows (console) Intel 80386 32-bit
$ x86_64-pc-linux-gnu-gcc test.c -o test.elf64
$ file test.elf64
test.elf64: ELF 64-bit LSB shared object, x86-64, version 1 (GNU/Linux), dynamically linked (uses shared libs), for GNU/Linux 2.6.9, not stripped
~~~~

Итак, простые бинарники мы собирать готовы.

Всё было бы хорошо, если бы `crossdev` не был прибит гвоздями к `emerge`.

Что же делать пользователям `paludis`? Самый простой вариант - разобраться что именно
делает `crossdev` и воссоздать такую-же среду.

Грубо говоря сборка любого кросскомпилятора проходит в 5 этапов:

1. Устанавливается `binutils` (`assembler`, `linker`)
2. Устанавливаются заголовочные файлы ядра и бибилиотеки `С` (или их эквиваленты) для `target` системы:
   `stddef.h`, `windows.h` (я не шучу! :]).
3. собирается `gcc` (только компилятор `C`), который в состоянии генерить _только_
   промежуточные объектные файлы. Исполнять их пока нельзя - нет `crt`, с которой надо слинковать эти файлы.
4. собирается библиотека `С` в каком-то ее виде, которая предоставляет `crt startup code` (код,
   который исполняется до `main()`).
5. Собирается компилятор C, который уже может собирать полноценные программы (а им уже `С++` и далее)

Открываем `/usr/portage/sys-devel/crossdev/files/crossdev` и видим примерно такой порядок действий:

~~~~ { .shell }
## настройка окружения:
CTARGET=mingw32
cross="cross-${CTARGET}"
...

## собственно сборка (фазы в той же последовательности, как описаны выше)
# 1.
                                             emerge $cross/binutils
# 2.
CROSSCOMPILE_OPTS="headers-only"             emerge $cross/w32api
CROSSCOMPILE_OPTS="headers-only"             emerge $cross/mingw-runtime
# 3.
CROSSCOMPILE_OPTS="bootstrap" USE="-* nocxx" emerge $cross/gcc
# 4.
CROSSCOMPILE_OPTS=""                         emerge $cross/w32api
# 5.
USE="${LUSE} ${USE}" CROSSCOMPILE_OPTS=""    emerge $cross/mingw-runtime
~~~~

Концептуально всё просто :]

Итак, теперь конкретная пошаговая инструкция для пользователей `paludis`:

1. Создадим и наполним оверлей `cross-repo`:

~~~~ { .shell }
# создаем
$ cat /etc/paludis/repositories/cross-repo.conf
master_repository = gentoo
format = e
location = /home/slyfox/portage/cross/

$ cat /etc/paludis/repository_defaults.conf
builddir = /var/tmp/paludis
# DISTFILES у меня абыгде
distdir = /mnt/archive/distfiles
names_cache = /var/cache/paludis/names
write_cache = /var/cache/paludis/metadata

# наполняем
$ cd $HOME/portage/cross
$ LANG=C tree
.
|-- cross-mingw32
|   |-- binutils -> /usr/portage/sys-devel/binutils
|   |-- gcc -> /usr/portage/sys-devel/gcc
|   |-- gdb -> /usr/portage/sys-devel/gdb
|   |-- mingw-runtime -> /usr/portage/dev-util/mingw-runtime
|   `-- w32api -> /usr/portage/dev-util/w32api
`-- profiles
    |-- categories
    `-- repo_name

# 2 файла и 5 симлинков. Создать их можно так:
$ mkdir -p profiles
$ echo cross-mingw32 >> profiles/categories
for i in w32api mingw-runtime
  do ln -s /usr/portage/dev-util/$i cross-mingw32/$i
done
for i in gcc gdb binutils
  do ln -s /usr/portage/sys-devel/$i cross-mingw32/$i
done
~~~~

2. Сконфигурим окружение для `paludis`.
   Нам нужно задавать особое значение для `$CTARGET` (эта переменная указывает на то, для какой
   платформы будет генерить бинарники получившийся кросскомпилятор).
   Для этого добавим следующую штуку в `/etc/paludis/bashrc`:

~~~~ { .shell }
   case "${CATEGORY}" in
    cross-*)
        ABI=cross
        LIBDIR_cross=lib
        CFLAGS_cross=
        CPPFLAGS_cross=
        CXXFLAGS_cross=
        LDFLAGS_cross=

        CBUILD="${CHOST}"
        CTARGET="${CATEGORY#cross-}"

        case "${PN}" in
            binutils|gcc|gdb)
                :
            ;;
            *)  # a lot of packages don't know what the crosscompilation is
                CC="${CTARGET}-gcc"
                CXX="${CTARGET}-g++"
                LD="${CTARGET}-ld"
            ;;
        esac
    ;;
esac
~~~~

   Выглядит длинно, но пугаться не надо. Тут мы вырезаем всякие артефакты, которые пытаются выпрагнуть из
   нашей текущей системы (*_cross: `multilib ABI` и `CTARGET`) и явно заменяем `CC`, `CXX` и `LD` для всех, кто в `cross-`
   категории. Это надо для программ, которые не понимают `--host`/`--build`/`--target` (обычно такие программы
   просто не используют `autoconf`).

3. Выставляем переменные окружения для сборки:

~~~~ { .shell }
$ cat /etc/paludis/use.conf.d/cross-mingw32.conf

cross-mingw32/* crosscompile_opts: headers-only
cross-mingw32/gcc -* nocxx
~~~~

4. Ставим `binutils`, хедеры, огрызок `libc` и так называемый `gcc-quick` (из шага `3.` выше):

~~~~ { .shell }
$ paludis -i cross-mingw32/{binutils,w32api,mingw-runtime,gcc}
~~~~

5. Убираем `bootstrap` переменные. Они нам больше не понадобятся - обновление
   `binutils` и `gcc` их не потребует:

~~~~ { .shell }
$ cat /etc/paludis/use.conf.d/cross-mingw32.conf

#### cross-mingw32/* crosscompile_opts: headers-only
#### cross-mingw32/gcc -* nocxx

cross-mingw32/gcc -nls -gcj -gtk -mudflap
~~~~

6. Собираем полноценный кросскомпилятор:

~~~~ { .shell }
$ paludis -i cross-mingw32/{w32api,mingw-runtime,gcc}
~~~~

7. Тестируем:

~~~~ { .shell }
$ cd ~/.wine/drive_c # :]
$ cat > a.c <<EOF
#include <stdio.h>
int main()
{
    printf ("hello!\n");
    return 0;
}
EOF

$ mingw32-gcc a.c -o a.exe
$ wine a.exe
hello!
~~~~

Готово! :]

Ссылки, которые помогали мне забороть это дело:

- [`paludis` + `avr`, старьё](http://en.gentoo-wiki.com/wiki/Paludis/AVR_Crossdev)
- [`mingw` и `emerge`](http://www.gentoo-wiki.info/MinGW)
- [почти работающее решение `epheminet`](http://ephemient.livejournal.com/51870.html)
- [как я лоханулся :)](https://bugs.gentoo.org/show_bug.cgi?id=346469)

**UPDATE:**

Надо же и на обычный `linux` target `gcc` скроссить. Возьмем `armv5tel-softfloat-linux-gnueabi`
(в аппаратном исполнении их уже есть у меня).

1. Продолжаем забивать симлинками наш оверлей:

~~~~
$ cd $HOME/portage/cross

$ echo cross-armv5tel-softfloat-linux-gnueabi >> profiles/categories
ln -s /usr/portage/sys-kernel/linux-headers cross-armv5tel-softfloat-linux-gnueabi/linux-headers
ln -s /usr/portage/sys-libs/glibc           cross-armv5tel-softfloat-linux-gnueabi/glibc

for i in gcc gdb binutils
  do ln -s /usr/portage/sys-devel/$i cross-armv5tel-softfloat-linux-gnueabi/$i
done
~~~~

2. `bashrc` у `paludis` уже был сконфигурен.

3. Готовимся к `stage1` и выставляем минимальные USE:

~~~~ { .shell }
$ cat /etc/paludis/use.conf.d/cross-armv5tel-softfloat-linux-gnueabi.conf

cross-armv5tel-softfloat-linux-gnueabi/* crosscompile_opts: headers-only
cross-armv5tel-softfloat-linux-gnueabi/gcc -* nocxx
~~~~

4. [Опять] Ставим `binutils`, хедеры, огрызок `libc` и так называемый `gcc-quick` (из шага `3.` выше):

~~~~ { .shell }
$ paludis -i cross-armv5tel-softfloat-linux-gnueabi/{binutils,linux-headers,glibc,gcc}
~~~~

5. Убираем **bootstrap** переменные. Они нам больше не понадобятся - обновление
   **binutils** и **gcc** их не потребует:

~~~~ { .shell }
$ cat /etc/paludis/use.conf.d/cross-mingw32.conf

#### cross-armv5tel-softfloat-linux-gnueabi/* crosscompile_opts: headers-only
#### cross-armv5tel-softfloat-linux-gnueabi/gcc -* nocxx

cross-armv5tel-softfloat-linux-gnueabi/gcc -nls -gcj -gtk -mudflap
~~~~

6. Собираем полноценный кросскомпилятор:

~~~~ { .shell }
$ paludis -i cross-armv5tel-softfloat-linux-gnueabi/glibc
$ paludis -i cross-armv5tel-softfloat-linux-gnueabi/gcc
~~~~

7. Тестируем:

~~~~ { .shell }
$ cat > a.c <<EOF
#include <stdio.h>
int main()
{
    printf ("hello!\n");
    return 0;
}
EOF

$ armv5tel-softfloat-linux-gnueabi-gcc a.c -o a.elf
# утягиваем на target:
$ uname -m
armv5tel
$ ./a.elf
hello!
~~~~

Всё работает :]
