---
title: LD_PRELOAD and multiple libraries
date: October 8, 2011
---

:PostID: 154
:Title: LD_PRELOAD and multiple libraries
:Keywords: LD_PRELOAD, chaining, gentoo, sandbox, linux
:Categories: notes

Иногда нужно запустить какую-то конкретную программу так, чтобы
одна конкретная функция работала в ней не так, как обычно.

Примеры, когда это может понадобиться:

- перегрузить функцию открытия файла (**open()**) так,
  чтобы вести отчёт открывавшихся программой файлов
- перегрузить функцию открытия сокетов (**connect()**) так,
  чтобы прозрачно для программы авторизоваться на **HTTP** (или **SCOKS**) прокси.
  Такие проги даже есть на свете: `tsocks <http://tsocks.sourceforge.net/>`_ и `transconnect <http://transconnect.sourceforge.net/>`_.
  Было время, когда я пользовался ими каждый день :]

Попробуем перегрузить функцию **exit()** в **glibc**.
Большинство **UNIX** позволяют принудительно вгрузить какую-то стороннюю библиотеку
в адресное пространство запускаемой программы через глобальную переменную **LD_PRELOAD**.

Попробуем простой пример:

.. code-block:: C

    #define _GNU_SOURCE
    #include <stdlib.h>
    #include <stdio.h>
    #include <unistd.h>
    #include <dlfcn.h>
    void exit(int status)
    {
        void (*real_exit)(int status);
        *(void **)&real_exit = dlsym (RTLD_NEXT, "exit");
        fprintf (stderr, "<<<%s:Morning sir! I pass you to the %p function>>>\n", W, real_exit);
        real_exit (status);
    }

.. code-block:: bash

    $ cc -g -O0 -fPIC -DW='"w1"' main.c -o stat_wrapper1.so -shared -ldl
    $ cc -g -O0 -fPIC -DW='"w2"' main.c -o stat_wrapper2.so -shared -ldl
    $ LD_PRELOAD="./stat_wrapper1.so ./stat_wrapper2.so" ls .
        main.c  makefile  stat_wrapper1.so  stat_wrapper2.so
        <<<w1:Morning sir! I pass you to the 0x7fba1919a67c function>>>
        <<<w2:Morning sir! I pass you to the 0x7fba18a38150 function>>>

Работает :]

В **gentoo** при сборке пакетов используется утилита **sandbox**, которая перехватывает функции
обращения к файловой системе и ругается, когда при записи билд вылазит за дозволенные границы:

.. code-block:: bash

    $ /bin/bash -c 'echo 1 > foo'
    $ sandbox /bin/bash -c 'echo 1 > foo'
        /bin/bash: foo: Permission denied
    $ sandbox /bin/bash -c 'echo 1 > /tmp/foo' # а в /tmp/ - можно :]

Но внутри **sandbox** **LD_PRELOAD** не работает.
