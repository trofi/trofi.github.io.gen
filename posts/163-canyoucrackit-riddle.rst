---
title: canyoucrackit riddle
date: December 4, 2011
---

:PostID: 163
:Title: canyoucrackit riddle
:Keywords: canyoucrackit, simple, fun, spoiler, riddle, cyberwin, a3bfc2af, d2ab1f05, da13f110, 15b436de1f9107f3778aad525e5d0b20, da75370fe15c4148bd4ceec861fbdaa5, hqDTK7b8K2rvw
:Categories: notes

Вчера на ночь глядя получил от Антоши ссылку на `загадку <http://www.canyoucrackit.co.uk/>`_.
Обычно я таким даже не пытаюсь заниматься, ибо сложные :].

Но в этот раз я проявил настойчивость и хакнул его. Попробуйте
разломать это чудо сами. Даже я осилил!.

Дальше идёт один большой спойлер :].

.. raw:: html

   <!--more-->

**Первая стадия.**

Всё начинается с **PNG** файла на `главной странице < http://www.canyoucrackit.co.uk/>`_.
**eb**, **e8** и **90** подсказали, что это шестнадцатиричный дамп 32-битного или 16-битного
варианта **IA-32** инструкций.

Самый простой вариант это дело проверить - вбить первых пару байт в любимый редактор с дизассемблером.
Я кроме **hteditor** ничего не знаю, так что юзаем его :].

.. code-block:: asm

    00000000 eb04                           jmp         0x6
    00000002 af                             scasd
    00000003 c2bfa3                         ret         a3bf
    00000006 81ec00010000                   sub         esp, 0x100
    0000000c 31c9                           xor         ecx, ecx
    0000000e 880c0c                         mov         [esp+ecx], cl
    00000011 fec1                           inc         cl
    00000013 75f9                           jnz         0xe
    00000015 31c0                           xor         eax, eax
    00000017 baefbeadde                     mov         edx, deadbeef

Первый прыжок пропускает что-то странное, а дальше идёт (немножко понятный!) код.

Я понял, что забивать все 160 символов вручную мне будет лень и я загуглил первых 10 байт.
Я тут-же получил текстовую форму представления на картинке и написал прожку на "C",
чтобы ее можно было изучать и патчить в **hteditor**.

.. code-block:: c

    // gcc -m32 -O2 -fomit-frame-pointer -nostdlib stage1.c -o stage1
    static char pseudostack[1024 + 256];
    int _start()
    {
        void * stack_bottom = (void*)&pseudostack[sizeof (pseudostack)];
        asm volatile("movl %0, %%esp" : : "r"(stack_bottom) : "esp", "memory");
        asm volatile(".byte 0xeb, 0x04, 0xaf, 0xc2, 0xbf, 0xa3, 0x81, 0xec, 0x00, 0x01, 0x00, 0x00, 0x31, 0xc9, 0x88, 0x0c\n");
        asm volatile(".byte 0x0c, 0xfe, 0xc1, 0x75, 0xf9, 0x31, 0xc0, 0xba, 0xef, 0xbe, 0xad, 0xde, 0x02, 0x04, 0x0c, 0x00\n");
        asm volatile(".byte 0xd0, 0xc1, 0xca, 0x08, 0x8a, 0x1c, 0x0c, 0x8a, 0x3c, 0x04, 0x88, 0x1c, 0x04, 0x88, 0x3c, 0x0c\n");
        asm volatile(".byte 0xfe, 0xc1, 0x75, 0xe8, 0xe9, 0x5c, 0x00, 0x00, 0x00, 0x89, 0xe3, 0x81, 0xc3, 0x04, 0x00, 0x00\n");
        asm volatile(".byte 0x00, 0x5c, 0x58, 0x3d, 0x41, 0x41, 0x41, 0x41, 0x75, 0x43, 0x58, 0x3d, 0x42, 0x42, 0x42, 0x42\n");
        asm volatile(".byte 0x75, 0x3b, 0x5a, 0x89, 0xd1, 0x89, 0xe6, 0x89, 0xdf, 0x29, 0xcf, 0xf3, 0xa4, 0x89, 0xde, 0x89\n");
        asm volatile(".byte 0xd1, 0x89, 0xdf, 0x29, 0xcf, 0x31, 0xc0, 0x31, 0xdb, 0x31, 0xd2, 0xfe, 0xc0, 0x02, 0x1c, 0x06\n");
        asm volatile(".byte 0x8a, 0x14, 0x06, 0x8a, 0x34, 0x1e, 0x88, 0x34, 0x06, 0x88, 0x14, 0x1e, 0x00, 0xf2, 0x30, 0xf6\n");
        asm volatile(".byte 0x8a, 0x1c, 0x16, 0x8a, 0x17, 0x30, 0xda, 0x88, 0x17, 0x47, 0x49, 0x75, 0xde, 0x31, 0xdb, 0x89\n");
        asm volatile(".byte 0xd8, 0xfe, 0xc0, 0xcd, 0x80, 0x90, 0x90, 0xe8, 0x9d, 0xff, 0xff, 0xff, 0x41, 0x41, 0x41, 0x41\n");
        return 0;
    }

Немного черной магии для установки **esp** в статическую область памяти (чтобы было прощее ее исследовать).
Дальше до запуска опасного бинаря я решил изучить всю программу целиком (а вдруг там троян? :]).
Дальше я буду приводить код с адресами в **ELF** файле (я не осилил заставить **hteditor** переименовывать
метки в обычных бинарных данных).

Прога состоит из нескольких частей:

.. code-block:: asm

    80480c7 ! eb04                             jmp         skip_something
    80480c9   af                               scasd
    80480ca   c2bfa3                           ret         0a3bfh

Пропуск какого-то подозрительного мусора.

.. code-block:: asm

    ....... ! skip_something:                 ;xref j80480c7
    ....... ! 81ec00010000                     sub         esp, 100h
    80480d3 ! 31c9                             xor         ecx, ecx
    80480d5 !
    ....... ! init_table:                     ;xref j80480da
    ....... ! 880c0c                           mov         [esp+ecx], cl
    80480d8 ! fec1                             inc         cl
    80480da ! 75f9                             jnz         init_table

Выделение на стеке 256 байт и инициализация их значениями **0x00-0xFF**.

.. code-block:: asm

    80480dc ! 31c0                             xor         eax, eax
    80480de ! baefbeadde                       mov         edx, 0deadbeefh
    80480e3 !
    ....... ! permutate_table:                ;xref j80480f9
    ....... ! 02040c                           add         al, [esp+ecx]
    80480e6 ! 00d0                             add         al, dl
    80480e8 ! c1ca08                           ror         edx, 8
    80480eb ! 8a1c0c                           mov         bl, [esp+ecx]
    80480ee ! 8a3c04                           mov         bh, [esp+eax]
    80480f1 ! 881c04                           mov         [esp+eax], bl
    80480f4 ! 883c0c                           mov         [esp+ecx], bh
    80480f7 ! fec1                             inc         cl
    80480f9 ! 75e8                             jnz         permutate_table
    80480fb ! e95c000000                       jmp         trampoline

Перестановка некоторых байт местами. Первый байт - **cl**, второй выбирается
по ключу **0xdeadbeef** и **add/ror**. Главное, что код пока пасётся в
своих 256 байтах, никуда не вылзит.

Последняя инструкция уводит нас в конец кода (я назвал его **trampoline**).
Рассмотрим сначала его:

.. code-block:: asm

    ....... ! trampoline:                     ;xref j80480fb
    ....... ! 90                               nop
    804815d ! 90                               nop
    804815e ! e89dffffff                       call        body
    8048163 ! 41414141                         dd          41414141h

Из необычного: впервые используется **call** (и только однажды!).
Вернуться ему пока некуда (дальше только конец программы).

Первая часть **body** - очень интересная.

.. code-block:: asm

    ....... ! body:                           ;xref c804815e
    ....... ! 89e3                             mov         ebx, esp
    8048102 ! 81c304000000                     add         ebx, 4
    8048108 ! 5c                               pop         esp
    8048109 ! 58                               pop         eax ; 1
    804810a ! 3d41414141                       cmp         eax, 41414141h
    804810f ! 7543                             jnz         bad_signature
    8048111 ! 58                               pop         eax ; 2
    8048112 ! 3d42424242                       cmp         eax, 42424242h
    8048117 ! 753b                             jnz         bad_signature
    8048119 ! 5a                               pop         edx
    804811a ! 89d1                             mov         ecx, edx
    804811c ! 89e6                             mov         esi, esp
    804811e ! 89df                             mov         edi, ebx
    8048120 ! 29cf                             sub         edi, ecx
    8048122 ! f3a4                             repz movsb
    8048124 ! 89de                             mov         esi, ebx
    8048126 ! 89d1                             mov         ecx, edx
    8048128 ! 89df                             mov         edi, ebx
    804812a ! 29cf                             sub         edi, ecx
    804812c ! 31c0                             xor         eax, eax
    804812e ! 31db                             xor         ebx, ebx
    8048130 ! 31d2                             xor         edx, edx

Здесь со стека восстанавливается адрес возврата (который указывает на **0x41414141**)
и сохраняется в **esp**. По этому адресу последовательно считываются:

- сигнатура **0x41414141** (**pop eax ; 1**, есть в нашем образе)
- сигнатура **0x42424242** (**pop eax ; 2**, дальше ничего нет в образе - где-то надо найти)
- размер блока (**pop edx**)
- сам блок (копируется **edx** байт через **rep movsb** в область перед нашей таблицей)

Уже весело! Код привязался к данным в сегменте кода, которых нет на картинке. Посмотрим,
что делает остальной код.

.. code-block:: asm

    ....... ! decrypt_input:                  ;xref j8048152
    ....... ! fec0                             inc         al
    8048134 ! 021c06                           add         bl, [esi+eax]
    8048137 ! 8a1406                           mov         dl, [esi+eax]
    804813a ! 8a341e                           mov         dh, [esi+ebx]
    804813d ! 883406                           mov         [esi+eax], dh
    8048140 ! 88141e                           mov         [esi+ebx], dl
    8048143 ! 00f2                             add         dl, dh
    8048145 ! 30f6                             xor         dh, dh
    8048147 ! 8a1c16                           mov         bl, [esi+edx]
    804814a ! 8a17                             mov         dl, [edi]
    804814c ! 30da                             xor         dl, bl
    804814e ! 8817                             mov         [edi], dl
    8048150 ! 47                               inc         edi
    8048151 ! 49                               dec         ecx
    8048152 ! 75de                             jnz         decrypt_input

Опять видим **xor** и перестановку байт свежевычитанного блока с нашей таблицей.

.. code-block:: asm

    ....... ! bad_signature:                  ;xref j804810f j8048117
    ....... ! 31db                             xor         ebx, ebx
    8048156 ! 89d8                             mov         eax, ebx
    8048158 ! fec0                             inc         al
    804815a ! cd80                             int         80h

**eax = 1; int 0x80** - это системный вызов **exit** в **linux**.

Выводы:

- Код не проверяет своей целостности и никак не привязан к байтам, которые он в себе содержит.
  Его можно **несчадно патчить** на предмет вывода результата дешифровки :]
- Код не пытается выводить результатов своей работы - они остаются в памяти по адресу
  **esp - 256 - edx** от начального адреса.

Тут я пошел спать, так как не знал где взять данные на расшифровку. Утром меня осенило:
неспроста код выложен на `картинке <http://www.canyoucrackit.co.uk/images/cyber.png>`_
(а не в текстовом или бинарном виде).
Заглянув в нее текстовым редактором в глаза бросается **base64** строка в секции коментариев:

.. code-block:: bash

    QkJCQjIAAACR2PFtcCA6q2eaC8SR+8dmD/zNzLQC+td3tFQ4qx8O447TDeuZw5P+0SsbEcYR78jKLw==

.. code-block:: bash

    $ printf "QkJCQjIAAACR2PFtcCA6q2eaC8SR+8dmD/zNzLQC+td3tFQ4qx8O447TDeuZw5P+0SsbEcYR78jKLw==" | base64 -d > stage1.data
    $ hexdump -C stage1.sata
    00000000  42 42 42 42 32 00 00 00  91 d8 f1 6d 70 20 3a ab  |BBBB2......mp :.|
    00000010  67 9a 0b c4 91 fb c7 66  0f fc cd cc b4 02 fa d7  |g......f........|
    00000020  77 b4 54 38 ab 1f 0e e3  8e d3 0d eb 99 c3 93 fe  |w.T8............|
    00000030  d1 2b 1b 11 c6 11 ef c8  ca 2f                    |.+......./|
    0000003a

Видим магическую сигнатуру **0x42424242** и судя по всему размер блока - **0x32** байта.

Дописываем этот блок в конец функции **trampoline** сразу после **0x41414141**
и заменяем **int 80** на **int 3**, чтобы в **core dump увидеть** чего там нарасшифровалось :]

.. code-block:: c

    // gcc -m32 -O2 -fomit-frame-pointer -nostdlib stage1.c -o stage1
    static char pseudostack[1024 + 256];
    int _start()
    {
        void * stack_bottom = (void*)&pseudostack[sizeof (pseudostack)];
        asm volatile("movl %0, %%esp" : : "r"(stack_bottom) : "esp", "memory");
        asm volatile(".byte 0xeb, 0x04, 0xaf, 0xc2, 0xbf, 0xa3, 0x81, 0xec, 0x00, 0x01, 0x00, 0x00, 0x31, 0xc9, 0x88, 0x0c\n");
        asm volatile(".byte 0x0c, 0xfe, 0xc1, 0x75, 0xf9, 0x31, 0xc0, 0xba, 0xef, 0xbe, 0xad, 0xde, 0x02, 0x04, 0x0c, 0x00\n");
        asm volatile(".byte 0xd0, 0xc1, 0xca, 0x08, 0x8a, 0x1c, 0x0c, 0x8a, 0x3c, 0x04, 0x88, 0x1c, 0x04, 0x88, 0x3c, 0x0c\n");
        asm volatile(".byte 0xfe, 0xc1, 0x75, 0xe8, 0xe9, 0x5c, 0x00, 0x00, 0x00, 0x89, 0xe3, 0x81, 0xc3, 0x04, 0x00, 0x00\n");
        asm volatile(".byte 0x00, 0x5c, 0x58, 0x3d, 0x41, 0x41, 0x41, 0x41, 0x75, 0x43, 0x58, 0x3d, 0x42, 0x42, 0x42, 0x42\n");
        asm volatile(".byte 0x75, 0x3b, 0x5a, 0x89, 0xd1, 0x89, 0xe6, 0x89, 0xdf, 0x29, 0xcf, 0xf3, 0xa4, 0x89, 0xde, 0x89\n");
        asm volatile(".byte 0xd1, 0x89, 0xdf, 0x29, 0xcf, 0x31, 0xc0, 0x31, 0xdb, 0x31, 0xd2, 0xfe, 0xc0, 0x02, 0x1c, 0x06\n");
        asm volatile(".byte 0x8a, 0x14, 0x06, 0x8a, 0x34, 0x1e, 0x88, 0x34, 0x06, 0x88, 0x14, 0x1e, 0x00, 0xf2, 0x30, 0xf6\n");
        asm volatile(".byte 0x8a, 0x1c, 0x16, 0x8a, 0x17, 0x30, 0xda, 0x88, 0x17, 0x47, 0x49, 0x75, 0xde, 0x31, 0xdb, 0x89\n");
        //                                          v - here it was 0x80
        asm volatile(".byte 0xd8, 0xfe, 0xc0, 0xcd, 0x03, 0x90, 0x90, 0xe8, 0x9d, 0xff, 0xff, 0xff, 0x41, 0x41, 0x41, 0x41\n");
        // base64-decoded from cyber.png
        asm volatile(".byte  0x42, 0x42, 0x42, 0x42, 0x32, 0x00, 0x00, 0x00, 0x91, 0xd8, 0xf1, 0x6d, 0x70, 0x20, 0x3a, 0xab\n");
        asm volatile(".byte  0x67, 0x9a, 0x0b, 0xc4, 0x91, 0xfb, 0xc7, 0x66, 0x0f, 0xfc, 0xcd, 0xcc, 0xb4, 0x02, 0xfa, 0xd7\n");
        asm volatile(".byte  0x77, 0xb4, 0x54, 0x38, 0xab, 0x1f, 0x0e, 0xe3, 0x8e, 0xd3, 0x0d, 0xeb, 0x99, 0xc3, 0x93, 0xfe\n");
        asm volatile(".byte  0xd1, 0x2b, 0x1b, 0x11, 0xc6, 0x11, 0xef, 0xc8, 0xca, 0x2f\n");
        return 0;
    }

В блоке **pseudostack** видим заветную строку:

.. code-block:: c

    "GET /15b436de1f9107f3778aad525e5d0b20.js HTTP/1.1"

**Вторая стадия.**

Идём по `ссылке <http://www.canyoucrackit.co.uk/15b436de1f9107f3778aad525e5d0b20.js>`_ и попадаем
на второе задание.

Узнаем, что это

.. code-block:: C

    // stage 2 of 3


Эта стадия очень простая. Она напомнила мне о `ICFPC 2006 <http://boundvariable.org/task.shtml>`_,
где используется чуть более сложная виртуальная машина.

Сначала я реализовал всё, как написано и удивлялся почему же оно падает
сразу после цикла расшифровки. Потом дошло, что спека немного глючная:

.. code-block:: diff

    --- stage2.js   2011-09-26 11:47:24.000000000 +0300
    +++ stage2_fixed.js     2011-12-04 00:30:54.037239149 +0300
    @@ -118,13 +118,13 @@
         // 
         // opcode | instruction | operands (mod 0) | operands (mod 1)
         // -------+-------------+------------------+-----------------
    -    // 0x00   | jmp         | r1               | r2:r1
    -    // 0x01   | movr        | r1, r2           | rx,   imm
    +    // 0x00   | jmp         | r1               | imm:r1
    +    // 0x01   | movr        | r1, r2           | r1,   imm
         // 0x02   | movm        | r1, [ds:r2]      | [ds:r1], r2
         // 0x03   | add         | r1, r2           | r1,   imm
         // 0x04   | xor         | r1, r2           | r1,   imm
         // 0x05   | cmp         | r1, r2           | r1,   imm
    -    // 0x06   | jmpe        | r1               | r2:r1
    +    // 0x06   | jmpe        | r1               | imm:r1
         // 0x07   | hlt         | N/A              | N/A
         //
         // flags

Второй операнд должен трактоваться как новое значение сегмента кода,
а не номер регистра, в котором он хранится.

Я написал эмулятор на **С**. С дебагом исходник весит 11KB. Интерфейсов общения со
внешним миром у виртуальной машины тоже нет - это видно из ее набора инструкций:

.. code-block:: C

    // opcode | instruction | operands (mod 0) | operands (mod 1)
    // -------+-------------+------------------+-----------------
    // 0x00   | jmp         | r1               | imm:r1
    // 0x01   | movr        | r1, r2           | r1,   imm
    // 0x02   | movm        | r1, [ds:r2]      | [ds:r1], r2
    // 0x03   | add         | r1, r2           | r1,   imm
    // 0x04   | xor         | r1, r2           | r1,   imm
    // 0x05   | cmp         | r1, r2           | r1,   imm
    // 0x06   | jmpe        | r1               | imm:r1
    // 0x07   | hlt         | N/A              | N/A

Выводы:

- Переменная **firmware: [0xd2ab1f05, 0xda13f110]** не понадобилась - неспроста.
- результат опять должен содержаться в памяти.

Запускаем эмулятор и дампим память. Видим заветную строку:

.. code-block:: c

    "GET /da75370fe15c4148bd4ceec861fbdaa5.exe HTTP/1.0"

**Третья стадия.**

Идём по `ссылке <http://www.canyoucrackit.co.uk/15b436de1f9107f3778aad525e5d0b20.js>`_ и попадаем
на последнее задание.

Это исполняемый PE файл, у которого в зависимостях библиотеки **cygwin**: **cygwin1.dll** и **cygcrypt-0.dll**.
Я поставил **cygwin** с оффсайта и в опциях выбрал **crypt** к дополнительной установке.

Подробное дизассемблирование я приводить не буду. Расскажу только пару моментов:

- бинарник собран **gcc** без оптимизаций (много дохлого кода)
- точка входа - **__main** в **cygwin**, но указатель на юзерский код передается
  прозрачно в **main()**:

.. code-block:: asm

    ...... ! entrypoint:
    ...... !   push        ebp
    401001 !   mov         ebp, esp
    401003 !   sub         esp, 18h
    401006 !   and         esp, 0fffffff0h
    401009 !   mov         dword ptr [esp], user_code ; вот он!
    401010 !   call        to_crt

- видим, что прога принимает 1 аргумент **hostname**
- пытается открыть файл 'license.txt'
- проверяет первые 4 байта 'license.txt' на сигнатуру **gchq**
- вычитывает строку(**key**) из 8 байт и применяет функцию **crypt()** с **salt=hqDTK7b8K2rvw** (алгоритм хэширования **DES**)
- требует, чтобы **crypt(key, "hqDTK7b8K2rvw") == "hqDTK7b8K2rvw"**
- требует каких-то 3 32-битных числа намекая, что они были в предыдущих двух уровнях.

Мне **DES** ломать было лень, точнее я не осилил **johntheripper**.
В теории файла с содержимым:

.. code-block:: bash

    hello:hqDTK7b8K2rvw

должно было хватить, но, видать - пароль длинноват (позже я выгуглил, что **key="cyberwin"**).
Я просто пропатчил фигов бинарь там, где он проверяет равенство на **strcmp**:

.. code-block:: asm

    401167 !   cmp         dword ptr [ebp-38h], 71686367h
    40116e !   jnz         invalid_license
    401170 !   mov         eax, [salt_indir]
    401175 !   mov         [esp+4], eax
    401179 !   lea         eax, [ebp-38h]
    40117c !   add         eax, 4
    40117f !   mov         [esp], eax
    401182 !   call        crypt_wrapper
    401187 !   mov         edx, eax
    401189 !   mov         eax, [salt_indir]
    40118e !   mov         [esp+4], eax
    401192 !   mov         [esp], edx
    401195 !   call        strcmp_wrapper
    40119a !   test        eax, eax
    40119c !   jnz         hash_mismatch ; от тут мы его и прищучим
    40119e !   mov         dword ptr [ebp-0ch], 1
    4011a5 !
    ...... ! hash_mismatch:                  ;xref j40119c

Меняем **jnz** на **jz** и суем в файл мусорок:

.. code-block:: bash

    printf "gchqhelloworld" > license.txt; wine da75370fe15c4148bd4ceec861fbdaa5.exe canyoucrackit.co.uk
    keygen.exe
    loading stage1 license key(s)...loading stage2 license key(s)...request:GET /hqDTK7b8K2rvw/646c/0/0/key.txt HTTP/1.0response:HTTP/1.1 404 Not FoundContent-Type: text/html; charset=us-asciiServer: Microsoft-HTTPAPI/2.0Date: Sat, 03 Dec 2011 21:53:17 GMTConnection: closeContent-Length: 315<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN""http://www.w3.org/TR/html4/strict.dtd"><HTML><HEAD><TITLE>Not Found</TITLE><META HTTP-EQUIV="Content-Type" Content="text/html; charset=us-ascii"></HEAD><BODY><h2>Not Found</h2><hr><p>HTTP Error 404. The requested resource is not found.</p></BODY></HTML>

Урлик, по кторому нам говорят, что мы уже почти у цели.
Но фигурируют 3 магические цифры для форматной строки (хорошо видно в бинаре):

.. code-block:: C

    "GET /%s/%x/%x/%x/key.txt HTTP/1.0"

В дизассембледном дампе видно, что после **loading stage1 license key(s)...**
вычитывается одно 32-битное число, а после **loading stage2 license key(s)...**
еще два.

Во второром задании было 2 магических числа - это неиспользуемый **firmware: [0xd2ab1f05, 0xda13f110]**.
В первом задании странный 4-байтный мусор перепрыгивался первой же инструкцией **jmp**:

.. code-block:: asm

    00000000 eb04                           jmp         0x6
    00000002 af                             scasd
    00000003 c2bfa3                         ret         a3bf

Логично предположить, что **0xa3bfc2af** - это и есть первый ключ.

Итого наш ключ для патченного бинаря (**12345678** можно заменить на любые 8 символов кроме **cyberwin** :]):

.. code-block:: C

    printf "gchq12345678\xaf\xc2\xbf\xa3\x05\x1f\xab\xd2\x10\xf1\x13\xda" > license.txt; wine da75370fe15c4148bd4ceec861fbdaa5.exe canyoucrackit.co.uk

и для непатченного бинаря:

.. code-block:: C

    printf "gchqcyberwin\xaf\xc2\xbf\xa3\x05\x1f\xab\xd2\x10\xf1\x13\xda" > license.txt; wine da75370fe15c4148bd4ceec861fbdaa5.exe canyoucrackit.co.uk

Запуск выплёвывает нам заветную строку:

.. code-block:: C

    "GET /hqDTK7b8K2rvw/a3bfc2af/d2ab1f05/da13f110/key.txt HTTP/1.0"

Идём по ссылке в браузере (сам бинарь нам нагло врёт и выводит **not found**):

.. code-block:: C

    Pr0t3ct!on#cyber_security@12*12.2011+

Вводим код `на главной <http://www.canyoucrackit.co.uk/>`_. Получаем следующий опус:

.. code-block:: text

    So you did it. Well done! Now this is where it gets interesting.
    Could you use your skills and ingenuity to combat terrorism and
    cyber threats? As one of our experts, you'll help protect our
    nation's security and the lives of thousands. Every day will bring
    new challenges, new solutions to find – and new ways to prove that
    you're one of the best.
    [Find out more and apply]

Такие пироги :]
