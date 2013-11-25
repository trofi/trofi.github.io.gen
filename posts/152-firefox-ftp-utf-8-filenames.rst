---
title: firefox ftp utf-8 filenames
date: September 4, 2011
---

:PostID: 152
:Title: firefox ftp utf-8 filenames
:Keywords: firefox, ftp, utf-8
:Categories: notes

Продолжая тему `нормальных utf-8 имён в firefox </2009/07/14/firefox-url-copypaste/>`_
сегодня уконфигурим нормальное отображение **utf-8** файлов по **FTP**.

TL;DR:

**about:config**: **intl.config.default**: **windows-1251** -> **utf-8**

.. raw:: html

   <!--more-->

Скрасноглазим необычных названий в **UTF-8** кодировке.
Заходим по **FTP** и видим:

::

    Индекс «ftp://sf/inbox/fun names/»

    Файл:god kvГ¤ll
    Файл:gГјnaydД±n
    Файл:hyvГ¤Г¤ pГ¤ivГ¤Г¤
    Файл:РїСЂРµРІРµРґ

Корявенько. В **about:config** ищем **intl.config.default** и исправляем **windows-1251** на **utf-8**.

Проверяем:

::

    Индекс «ftp://sf/inbox/fun names/»

    Файл:god kväll
    Файл:günaydın
    Файл:hyvää päivää
    Файл:превед

Работает \\o/

Не все FTP сервера умеют раздавать в кодировке **UTF-8**, так
что фикс может повредить пользователям кривых/старых **FTP** серверов
(так им и надо).
Но сервера, которые реализуют `rfc-2640 <http://lftp.yar.ru/RFC2640>`_,
должны работать без проблем:

::

   - Servers MUST support the UTF-8 feature in response to the FEAT
     command [RFC2389]. The UTF-8 feature is a line containing the exact
     string "UTF8". This string is not case sensitive, but SHOULD be
     transmitted in upper case. The response to a FEAT command SHOULD
     be:

        C> feat
        S> 211- <any descriptive text>
        S>  ...
        S>  UTF8
        S>  ...
        S> 211 end

.. code-block:: bash

    $ telnet sf 21
        Trying 192.168.1.5...
        Connected to sf.
        Escape character is '^]'.
        220 sf.home
    FEAT
        211-Features:
        MDTM
         MFMT
         LANG en-US;ko-KR;zh-TW;bg-BG;it-IT;fr-FR;ru-RU;zh-CN;ja-JP
         TVFS
         UTF8
         MFF modify;UNIX.group;UNIX.mode;
         MLST modify*;perm*;size*;type*;unique*;UNIX.group*;UNIX.mode*;UNIX.owner*;
         REST STREAM
         SIZE
        211 Конец
