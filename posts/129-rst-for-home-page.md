---
title: rst for home page
date: December 15, 2010
---

Никто не любит писать руками `(x)html`, но браузеры больше ничего не понимают :].
Приходится генерить его всякими извратскими способами. Я с недавних пор
юзаю [`rst` или `reStructuredText`](http://en.wikipedia.org/wiki/ReStructuredText).<!--more-->

Здесь можно посмотреть результат [сгенеренной](http://slyfox.ath.cx/index.rst) и
[исходной](http://slyfox.ath.cx/index.rst?raw) страниц.


[Примеры](http://docutils.sourceforge.net/docs/user/rst/quickref.html) использования разметки `rst`.

Я это дело настроил на `apache-2.2` и `docutils-0.7`.

Нам нужно зарегать расширение `.rst` как нечто, что подается на вход генератору страницы.
Я просто написал `cgi`. Можно пойти дальше - вкрутить кэширование в `cgi` или написать
свой `mod_чегонибудь`, но у меня нагрузка пока просто смешная.

В корне нашего сайта лежит `.htaccess` с таким обработчиком:

~~~~
# mod_action
AddHandler reStructuredText-type .rst
Action     reStructuredText-type  /rst/handle-rst.cgi

DirectoryIndex index.rst
~~~~

Содержимое `rst/handle-rst.cgi`:

~~~~ { .sh }
#!/bin/sh -e

if [ x"$PATH_TRANSLATED" = x ]; then
    echo -e -n "Content-Type: text/plain\r\n"
    echo -e -n "\r\n"

    echo -e -n "Ok, please tell me how did you get here?"
 else
    if [ x"$QUERY_STRING" = x"raw" ]; then
        echo -e -n "Content-Type: text/plain\r\n"
        echo -e -n "\r\n"

        # raw dump
        cat "$PATH_TRANSLATED"
    else
        echo -e -n "Content-Type: text/xml\r\n"
        echo -e -n "\r\n"

        /usr/bin/rst2html.py \
            --link-stylesheet \
            --stylesheet-path="/rst/css/html4css1.css" \
            "$PATH_TRANSLATED"
    fi
fi
~~~~

Тут мы особым образом обрабатываем запрос `?raw` (выводим файл 'как есть') или
генерим `xml` скриптом **rst2html.py** (входит в состав [`docutils`](http://docutils.sourceforge.net/))
