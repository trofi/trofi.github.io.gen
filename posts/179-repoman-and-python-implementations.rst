---
title: repoman and python implementations
date: December 25, 2012
---

:PostID: 179
:Title: repoman and python implementations
:Keywords: emerge, portage, gentoo, haskell, overlay, python, pypy, repoman
:Categories: notes

До недавних пор В оверлее `gentoo-haskell <https://github.com/gentoo-haskell/gentoo-haskell/>`_
сравнительно плохо обстояло дело с качеством ебилдов.

Некоторые установить в принципе было нельзя потому, что
для них попросту отсутствуют зависимости сборки.

(Например, в **RDEPENDS** написано =dev-vcs/darcs-2.5\*, а самого пакета уже нет.)

.. raw:: html

   <!--more-->

Такие проблемы (и много других) умеет находить программа **repoman**.
Ее можно запускать в отдельном каталоге с ебилдами, категории или полном оверлее.

Запуск на полном оверлее - самый интересный: мы находим сразу все проблемы.
Это удобно, если мы собираемся выбросить какую-то старую версию программы
и убедиться, что никто на нее не завязан.

Пример вывода **repoman** для одного пакета:

.. code-block:: bash

    $ gentoo-haskell/dev-haskell/cabal:repoman full
    RepoMan scours the neighborhood...
      RDEPEND.suspect               8
       dev-haskell/cabal/cabal-1.8.0.6-r1.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.10.2.0.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.14.0.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.16.0.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.16.0.3.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.17.0_pre9999.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.17.0_pre20121101.ebuild: 'virtual/pkgconfig'
       dev-haskell/cabal/cabal-1.17.0_pre20121213.ebuild: 'virtual/pkgconfig'
      KEYWORDS.missing              2
       dev-haskell/cabal/cabal-1.17.0_pre20121101.ebuild
       dev-haskell/cabal/cabal-1.17.0_pre20121213.ebuild

Тут никаких серьезных предупреждений нет кроме 'virtual/pkgconfig'.

На полном оверлее таких предупреждений довольно много.
Чем их меньше - тем лучше качество ебилдов.

В **gentoo-haskell** больше тысячи еблидов, так что
**repoman** тратит на это некоторое время.

Я решил замерять несколько реализаций python:

.. code-block:: bash

    $ gentoo-haskell:for p in python2.7 python3.2 pypy-c1.9 pypy-c2.0; do echo "#$p"; time { $p /usr/bin/repoman full -d > repoman-QA-`date +%F-%T`.log; }; done
    #python2.7
    real    5m51.622s
    user    5m4.262s
    sys     0m46.309s
    #python3.2
    real    5m11.271s
    user    4m25.474s
    sys     0m44.388s
    #pypy-c1.9
    real    3m59.397s
    user    2m54.265s
    sys     1m4.584s
    #pypy-c2.0
    real    3m56.389s
    user    2m51.201s
    sys     1m4.338s

Интересно, что прирост производительности наблюдается
как и в новых релизах **cpython**, так и в реализации **pypy**.

Интересно, что в **pypy** системное время прилично больше.

**UPDATE**.
Забыл главное сказать!
Самым эффективным было бы
`размазать задачу на N голов <http://bugs.gentoo.org/448462>`_  (у меня их 8).
