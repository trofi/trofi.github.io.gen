---
title: hackport gets hackage status and profile use depends
date: February 29, 2012
---

:PostID: 167
:Title: hackport gets hackage status and profile use depends
:Keywords: gentoo, hackport
:Categories: notes

Недавно снова взялся за `hackport <https://github.com/gentoo-haskell/hackport>`_.
[ Это прога, которая позволяет конвертировать пакеты с `hackage <http://hackage.haskell.org/packages/hackage.html>`_ в
`оверлей gentoo-haskell <https://github.com/gentoo-haskell/gentoo-haskell>`_. ]

.. raw:: html

   <!--more-->

**qnikst** `запилил <https://github.com/gentoo-haskell/hackport/commit/620218baed7e817867e6388fb17ecfa900359002>`_
поддержку зависимостей в стиле **EAPI=2+**. Теперь сгенеренные ебилды выглядят так:

.. code-block:: bash

    EAPI=4
    CABAL_FEATURES="lib profile haddock hoogle hscolour"
    inherit haskell-cabal
    ...
    KEYWORDS="~amd64 ~x86"
    IUSE=""
    # самое интересное:
    RDEPEND="=dev-haskell/conduit-0.2*[profile?]
                   =dev-haskell/stm-2.2*[profile?]
                   =dev-haskell/stm-chans-1.3*[profile?]
                   =dev-haskell/transformers-0.2*[profile?]
                   >=dev-lang/ghc-6.10.1"
    DEPEND="${RDEPEND}
                   >=dev-haskell/cabal-1.8"

Теперь к либам добавляется такое вот чудо. Такие веселые зависимости
не дадут собрать библиотеку с влюченным **USE=profile**, пока ее
зависимости тоже не будут собраны с поддержкой профилирования.

А я решил добавить команду ``hackport status --from-hackage``, которая
бы показывала что можно обновить в оверлее с **hackage**.
Она понимает категории и игнорировать **live** версии пакетов в оверлее.

Выглядит оно примерно так: `|картинка| <http://slyfox.ath.cx:8081/i/2012-02-28-11-09-29.png>`_.

.. |картинка| image:: http://slyfox.ath.cx:8081/i/2012-02-28-11-09-29.png

С виду юзабельно :]

Кстати, команда ``hackport status`` (без параметров) выведет также пакеты на hackage, которые в оверлее отсутствуют.
