---
title: xorg and udev
date: June 18, 2012
---

:PostID: 173
:Title: xorg and udev
:Keywords: xorg, hal, udev, x11_options, xorg.conf, xorg.conf.d
:Categories: notes

Давным-давно **Xorg** решил сделать жизнь простых юзеров лучше
и сделал автоконфигурацию настроек **X** сервера. Но для **Xorg**у
надо знать, как общаться с операционной системой. В далеком 2008
году интерфейсом был выбран `HAL <http://hackie.blog.tut.by/2009/01/07/haldudev/>`_.

С тех пор **HAL** был предан анафеме: плохой событийный интерфейс, недружелюбен к энергосбередению,
плохая архитектура, xml и прочие преступления, которых он не совершал. И был изгнан из дистрибутивов.

Вместо этого **Xorg** теперь разыскивает устройства через **udev**.

.. raw:: html

   <!--more-->

Всё бы хорошо, если бы не одна особенность:
**конфигурация udev не позволяет сделать всего того, что что позволял HAL**.

Пример конфига для **HAL**, `использующего x11_options <https://bugzilla.redhat.com/attachment.cgi?id=328263>`_:

.. code-block:: xml

  <?xml version="1.0" encoding="ISO-8859-1"?>
  <deviceinfo version="0.2">
    <device>
      <match key="info.capabilities" contains="input.mouse">
        <match key="info.product" contains="Logitech USB Receiver">
          <merge key="input.x11_options.AngleOffset" type="string">-35</merge>
          <merge key="input.x11_options.YAxisMapping" type="string">4 5</merge>
          <merge key="input.x11_options.XAxisMapping" type="string">6 7</merge>
          <merge key="input.x11_options.EmulateWheel" type="string">True</merge>
          <merge key="input.x11_options.EmulateWheelButton" type="string">9</merge>
        </match>
      </match>
    </device>
  </deviceinfo>

Тут мы видим, как заполняются произвольные атрибуты драйвера **evdev** (из **man evdev**).
Казалось бы, можно было бы реализовать в **udev** похожие штуки,
как советует `wiki ubuntu <https://wiki.kubuntu.org/X/InputConfiguration>`_:

.. code-block:: bash

  ENV{x11_options.JumpyCursorThreshold}="90", \
  ENV{x11_options.AreaBottomEdge}="4100"
  #
  ENV{ID_INPUT_TOUCHPAD}=="1", ENV{x11_options.AreaBottomEdge}="5000"

Но вот шутка, оно работает только в части дистрибутивов (**ubuntu**, **RHEL**).
В **Gentoo**, например, не работает.

Немного поковырявшись в исходниках оказалось, что патч (2 строки),
добавляющий эту "фишку" отсутствует в апстриме.

Я попробовал его `заслать <http://www.mail-archive.com/xorg-devel@lists.x.org/msg31064.html>`_
и получил `ответ <http://www.mail-archive.com/xorg-devel@lists.x.org/msg31068.html>`_,
что "фича" была вырезана специально и основным механизмом настройки теперь является .... **xorg.conf**!

**Xorg** по прежнему использует **udev** для получения списка текущих
и для отслеживания появления новых устройств, но вся конфиграция
должна находиться в **/etc/X11/xorg.conf** или раздельных файлах
**/etc/X11/xorg.conf.d/${name}.conf**.

Ну и ладно :] Теперь можно брать за основу стандартный конфиг:

.. code-block:: bash

    cp /usr/share/X11/xorg.conf.d/10-evdev.conf /etc/X11/xorg.conf.d/input.conf

и добавлять туда свои настройки для мыши, клавиатуры и др.:

::

  Section "InputClass"
        Identifier "evdev touchpad catchall"
        MatchIsTouchpad "on"
        MatchDevicePath "/dev/input/event*"
        Driver "evdev"
        #
        # here go our options
        Option "AreaBottomEdge" "5000"
        # ...
  EndSection

Как в старые добрые времена :]