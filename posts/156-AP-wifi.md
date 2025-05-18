---
title: AP wifi
date: October 23, 2011
---

На этой неделе у меня появился очередной девайc: `TP-LINK TL-WN722N`.
Задача-максимум была поднять его в `Access Point` режиме (`master mode`).

На `gentoo-wiki` оказась [отличная
статья](http://en.gentoo-wiki.com/wiki/Wireless/Access_point).

Всё, что мне нужно было сделать - определить драйвер модуля в ядре и
настроить `hostapd`. Определить оказалось просто. Суём девайс в
`USB` и смотрим в вывод `dmesg`:

    usb 2-6: new high speed USB device number 6 using ehci_hcd
    usb 2-6: New USB device found, idVendor=0cf3, idProduct=9271
    usb 2-6: New USB device strings: Mfr=16, Product=32, SerialNumber=48
    usb 2-6: Product: USB2.0 WLAN
    usb 2-6: Manufacturer: ATHEROS
    usb 2-6: SerialNumber: 12345

Видим, что это `ATHEROS` `idProduct=9271`. Гуглим про него (или
грепаем исходники ядра на `9271`). И находим красивую статью про
[`ath9k_htc`](http://linuxwireless.org/en/users/Drivers/ath9k_htc).
Собираем модуль ядра:

    CONFIG_ATH_COMMON=m
    CONFIG_ATH9K_HW=m
    CONFIG_ATH9K_COMMON=m
    CONFIG_ATH9K_HTC=m

    usb 2-6: ath9k_htc: Firmware - htc_9271.fw not found

Не хватает `firmware` (кода, который заливается в саму железку). По
ссылке выше он есть, но возиться с файлами и подкладывать их вручную не
очень хорошо. Я их
[запаковал](http://packages.gentoo.org/package/net-wireless/ar9271-firmware).

    usb 2-6: ath9k_htc: Transferred FW: htc_9271.fw, size: 51272
    ath9k_htc 2-6:1.0: ath9k_htc: HTC initialized with 33 credits
    ath9k_htc 2-6:1.0: ath9k_htc: FW Version: 1.3
    ath: EEPROM regdomain: 0x809c
    ath: EEPROM indicates we should expect a country code
    ath: doing EEPROM country->regdmn map search
    ath: country maps to regdmn code: 0x52
    ath: Country alpha2 being used: CN
    ath: Regpair used: 0x52
    ieee80211 phy1: Atheros AR9271 Rev:1
    Registered led device: ath9k_htc-phy1

После установки пакета он заводится и работает.

В `hostapd.conf` по сравнению с конфигом в `wiki` поменял только
пару строк:

    interface=wlan0
    bridge=br0
    driver=nl80211

В `conf.d/net` выставлен режим `bridge`:

    modules_wlan0=( "!iwconfig !wpa_supplicant" )
    config_wlan0=( "null" )
    config_eth0=( "null" )

    bridge_br0=( "eth0" )
    config_br0=( "<IP> netmask <MASK> broadcast <BCAST>" )
