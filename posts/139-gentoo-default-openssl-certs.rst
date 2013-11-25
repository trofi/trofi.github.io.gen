---
title: default ssl certificates in gentoo
date: May 3, 2011
---

:PostID: 139
:Title: default ssl certificates in gentoo
:Keywords: gentoo, ssl, default, localhost, apache, certificate
:Categories: notes

Некоторые программы в **gentoo** (**apache**, **postfix**) требуют наличия
более-менее правильных **SSL** сертификатов. Ебилды этих пакетов занимаются
автогенерацей таких сертификатов, которые потом хранятся в **/etc/ssl/${PN}/**.

Генерируются они один раз и содержат всякую ерунду в информации о владельце сертификата.

На одной из старых машин у меня истёк срок действия **SSL** сертификата и как следствие
отвалился **HTTPS**. **emerge -1 apache** не помог. Я решил немного разобраться как
(пере)генерить сертификаты.

.. raw:: html

   <!--more-->

Для начала найдем, кто же и когда генерит эти сертификаты. Берем ебилд **apache**:
**${PORTDIR}/www-servers/apache/apache-2.2.17.ebuild** -> **inherit apache-2** ->
**${PORTDIR}/eclass/apache-2.eclass**:

.. code-block:: bash

    inherit ... ssl-cert
    ...
    apache-2_pkg_postinst() {
    if use ssl && [[ ! -e "${ROOT}/etc/ssl/apache2/server.pem" ]]; then
        SSL_ORGANIZATION="${SSL_ORGANIZATION:-Apache HTTP Server}"
        install_cert /etc/ssl/apache2/server
    ...

Всё просто: если файла **server.pem** нет, то сертификаты генерятся. Итак,
решение для самых ленивых:

    удалить файлы **rm /etc/ssl/apache2/*** и пересобрать **emerge -1 apache**.

Теперь подробнее изучим функцию **install_cert** из **ssl-cert.eclass**.
Оказывается, мы можем поизменять большинство параметров в **gen_cnf**:

.. code-block:: bash

    # These can be overridden in the ebuild
    SSL_DAYS="${SSL_DAYS:-730}"
    SSL_BITS="${SSL_BITS:-1024}"
    SSL_COUNTRY="${SSL_COUNTRY:-US}"
    SSL_STATE="${SSL_STATE:-California}"
    SSL_LOCALITY="${SSL_LOCALITY:-Santa Barbara}"
    SSL_ORGANIZATION="${SSL_ORGANIZATION:-SSL Server}"
    SSL_UNIT="${SSL_UNIT:-For Testing Purposes Only}"
    SSL_COMMONNAME="${SSL_COMMONNAME:-localhost}"
    SSL_EMAIL="${SSL_EMAIL:-root@localhost}"

Так что можно выставить какие-то из этих переменных прямо в **/etc/make.conf**!
Ставим **SSL_DAYS** побольше и больше не паримся.

Чтобы получить чуть большую гибкость для генерации сертификатов напишем микроскрипт:

.. code-block:: bash

    #!/bin/bash
    source /etc/make.conf
    # эмулируем окружение, создаваемое 'ebuild.sh'
    T=`mktemp -d`
    env > "${T}/environment"
    # всё дубово
    ebegin() {
        echo -n "$@ ..."
    }
    eend() {
        [[ $@ -eq 0 ]] && echo "OK" || echo "FAIL"
    }
    ewarn() {
        echo "WARN: $@"
    }
    eerror() {
        echo "ERROR: $@"
    }
    source "$PORTDIR/eclass/ssl-cert.eclass"
    # магия!
    install_cert ./test-certs/server
    rm -rf -- "${T}"

**"$PORTDIR/eclass/ssl-cert.eclass"** лучше скопировать куда-нибудь
и немного его подправить. Можно, например, задать:

- более вменяемые источники энтропии (**SSL_RANDOM** в **gen_cnf**)
- умолчальный конфиг **/etc/ssl/openssl.conf**
- что-нибудь еще
