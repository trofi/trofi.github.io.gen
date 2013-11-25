---
title: claws-mail filter magic
date: July 6, 2011
---

:PostID: 142
:Title: claws-mail filter magic
:Keywords: claws-mail, perl, regexp, move
:Categories: notes

Любой мало-мальски вменяемый почтовый клиент позволяет разделять письма
по каким-то признакам. Я всё еще юзаю **claws-mail** и до недавнего времени
использовал **Processing** правила для списков рассылки, на которые я подписан.

.. raw:: html

   <!--more-->

Типичное **processing** правило (**~/.claws-mail/matcherrc**):

.. code-block:: c

    enabled rulename "maemo" header "List-Id" matchcase "for maemo developers <maemo-developers.maemo.org>" move "#mh/Mailbox/другие/maemo-developers"
    enabled rulename "lkml" header "X-List-ID" matchcase "kernelnewbies.nl.linux.org" move "#mh/Mailbox/другие/kernelnewbies"
    enabled rulename "mc-bugs" header "List-Id" matchcase "<mc-bugs.googlegroups.com>" move "#mh/Mailbox/mail-lists/mc/mc-bugs"
    enabled rulename "mc-commits" header "List-Id" matchcase "<mc-commits.googlegroups.com>" move "#mh/Mailbox/mail-lists/mc/mc-commits"
    enabled rulename "haskell-cafe" header "List-Id" matchcase "haskell-cafe.haskell.org" move "#mh/Mailbox/haskell-cafe"
    enabled rulename "haskell" header "List-Id" matchcase "haskell.haskell.org" move "#mh/Mailbox/haskell"
    enabled rulename "gentoo-commits.gentoo.org" header "List-Id" matchcase "gentoo-commits.gentoo.org" move "#mh/Mailbox/gentoo/commits"

Чтобы создавать/редактировать эти правила можни или править этот файл или кликать мышью.
Но иногда хочется выковырять какую-то часть **subject** или часть другого **mime** поля
и произвести фильтрацию по ней.

К примеру, разбросать по каталогам следующего вида письма:

.. code-block:: c

    Subject: [project1 0001]: bug1
    From: mantis <user@email>
    ...
    Subject: [project2 0002]: bug1
    From: mantis <user@email>

Хочется раскидать их по **project1 -> mantis/foo**, **project2 -> mantis/bar**, а остальное - просто в **mantis/**.
Можно руками создать 3 правила, но нельзя использовать в имени каталога-назначения результат сопоставления
по **regexp** (**TODO:** расширить **move** до понимания подстановок из **regexp**).
Но можно воспользоваться `perl плагином <http://www.claws-mail.org/plugins.php>`_, в котором можно реализовать
более развитую логику:

.. code-block:: perl

    # ~/.claws-mail/perl_filter
    my $h  = header subject;
    my $f  = header from;
    if ($f eq 'mantis <user@email>') {
    # [project_name bug_id]: description
    my $target = "#mh/Mailbox/mantis";
    if ($h =~ /\[([^\]]+) \d+\]:/) {
        my $project = $1;
        my %project_map = ( 'project1' => 'foo'
                          , 'project2' => 'bar'
                          );
        # вот она, чудострока:
        $target = ($target . "/" . $project_map{$project}) || $target;
    }
    make_sure_folder_exists "$target"; # автоматически создает каталог
    move "$target";
}

Желательно прочитать `различия между processing и filtering <http://www.claws-mail.org/faq/index.php/Filtering_and_Processing_of_Messages#What_is_the_difference_between_Filtering.2C_Folder_Processing.2C_Pre.2FPost-processing.3F>`_.
Вкратце: **filtering** автоматически выполняется только на только что вошедших (**Inbox**) сообщениях и только
после всех **processing** правил.

Можно повторно запустить все **filtering** правила на выделенные письма или каталог из меню:
**Tools->Filtering**.

В **man cm_perl** можно почитать про все (их мало) селекторы и действия.
