---
title: gentoo-haskell progress for two weeks
date: July 19, 2010
---

Сегодня закончился мой двухнедельный отпуск. Подведем итоги свершенного:

В официальное **дерево** ебилдов таки добавил (как и [обещал](/2009/12/11/gentoo-new-dev-request/)):

*   [media-sound/xmms2](http://packages.gentoo.org/package/media-sound/xmms2)
*   [net-fs/smbnetfs](http://packages.gentoo.org/package/net-fs/smbnetfs)

По ходу дела было закрыто 30 багов в [gentoo bugzilla](http://bugs.gentoo.org), относящихся к **haskell**.

Команда **gentoo-haskell** (aka [haskell herd](http://www.gentoo.org/proj/en/prog_lang/haskell/index.xml))
(в лице меня :]) не может удержаться, чтобы не сообщить, что она добыла бинарники **ghc** для следующих архитектур:

*   **alpha**: 6.10.4-r1, 6.12.3 (собирается 8.5 часов)
*   **ia64**: 6.10.4-r1, 6.12.3 (собирается 7.5 часов)
*   **x86-fbsd**: 6.12.3 (не сообщают, но думаю часа 1.5, как и обычный **x86**)

Как это обычно бывает с экзотическими архитектурами, без патчей не обошлось.<!--more-->
На **alpha** для **ghc**-6.10.4-r1 всё было хорошо, а в 6.12.3 сгнил кусочек кода,
отвечающий за высовывание **haskell** функций в **C** рантайм (так называемый
_foreign import wrapper_ aka **f.i.w**, бывший _foreign export dynamic_,
вкратце описанный [здесь](/2010/04/02/ghc-fixing-libffi-on-powerpc64/)).
Это была ошибка времени компиляции, исправилась откатом на переносимую
реализацию замыканий в **libffi** ([патч](http://darcs.haskell.org/cgi-bin/darcsweb.cgi?r=ghc;a=darcs_commitdiff;h=20100708065318-6895e-1e7f098b8589da683d6aa48ee7165d0760b2686d.gz)).

На **ia64** всё было горадзо разнообразнее:
Собрал я бинарник 6.10.4 и пытался собрать 6.12.3. **GHC** доходил до фазы сборки
самого себя и падал с очень пространной ошибкой:

~~~~
"inplace/bin/ghc-stage2"   -H32m -O -H64m -O0 -w ...
 ghc-stage2: internal error: evacuate: strange closure type 15
      (GHC version 6.12.3 for ia64_unknown_linux)
      Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
 make[1]: *** [libraries/dph/dph-base/dist-install/build/Data/Array/Parallel/Base/Hyperstrict.o] Aborted
~~~~

Ошибка гласит о том, что замыкание "статического" типа (в сегмете кода) якобы одновременно находится и в куче.
Проблема оказалась в виртуальных адресах регионов памяти на системе **ia64** (спасибо zygoloid).

Рассмотрим *amd64*: это железо физически не может оперировать виртуальными адресами старше 48 бит
(и физическими старше 36 бит на десктопных процах). Судя по всему это ограничение того, как
представляется **MMU** в кристалле: содранная по структуре с 32-битных (**i386**) предков структура

    virtual_address = PAGE_DIR   << (10 + 12) // 10 bits
                    | PAGE_TABLE << 12        // 10 bits
                    | PAGE                    // 12 bits // 4K pages
                                              // all: 32 bits

Превратилась в([пруф](http://en.wikipedia.org/wiki/X86-64#Page_table_structure)):

    virtual_address = // PML5 not implemented yet
                      PML4       << (?? + ?? + 13) // up to 8 bits
                    | PAGE_DIR   << (?? + 13)      // ?? bits
                    | PAGE_TABLE << 13             // ?? bits
                    | PAGE                         // 13 bits // 8K pages
                                                   // all: up to 48 bits

Посмотрим на распределение памяти в типичном процессе на **amd64**:

~~~~
[sf] ~:pmap $$
23807:   bash
0000000000400000    824K r-x--  /bin/bash                  # code
00000000006cd000      4K r----  /bin/bash                  # code
00000000006ce000     36K rw---  /bin/bash                  # PLT/GOT
00000000006d7000     24K rw---    [ anon ]                 # bss/rw data
0000000000ca9000   1876K rw---    [ anon ]                 # heap
00007f2370e49000      8K r-x--  /usr/lib64/gconv/KOI8-R.so # shared lib
...
00007f23725d3000      4K rw---  /lib64/ld-2.11.2.so        # shlib PLT/GOT
00007f23725d4000      4K rw---    [ anon ]                 # bss/rw data

00007fff0f79b000    132K rw---    [ stack ]                # stack!
00007fff0f7ff000      4K r-x--    [ anon ]                 # stack bottom, red zone? или вообще in-userspace vdso. хз
ffffffffff600000      4K r-x--    [ anon ]                 # vdso
 total            25296K
~~~~

Как видим, старших 4 байта адреса всегда равны нулю (ну кроме **vdso**, но ей можно).
*GHC* на этом и схалтурил. Минимальная самостоятельная единица, выделяемая _storage manager_ - **1MB** (20 bits).
Итого значащих битов: 48 - 20 = 28 bits. **GHC** создает массив битов (выставленный бит означает,
что адрес выделен в _heap_). Индексом в массиве является значащая часть указателя (28-битное значение).

Всё было бы хорошо, если бы остальные 64-битные архитектуры так делали, и они обычно так и делают (хотя и не обязаны).
На **ppc64**, **alpha** и **amd64** всё так и есть (правда, всегда может измениться),
но на **ia64** тип региона определяется старшим байтом в адресе.

Посмотрим на распределение памяти в типичном процессе на **ia64**:

~~~~
slyfox@i2 ~ $ pmap $$
30584:   -bash
0000000000000000     16K r----    [ anon ]              # zero address mapping (sysctl vm.mmap_min_addr)
2000000000000000    240K r-x--  /lib/ld-2.11.2.so       # shlib code
2000000000048000     32K rw---  /lib/ld-2.11.2.so       # shlib bss
...
4000000000000000   1744K r-x--  /bin/bash               # code
6000000000000000     80K rw---  /bin/bash               # bss/rw data
...
6000000000014000    304K rw---    [ anon ]              # heap
600007ffffd74000     16K rw---    [ anon ]
60000fffffd1c000    336K rw---    [ stack ]             # stack
a000000000000000    128K r-x--    [ anon ]              # ? vdso?
 total             7440K
~~~~

Итак, что мы видим: если отбросить старшие 4 байта адреса из секций _code_ и _heap_ мы как нефиг
можем (и получаем) в нашем случае коллизию - то есть на статический код наш Ъ-алгоритм находит
соответствующий участок в памяти. Решение простое - не обрезать старшие биты на 64-битных не-**amd64**.
Это снизит производительность сборщика мусора, но там и так много проблем помимо этой мелочи :]. ([патч](http://darcs.haskell.org/cgi-bin/darcsweb.cgi?r=ghc;a=darcs_commitdiff;h=20100709115917-6895e-c85e49343d85060ec90f07edd4fcef1d84a9ef3a.gz))

Кстати, эта проблема раньше обходилась костылями, которые двигали туда-сюда сегмент кода, чтобы такие
коллизии не возникали. Эти костыли тоже рассыпались и их пришлость выкосить ([патч](http://darcs.haskell.org/cgi-bin/darcsweb.cgi?r=ghc;a=darcs_commitdiff;h=20100708180943-6895e-d32465a82d9ef3a8197f1a9440809ffd948b1e54.gz)).

Это позволилос собрать **ghc**-6.12.3, но на этом дело не кончилось.

Собранный на 6.12.3 **darcs**-2.4.4 сразу начал падать. По тесту [#3516](http://hackage.haskell.org/trac/ghc/ticket/3516)
я быстро вычислил, что виноват опять-таки **f.i.w**.
Проблема ровно как с [**ppc64**](/2010/04/02/ghc-fixing-libffi-on-powerpc64/),
фиксилось также - откатом на **libffi** ([патч](http://darcs.haskell.org/cgi-bin/darcsweb.cgi?r=ghc;a=darcs_commitdiff;h=20100709213922-6895e-cd19f733b120f43137ac7ed811187a550eb5b668.gz)).
У меня уже приличный опыт в этом деле **:D**.

С **x86-freebsd** проблем не было за исключением того, что в версии платформы **ghc** никак не ожидал видеть
**i686-gentoo-freebsd8** :]
Это фиксится локальными хаками в gentoo, пока апстрим не склонится к нормальному использованию **autotools**.

Итого, текущая картина по бинарникам:

~~~~
package                               alpha   amd64   ia64   ppc   ppc64   sparc   x86   x86-fbsd
----------------------------------- ------- ------- ------ ----- ------- ------- ----- ----------
ghc-6.12.3                           ~alpha  ~amd64  ~ia64        ~ppc64          ~x86  ~x86-fbsd
ghc-6.12.1                                   ~amd64         ~ppc  ~ppc64  ~sparc  ~x86           
ghc-6.10.4-r1                        ~alpha  ~amd64  ~ia64  ~ppc  ~ppc64  ~sparc  ~x86           
ghc-6.2.2                                                    ppc           sparc   x86           
ghc-6.4.2                            ~alpha   amd64  ~ia64   ppc   ppc64   sparc   x86  ~x86-fbsd
ghc-6.6                              ~alpha  ~amd64         ~ppc          ~sparc  ~x86           
ghc-6.6.1                            ~alpha   amd64  ~ia64   ppc           sparc   x86           
ghc-6.8.2                             alpha   amd64   ia64  ~ppc           sparc   x86           
ghc-6.8.2-r1                                  amd64                                x86           
~~~~

Всего-то нужно добыть доступ к **ppc**, **ppc64** и **sparc** :]

Отдельное спасибо **Сергею Семашко** (aka **ssvb**) за предоставленный доступ к **G5** (**ppc64**).
Без опыта, полученного истязанием там **ghc** я бы не зафиксил все эти поганые
баги (ну как минимум так быстро).
