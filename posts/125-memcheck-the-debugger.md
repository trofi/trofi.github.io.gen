---
title: "memcheck: the debugger"
date: October 21, 2010
---

[`valgrind`](http://valgrind.org) - это подпроект `KDE`, который направлен
на отлов багов в `С` и `C++` программах.

Лирики и истории тут не будет, но можно почитать
[ссылку](http://dot.kde.org/2006/02/20/interview-valgrind-author-julian-seward).

Краткая теория как работает `valgrind`:

`valgrind` - это набор утилит, построенных поверх либы-эмулятора процессора `libvex`
(причем `libvex` может эмулировать не только тот процессор, на котором она рабоотает,
но и другие). То есть `libvex` читает инструкции процессора и эмулирует их, отслеживая
что какая инструкция куда переместила (с точность до бита!).

В состав `valgrind` входит утилита `memcheck` (самая популярная утилита, работает по умолчанию).
В послании расскажу пока только про нее.

Она отслеживает:

1. **обращение за пределы выделенной памяти вроде такого:**

~~~~ { .c }
int main() {
    char * p = malloc (1);
    p[1] = '!'; // пишем сразу за границу буфера
    free (p);
    return 0;
}
~~~~

2. **использование неинициализированных данных**

~~~~ { .c }
#include <stdio.h>
int main() {
    int trash; // не инициализируем
    printf ("%d\n", trash);
    return 0;
}
~~~~

3. **утечки памяти**

~~~~ { .c }
int main() {
    char * p = malloc (1); // и не освобождаем
    return 0;
}
~~~~

Чтобы получить вменяемую иформацию о том, где произошла ошибка - нужно:

* собрать программу с отладкой. Для этого у `gcc` есть семейство ключей, начинающихся на `-g`.
* (не обязательно) выставить уровень оптимизаций поменьше. Снижение уровня оптимизаций улучшает
  точность отладочной информации: функции и шаблоны не инлайнятся, порядок инструкций не меняется и т.д.
  Так что изменяя оптимизации вы трейсите немного разные программы (в которых могут встречаться совсем
  разные баги). Но обычно это не проблема.

Собираем, запускаем (я взял пример `2.` выше):

~~~~ { .sh }
gcc -ggdb -O0 main.c -o main

valgrind --track-origins=yes ./main
~~~~

Получаем:

~~~~
==2396== Use of uninitialised value of size 8
==2396==    at 0x4E6D81B: _itoa_word (in /lib64/libc-2.11.2.so)
==2396==    by 0x4E6EA7C: vfprintf (in /lib64/libc-2.11.2.so)
==2396==    by 0x4E788F9: printf (in /lib64/libc-2.11.2.so)
==2396==    by 0x400552: main (main.c:4)
==2396==  Uninitialised value was created by a stack allocation
==2396==    at 0x400534: main (main.c:2)
~~~~

Из этого всего мы видим, что в строке `main.c:4` произошло использование
неинициализированной переменной размера 8. Создано значение на стеке в строке `main.c:2`.
Созданное значение отслеживается **только** со включенным ключом `--track-origins=yes`.
Ключ появился в `valgrind` в версии `3.5.0`.

Всё просто. Мы видим стек вызовов. В самом верху - самые близкие к ошибке строки, ниже - те,
кто эти строки вызвал (размотан стек).

По умолчанию `valgrind` трейсит только один процесс и пишет вывод на `stdout`.
Это классно для консольных интерактивных программ, но неудобно при отладке демонов.
На этот случай у `valgrind` есть вагон ключей:

* `--trace-children=yes` - трейсит все содраваемые процессом подпроцессы. Для этого в логе
  и пишется `PID` процесса вначале: `==PID==`.

* `--log-file=<log_file_name>` - выводит лог в отдельный файл.

Есть и другие интересные полезные ключи (`valgrind --help`):

* `--num-callers=NUM` - глубина размотки стека при отображении ошибки (`valgrind` отслеживает какая функция какую вызвала).
  В примере выше у нас 4 уровня вложенности: `_itoa_word`, `vfprintf`, `printf`, `main`.
  По умолчанию отслеживается 12 вызовов. Часто это бывает мало (у нас в проекте до 30).

* `--verbose` - показывает много всего интересного (и не очень): какие функции перехватываются `valgrind`
  и прочая ерунда.

* `--track-fds=yes` - отслеживает незакрытые файлы при завершении программы (особо больной вопрос для демонов).

* `--db-attach=yes` - присосаться к программе отладчиком в точке возникновения ошибки
  (в случае с демонами это не так просто :]).

* `--leak-check=full` - ищет утечки памяти

* `--show-reachable=yes` - показывает неосвобожденную, но неутекшую (есть ссылки из программы)
  память при завершении программы.

* `--malloc-fill=val` / `--free-fill=val` - забавает памят после `malloc`/`free` значениями `val`.
  По умолчанию 0 (не всегодя хорошо, так как гореспрограммисты любят совать проверки на 0 без повода).

У `C++` программ (особенно написаных с использованием `STL`) есть одна засада, усложняющая ловить ошибки:
многие функции работы с памятью выделяют боьлше памяти, чем это реально надо. Эта оптимизация скрывает
ошибки обращения за границы выделенной памяти в контейнерах типа `std::vector`.

К счастью в шаблонах контейнеров `gcc` есть волшебная переменная среды, отключающая такие оптимизации: `GLIBCXX_FORCE_NEW`.

Итак, наша коммандная строка параноика довольно жирная. Слепим микроскрипт:

~~~~ { .sh }
#!/bin/sh

# лежит где-то в ~/bin/vg.sh

GLIBCXX_FORCE_NEW=1 \
valgrind \
    --track-origins=yes  \
    --trace-children=yes \
    --num-callers=50     \
    --track-fds=yes      \
    --leak-check=full    \
    --show-reachable=yes \
    --malloc-fill=0xa1   \
    --free-fill=0xa1     \
"$@"
~~~~

Юзать так:

~~~~ { .sh }
vg.sh --log-file=valgrind.log ./my-nice-program --my-nice-params
~~~~

Работает даже на демонах :]

Еще читаете? Офигеть! Для самых терпеливых: черная магия!

`valgrind` позволяет не только ковыряться в уже готовых бинарниках.
В свой исходник можно вставлять специальные макросы, коотрые
работают только при сборке с поддержкой `valgrind`. Сидят эти макросы в
`<valgrind/valgrind.h>` и `<valgrind/memcheck.h>`.

С их помощью можно решать самые разные задачи.

Например, представим, что у нас в программе есть свой распределитель памяти и
мы хотим отслеживать утечки в нем:

~~~~ { .c }
#include <stdlib.h>

typedef unsigned char u8;

static u8 *   mempool      = 0;
static u8 *   mempool_p    = 0;
static size_t mempool_size = 0;

void mempool_init (size_t pool_size) {
    mempool_p = mempool = (u8*)malloc (pool_size);
    if (mempool) mempool_size = pool_size;
}

void mempool_destroy (void) { free (mempool); }

void * mempool_alloc (size_t obj_size) {
    // не влезет
    if (mempool_p + obj_size > mempool + mempool_size) return 0;

    void * result = mempool_p; mempool_p += obj_size;
    return result;
}

void mempool_free (void * obj, size_t obj_size) {
    // освобождаем только последний. такая вот халтура :]
    if (mempool_p == (u8*)obj + obj_size) mempool_p -= obj_size;
}

int main () {
    mempool_init (1000);

    void * p1 = mempool_alloc (10);
    void * p2 = mempool_alloc (20);
    mempool_free (p2, 20);
    // Забыли
    //mempool_free (p1, 10);

    mempool_destroy ();
    return 0;
}
~~~~

~~~~
$ g++ -ggdb -O0 a.c -o a
$ vg.sh ./a
...
==24548== HEAP SUMMARY:
==24548==     in use at exit: 0 bytes in 0 blocks
==24548==   total heap usage: 1 allocs, 1 frees, 1,000 bytes allocated
~~~~

Врёт! `p1` то не освобожден!
У `valgrind` есть чудомакросы, помечающие память, как выделенную из аллокатора:

~~~~ { .c }
VALGRIND_MALLOCLIKE_BLOCK(addr, sizeB, rzB, is_zeroed)
VALGRIND_FREELIKE_BLOCK(addr, rzB)
~~~~

Их больше, но заюзаем только эти. Изменим исходнег:

~~~~ { .c }
#include <stdlib.h>
#include <valgrind/valgrind.h> // 1

typedef unsigned char u8;

static u8 *   mempool      = 0;
static u8 *   mempool_p    = 0;
static size_t mempool_size = 0;

void mempool_init (size_t pool_size) {
    mempool_p = mempool = (u8*)malloc (pool_size);
    if (mempool) mempool_size = pool_size;
}

void mempool_destroy (void) { free (mempool); }

void * mempool_alloc (size_t obj_size) {
    // не влезет
    if (mempool_p + obj_size > mempool + mempool_size) return 0;

    void * result = mempool_p; mempool_p += obj_size;
    VALGRIND_MALLOCLIKE_BLOCK(result, obj_size, 0, 0); // 2
    return result;
}

void mempool_free (void * obj, size_t obj_size) {
    // освобождаем только последний. такая вот халтура :]
    if (mempool_p == (u8*)obj + obj_size) mempool_p -= obj_size;
    VALGRIND_FREELIKE_BLOCK(obj, 0); // 3
}

int main () {
    mempool_init (1000);

    void * p1 = mempool_alloc (10);
    void * p2 = mempool_alloc (20);
    mempool_free (p2, 20);
    // Забыли
    //mempool_free (p1, 10);

    mempool_destroy ();
    return 0;
}
~~~~

~~~~
$ g++ -ggdb -O0 a.c -W -Wall -o a
a.c: In function ‘int main()’:
a.c:35: предупреждение: неиспользуемая переменная ‘p1’
# хехе, нашу переменную засекли :]
$ vg.sh ./a
...
==8958== HEAP SUMMARY:
==8958==     in use at exit: 1,000 bytes in 1 blocks
==8958==   total heap usage: 3 allocs, 2 frees, 1,030 bytes allocated
==8958== 
==8958== 1,000 bytes in 1 blocks are still reachable in loss record 1 of 1
==8958==    at 0x4C2635E: malloc (vg_replace_malloc.c:236)
==8958==    by 0x4008E5: mempool_init(unsigned long) (a.c:11)
==8958==    by 0x400A8C: main (a.c:33)
~~~~

Оп! Мы немножко запутали `valgrind` тем, что у нас `p1` и `mempool` - один адрес.
Из-за этого он неправильно отределяет точку выделения памяти. Это баг в `valgrind`! :]
Ну не важно. Главное, что псевдоутечка детектируется.


Второй пример - поиск врага, модифицирующего память.

~~~~ { .c }
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void init (char * p);
void process (const char * p);

int main ()
{
    char * p = (char *)malloc (100);
    init (p);
    printf ("before process: p[33] = %c\n", p[33]);
    process (p);
    printf ("after process:  p[33] = %c\n", p[33]);
    free (p);
    return 0;
}

// Делаем вид, что дальше идёт много чужого неясного кода
void init (char * p)
{
    memset (p, '+', 100);
}

void process (const char * p)
{
    char * q = (char *)p; // ужас! сейчас испоганит!
    q[33] = '-';
}
~~~~

Глядя на прототип функции `process` никак не скажешь,
что она может изменить данные `p[33]`, но тест `printf` говорит об обратном.

Как найти врага? Сделаем память "недоступной" и проверим:

~~~~ { .c }
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <valgrind/memcheck.h>

void init (char * p);
void process (const char * p);

int main ()
{
    char * p = (char *)malloc (100);
    init (p);
    printf ("before process: p[33] = %c\n", p[33]);
    VALGRIND_MAKE_MEM_NOACCESS(p, 100);
    process (p);
    printf ("after process:  p[33] = %c\n", p[33]);
    free (p);
    return 0;
}

// Делаем вид, что дальше идёт много чужого неясного кода
void init (char * p)
{
    memset (p, '+', 100);
}

void process (const char * p)
{
    char * q = (char *)p; // ужас! сейчас испоганит!
    q[33] = '-';
}
~~~~

Запускаем:

~~~~
$ g++ -ggdb -O0 a.c -W -Wall -o a
$ vg.sh ./a
...
before process: p[33] = +
==22521== Invalid write of size 1
==22521==    at 0x400A79: process(char const*) (a.c:31)
==22521==    by 0x400A0A: main (a.c:16)
==22521==  Address 0x5931061 is 33 bytes inside a block of size 100 alloc'd
==22521==    at 0x4C2635E: malloc (vg_replace_malloc.c:236)
==22521==    by 0x40096F: main (a.c:12)
==22521== 
==22521== Invalid read of size 1
==22521==    at 0x400A13: main (a.c:17)
==22521==  Address 0x5931061 is 33 bytes inside a block of size 100 alloc'd
==22521==    at 0x4C2635E: malloc (vg_replace_malloc.c:236)
==22521==    by 0x40096F: main (a.c:12)
==22521== 
after process:  p[33] = -
...
~~~~

`read` нас не очень интересует, а вот `write` - это и есть наш запрятанный глюк:
`(a.c:31)`

Хватит для начала :]
