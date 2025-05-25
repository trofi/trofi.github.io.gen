---
title: gcc's -Wmissing-format-attribute option
date: March 12,2013
---

Начну сразу с примера. Игрушечний пример выглядит так:

```cpp
/* a1.c */
#include <stdarg.h>
#include <stdio.h>
static int verbosity = 0;
static void v_log (int minimal_verbosity, const char *fmt, ...) {
    va_list ap;
    /* should we output it? */
    if (minimal_verbosity > verbosity)
        return;
    /* real work */
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
}
int main() {
    /* parse something */
    verbosity = 5;
    /* */
    v_log (3, "About to exit from %s\n");
    return 0;
}
```

Типичная программа. Чтобы найти ошибку - нужно хорошенько
присматриваться с форматной строке, а хочется автоматизировать поиск
таких тривиальных ошибок.

С давних пор `gcc` позволяет объявить функцию, как
`printf()`-подобную:

```cpp
/* a2.c */
#include <stdarg.h>
#include <stdio.h>
static int verbosity = 0;
static void v_log (int minimal_verbosity, const char *fmt, ...) __attribute__ ((format (printf, 2, 3)));
static void v_log (int minimal_verbosity, const char *fmt, ...) {
    va_list ap;
    /* should we output it? */
    if (minimal_verbosity > verbosity)
        return;
    /* real work */
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
}
int main() {
    /* parse something */
    verbosity = 5;
    /* */
    v_log (3, "About to exit from %s\n");
    return 0;
}
```

```
$ gcc a2.c -Wall -o a
a2.c: В функции «main»:
a2.c:19:9: предупреждение: format «%s» expects a matching «char *» argument [-Wformat]
```

Споймал! Но руками выискивать функции с `ellipsis` (`(...)`) тоже
не хочется.

Сегодня с утра одним глазом смотрел, как собирается `gcc` (ну а что
еще гентушнегу делать?) и заметил неведомую для себя опцию
`-Wmissing-format-attribute`.

Она, собственно, подсказывает куда надо всунуть форматные атрибуты:

```
$ gcc a1.c -Wall -Wmissing-format-attribute -o a
a1.c: В функции «v_log»:
a1.c:11:9: предупреждение: этой функции, вероятно, можно задать атрибут форматирования gnu_printf [-Wmissing-format-attribute]
```

Я подумал, что неплохо бы всунуть в сборку всех наших проектов этот
чудоключ и попробовать их пособирать. Нашел много интересных ошибок в
реальном коде. Утечка форматной строки из пользовательского кода
(похожих ошибок много):

```diff
@@ -57,9 +60,9 @@ public:
        }
        static void ThrowError(void *ud, const SQChar *s) {
                SQCompiler *c = (SQCompiler *)ud;
-               c->Error(s);
+               c->Error("%s", s);
        }
-       void Error(const SQChar *s, ...)
+       void Error(const SQChar *s, ...) GCCISM(__attribute__ ((format (printf, 2, 3))))
        {
                static SQChar temp[256];
                va_list vl;
```

Просто мусор в обработчике ошибки (похожих ошибок много). Бедный
пользователь не узнал бы, почему у него случается `SIGSEGV`:

```diff
@@ -926,7 +931,7 @@ void check_cgp( void ) {

             pthread_t thread;
             if( pthread_create( &thread, &thread_attr, &cgp_check_thread, cgpfile ) )
-                mexit( EXIT_FAILURE, "pthread_create(): %s", errno );
+                mexit( EXIT_FAILURE, "pthread_create(): %s", strerror(errno) );
         }
         else if( !strcmp( command, "QUIT" ) ) {
             cgp_response( NULL, "QUITTING" );
```
