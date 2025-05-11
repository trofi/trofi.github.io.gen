---
title: c and small ints
date: December 17, 2010
---

Задал мне сегодня друг веселую задачку:

~~~~ { .c }
uint16_t a = 0xFFFF, b = 0xFFFF; uint64_t c = a * b;
~~~~

Вопрос: *что будет в 'c' при выполнении этого кода?*

Он же и сказал правильный ответ:

~~~~ { .c }
#include <inttypes.h>
#include <stdio.h>

int main()
{
    uint16_t a = 0xFFFF,
             b = 0xFFFF;
    uint64_t c = a * b;
    printf("%" PRIX64 "\n", c);
    return 0;
}
~~~~

Практика показывает:

~~~~ { .sh }
$ ./test
FFFFFFFFFFFE0001
~~~~

С учетом продвижения интегральных типов до `int` результат будет вычисляться так:

~~~~ { .c }
uint64_t c = (uint64_t)(int)((int)a * (int)b);
~~~~

`(int)0xFFFF * (int)0xFFFF` даст отрицательное число `0xFFFE0001` (если `int` - 32 бита),
которое потом знакорасширится в 64 бита: `0xFFFFFFFFFFFE0001`
