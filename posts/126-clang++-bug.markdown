---
title: clang++ stdcall bug
date: October 25, 2010
---

Наконец-то собрав чудопроект **clang**'ом я отважился его запустить и
тут-же получил **SIGSEGV**. Отлов самого бага занял у меня почти сутки.
Я знал, что ищу, но я не мог гарантированно воспроизвести вылет.

Рассмотрим такой исходник:

~~~~ { .cpp }
//====---- interface.hpp ----=====//
#ifndef __interface_hpp__
#define __interface_hpp__

#define FN_ARGS int a, int b, int c, int d, int e, int f
#define FN_ARGV     1,     2,     3,     4,     5,     6

extern "C" void foo (void);

class I {
  public:
    virtual void Startup(FN_ARGS) __attribute__((stdcall));
};

#endif // __interface_hpp__
//====---- interface.cc ----=====//
#include "interface.hh"

void I::Startup(FN_ARGS) //STDCALL
{
    foo();
}
//====---- main.cc ----=====//
#include <stdio.h>
#include "interface.hh"

void foo (void) { }

int main (int argc, char * * argv)
{
    I _d;
    _d.Startup(FN_ARGV);
    return 0;
}
~~~~

Итак, мы тут видим:

- **I::Startup()** - **stdcall** функция с **6** параматрами
- **foo()** - **cdecl** функция с **0** параметрами

Запускаем всё это таким злым скриптом (генерим 32-битный код,
так как на **amd64** **stdcall** ничего не значит и там всё работает):

~~~~ { .sh }
#!/bin/sh

export LANG=C

rm -rf *.o

CXX=clang++; OEXT=o_clang
#CXX=g++; OEXT=o_gcc
CXXFLAGS="-m32 -g -O2 -fomit-frame-pointer"

for f in *.cc
do
    $CXX $CXXFLAGS -c "$f" -o "$f.$OEXT"
done

$CXX     $CXXFLAGS *.$OEXT -o prog

./prog

~~~~

~~~~
$ ./mk.sh
./mk.sh: line 18:  5606 Segmentation fault      ./prog
~~~~

Ух, свалилось. Посмотрим во что собралась наша **I::Startup()**:

**g++**:

~~~~
$ objdump -d -S interface.cc.o_gcc 

00000000 <_ZN1I7StartupEiiiiii>:
//====---- interface.cc ----=====//
#include "interface.hh"

void I::Startup(FN_ARGS) //STDCALL
   0:   83 ec 0c                sub    $0xc,%esp
{
   3:   e8 fc ff ff ff          call   4 <_ZN1I7StartupEiiiiii+0x4>
    foo();
   8:   83 c4 0c                add    $0xc,%esp
   b:   c2 1c 00                ret    $0x1c

~~~~

**g++** правильно реализует **stdcall** конвению: вызывает функцию **foo()** и сам чистит за собой стек:

**ret $0x1c** - выталкивает со стека **7** параметров (**6** явных + **this**)

**clang++**:

~~~~
objdump -d -S interface.cc.o_clang 

00000000 <_ZN1I7StartupEiiiiii>:
//====---- interface.cc ----=====//
#include "interface.hh"

void I::Startup(FN_ARGS) //STDCALL
{
    foo();
   0:   e9 fc ff ff ff          jmp    1 <_ZN1I7StartupEiiiiii+0x1>

~~~~

Халтура! **I::Startup()** не трогает стек, будто мы в **cdecl** функции. Естественно, это ведет
к тому, что после возврата из функции **foo()** мы вернемся в **main()**, но у него будет сдвинутый
на **28** байт стек. Код возврата из **main()** свалится и мы получим наш **SIGSEGV**.

- [clang bug](http://llvm.org/bugs/show_bug.cgi?id=8461)
- [fixed gcc bug](http://gcc.gnu.org/bugzilla/show_bug.cgi?id=40718)
- [fixed gentoo gcc bug](https://bugs.gentoo.org/show_bug.cgi?id=282189)
