---
title: clang++ features
date: October 19, 2010
---

Недавно тут вышел [clang-2.8](http://llvm.org/releases/2.8/docs/ReleaseNotes.html),
в котором таки пофиксили зарерпорчееный мной
[баг про -fno-rtti и try/catch](http://llvm.org/bugs/show_bug.cgi?id=6974).

Это должно было позволить собрать с помощью **clang++** большой **C++** проект на работе.

**clang++** споймал пару прикольных подозрительных кусков кода (все примеры - реальные):

1. Использование логических (**|**, **&**) операций вместо булевых (**||**, **&&**):


~~~~ { .cpp }
 int CheckFileMask (const char * lpPath)
 {
     int result = CheckPathName (lpPath);
     // Что-то делается ...
     return result || 0x00000002;
 }
 ...
 bool SetDOSMask (char * lpMask)
 {
     // Что-то делается ...
     int result = CheckFileMask (lpMask);

     if( result && 0x00000001 != 0 )
     {
         // обрабатываем бит '0'
     }
     if( result && 0x00000002 != 0 )
     {
         // обрабатываем бит '1'
     }

     return result >= 0;
 }
~~~~

**Clang** говорит:

    use of logical || with constant operand; switch to bitwise | or remove constant
        [-Wconstant-logical-operand]
      return result || 0x00000002;

исправляем:

~~~~ { .cpp }
 int CheckFileMask (const char * lpPath)
 {
     int result = CheckPathName (lpPath);
     // Что-то делается ...
     return result | 0x00000002;
 }
 ...
 bool SetDOSMask (char * lpMask)
 {
     // Что-то делается ...
     int result = CheckFileMask (lpMask);

     if (result & 0x00000001)
     {
         // обрабатываем бит '0'
     }
     if (result & 0x00000002)
     {
         // обрабатываем бит '1'
     }

     return result >= 0;
 }
~~~~

2. Неиспользуемый результат в выражении:

Оригинал:

~~~~ { .cpp }
     while( ( * lpTmp == ' ' ) || ( * lpTmp == '\t' ) ) * lpTmp ++;
~~~~

Предупреджение:

    boot/emucfg.cpp:244:56: warning: expression result unused [-Wunused-value]
        while( ( * lpTmp == ' ' ) || ( * lpTmp == '\t' ) ) * lpTmp ++;
                                                           ^ ~~~~~~~~
Исправляем:

~~~~ { .cpp }
     while( ( * lpTmp == ' ' ) || ( * lpTmp == '\t' ) ) lpTmp ++;
~~~~

Было еще несколько красивых ошибок типизации, про которые лень писать.

3. И самое интересное: **clang++** правильно реализует **ADL** (argument dependent lookup),
   в отличие от **g++**.

Рассмотрим пример из бага, который я [завел](http://gcc.gnu.org/bugzilla/show_bug.cgi?id=46075)
и тут же огрёб:

~~~~ { .cpp }
template<typename T> struct my_T {
    my_T() {
        my_foo ((const T *)0);
    }
};

struct User {
    User(); // explicit c-tor
    my_T<char> t;
};

// we specialize my_foo for 'const char *'
static void my_foo (const char *) {}

User::User() { }

int main() {
    User u;
    return 0;
}

~~~~

Этот код **g++** собирает, а **clang++** - нет:

    a.cc:4:9: error: use of undeclared identifier 'my_foo'
            my_foo ((const T *)0);
            ^
    a.cc:16:7: note: in instantiation of member function 'my_T<char>::my_T' requested here
    User::User() { }

**Внимательно** читаем по [ссылке](http://blog.llvm.org/2009/12/dreaded-two-phase-name-lookup.html).
Читаем еще раз!

Тут мы видим, что:

1. **my_foo()** в шаблоне явно зависит от параметра шаблона
2. значит имя является зависимым
3. то есть его поиск производится в точке инстанцирования (определении конструктора **User::User()**)
4. значит **my_foo()** должно находиться при инстанцировании

А вот и нет! **4.** пункт является верным только для типов, находящихся в пространстве имён **my_T** или пространстве
имён точки инстанцирования.

Но(**!**) встроенные типы(**int**, **char**, etc.) находятся в "нигде" (ни в каком из пространств имён)
и, как следствие, не подлежат поиску в **ADL** при специализации (это текущая формулировака поиска Кёнига
в стандарте). Вот если бы я вместо **char** объявил свой тип - всё было бы хорошо:

~~~~ { .cpp }
template<typename T> struct my_T {
    my_T() {
        my_foo ((const T *)0);
    }
};

struct myO {};

struct User {
    User(); // explicit c-tor
    my_T<myO> t;
};

static void my_foo (const myO *) {}

User::User() { }

int main() {
    User u;
    return 0;
}
~~~~

**g++** с таким положением дел не согласен и считает это
[ошибкой в стандарте](http://www.open-std.org/JTC1/SC22/WG21/docs/cwg_active.html#225).
Правда, его в этом никто не поддерживает уже N лет.

**clang++** считает, что стандарту 10 лет, формулировка не менялась и по сему можно реализовать
в точности с формулировкой:

    you'll need to move the declaration up, or instantiate my_T with a non-builtin type (so that 
    argument-dependent lookup can find my_foo in the namespace of that type)

    so builtin types are special?

    not special, really; they have no "associated namespaces", i.e., there's nowhere for the compiler to look 
    at instantiation time

    do coercions work in such case? Will 'static void my_foo(const void *) {}' for my user's types?

    implicit conversions do work
    static functions might not, though; I'd have to check the standard

    the C++ DR in question is at http://www.open-std.org/jtc1/sc22/wg21/docs/cwg_active.html#225 ,
    and apparently hasn't been touched in 10 years. me, I'll stick with what the standard says :)

Для достижения портабельности придется декларацию **my_foo()** вытаскивать раньше шаблонного кода.

Спасибо **dgregor** на **oftc/#llvm** и **Andrew Pinski** в
[gcc bugzilla](http://gcc.gnu.org/bugzilla/show_bug.cgi?id=46075) за разьяснения.
