---
title: "debuigging LTO builds"
date: May 19, 2020
---

## `LTO`

Link-Time Optimizations a way to unlock many inter-procedural
optimizations across modules (across `.c` files). Normally these
optimizations applied within a file.
It is a great opportunity to get smaller and faster programs for free:
dead code elimination and inliner sees the whole program. And it is also
an endless pit of bugs you can get yourself into without an easy way to
debug it: changes in one `.c` file can expose surprising behavior in
another `.c` file. That is not very friendly to `printf` debugging
technique.
But not all hope is lost! We can get a lot of detail back from the
compiler if something goes wrong.

This post will focus on build-time debugging (opposed to run-time
debugging).

## Simple example

Let's see what we can infer just by looking at how `gcc` builds a tiny
example.

``` c
// one-two.c
int  one(void) { return 1; }
int  two(void) { return 2; }
```

``` c
// three-four.c
int three(void) { return 3; }
int  four(void) { return 4; }
```

``` c
// main.c
extern int  one(void);
extern int four(void);
//
int main() { return one() + four(); }
```

Here we defined three modules where each has a redundant unused
function. Let's see what ends up being in the final binary.

``` 
$ gcc -O1    -flto -c one-two.c    -o one-two.o
$ gcc -O1 -fno-lto -c three-four.c -o three-four.o
$ gcc -O1    -flto -c main.c       -o main.o
$ gcc        -flto    main.o one-two.o three-four.o -o main
./main; echo $?
5
```

I built `one-two.c` with `LTO` bytecode and `three-four.c` without
`LTO` bytecode. Both files are otherwise symmetric.

``` 
$ objdump -d main
...
0000000000001125 <main>:
    1125: 48 83 ec 08          sub    $0x8,%rsp
    1129: e8 0e 00 00 00       callq  113c <four>
    112e: 83 c0 01             add    $0x1,%eax
    1131: 48 83 c4 08          add    $0x8,%rsp
    1135: c3                   retq

0000000000001136 <three>:
    1136: b8 03 00 00 00       mov    $0x3,%eax
    113b: c3                   retq

000000000000113c <four>:
    113c: b8 04 00 00 00       mov    $0x4,%eax
    1141: c3                   retq
...
```

A few things happened here:

1.  `one()` function from `one-two.c` was inlined directly into
    `main` as `add $0x1,%eax`
2.  `two()` assembly code was not generated at all
3.  redundant unused `three()` body was generated
4.  `four()` was not inlined into `main()` and is left as `callq
    four`

Let's check how internal representation looks like for `LTO` objects with
`lto-dump`:

``` 
$ lto-dump -list main.o
Type   Visibility  Size  Name
function  default     0  one
function  default     0  four
function  default     3  main

$ lto-dump -dump-body=main main.o
Gimple Body of Function: main
main ()
{
  <bb 2> [local count: 1073741824]:
  _1 = one ();
  _2 = four ();
  _6 = _1 + _2;
  return _6;
}

$ lto-dump -dump-body=one one-two.o
Gimple Body of Function: one
one ()
{
  <bb 2> [local count: 1073741824]:
  return 1;
}
```

Here we see direct intermediate representation of `main()` function.
Both function calls are present. Variable assignments like `_6 = _1 +
_2;` are very verbose, but also very simple.
For large programs it's frequently very hard to say which symbols come
from where, where they are defined and whether bytecode (`IR`) version
is available for `LTO`. You can peek at `resolution` files left by
`-save-temps` `gcc` flag:

``` 
$ gcc -flto main.o one-two.o three-four.o -o main -save-temps
$ cat -- *.res
2
main.o 3
191 b0674bfb5d86b316 PREVAILING_DEF main
194 b0674bfb5d86b316 RESOLVED_EXEC four
196 b0674bfb5d86b316 RESOLVED_IR one
one-two.o 2
190 e8fe3cc27eebcea2 PREVAILING_DEF_IRONLY one
192 e8fe3cc27eebcea2 PREVAILING_DEF_IRONLY two
```

In the output we see:

- `RESOLVED_IR` - symbol is referred and is known to have `IR` somewhere
  else
- `PREVAILING_DEF_IRONLY` - symbol is defined and has `IR` only
- `RESOLVED_EXEC` - symbol is referred and is known to have machine
  code lying somewhere around

And so on. Some numbers are count of entries in the lists, some are type
numbers.
Check out `lto-dump` source code at
<https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/lto/lto-dump.c#l61> and
`man lto-dump`. I think it's worth documenting more things in the
manual.
There is a minor wart here: the exact name of `.res` file is hard to
predict: <https://gcc.gnu.org/PR95224>. I suspect it's very easy to fix.
We can also get the call graph of references:

``` 
$ lto-dump -callgraph *.o
digraph symtab {
    "main/0" -> "four/2"
    "main/0" -> "one/4"
}

$ lto-dump -callgraph *.o | dot -Tsvg -o cg.svg
```

## Real world example

In <https://bugs.gentoo.org/723370> Andrew reported a mysterious
`LTO`-only link failure of `gcc` itself. It should not happen as `gcc`
is regularly tested on `LTO` builds. And yet:

``` 
ld: gcc-10.1.0/temp/genmatch.NlicMk.ltrans2.ltrans.o:
  in function `main':
    <artificial>:(.text.startup+0x94ba):
      undefined reference to `libintl_dgettext'
```

Looks like a simple error of trying to use undefined
`libintl_dgettext` symbol. The only annoyance is that it's unclear
what actually tries to pull that undefined symbol in.
`ltrans2.ltrans.o` is a large intermediate object file pulled together
from many small object files.
To simplify the problem we can enable debugging symbols when building
`gcc`. Gentoo provides an easy way to do it:

``` 
$ STAGE1_CFLAGS="$(portageq envvar CFLAGS) -ggdb3" STAGE1_CXXFLAGS="$(portageq envvar CXXFLAGS) -ggdb3" emerge -v1 sys-devel/gcc
...
..././prev-gcc/xg++ ... -o build/genmatch ... ./../intl/libintl.a

ld: /tmp/genmatch.FJdDDJ.ltrans2.ltrans.o: in function `main':
  gcc-10.1.0/libcpp/files.c:1439:
    undefined reference to `libintl_dgettext'
```

Now we can see where that reference comes from: `libcpp/files.c:1439`.
Rerunning above `xg++` command with appended `-save-temps`
generates useful `.res` file:

``` 
$ ..././prev-gcc/xg++ ... -o build/genmatch ... ./../intl/libintl.a -save-temps
...
$ cat -- *.res | fgrep libintl_dgettext
<nothing!>
```

No references! Let's see how that symbol is defined and how it gets
called:

``` 
# call site
$ prev-gcc/lto-dump -list libcpp/files.o  | fgrep dgettext
function  default     0  *libintl_dgettext

$ prev-gcc/lto-dump -list -demangle libcpp/files.o  | fgrep dgettext
function  default     0  dgettext


# definition site
$ prev-gcc/lto-dump -list intl/dgettext.o  | fgrep dgettext
function  default     3  libintl_dgettext
$ prev-gcc/lto-dump -list -demangle intl/dgettext.o  | fgrep dgettext
function  default     3  libintl_dgettext
```

Here is my interpretation of the above:

1.  Definition site looks at expected: it's a nice `libintl_dgettext`
    symbol.
2.  Call site is strange: depending on `-demangle` option it
    completely changes it's name.

This means `dgettext` prototype is not a typical prototype you
would expect:

``` c
extern char *dgettext (const char *__domainname, const char *__msgid)
   __asm__ ("libintl_dgettext");
```

It's a `gcc` extension (`TODO`: which one?) to emit references to
`dgettext` `c` function via `libintl_dgettext` assembly labels.
`gcc` linker plugin does not know that `libintl_dgettext`
reference exists as `LTO` phase is ran before an assembler phase. `gcc`
currently does not understand effect of the assembly symbol mangling.
Thus the fix here is either avoid assembly or disable `LTO` for all
callers of this `__asm__` declaration.
<https://gcc.gnu.org/PR95194> suggests a proof-of-concept style
workaround to avoid `__asm__` symbols in `libintl`.

``` diff
--- a/intl/libgnuintl.h
+++ b/intl/libgnuintl.h
@@ -84,25 +84,25 @@ extern "C" {
    Since Solaris gettext() behaves differently than GNU gettext(), this
    would be unacceptable.

    The redirection happens by default through macros in C, so that &gettext
    is independent of the compilation unit, but through inline functions in
    C++, in order not to interfere with the name mangling of class fields or
    class methods called 'gettext'.  */

 /* The user can define _INTL_REDIRECT_INLINE or _INTL_REDIRECT_MACROS.
    If he doesn't, we choose the method.  A third possible method is
    _INTL_REDIRECT_ASM, supported only by GCC.  */
 #if !(defined _INTL_REDIRECT_INLINE || defined _INTL_REDIRECT_MACROS)
-# if __GNUC__ >= 2 && !defined __APPLE_CC__ && (defined __STDC__ || defined __cplusplus)
+# if __GNUC__ >= 2 && !defined __APPLE_CC__ && (defined __STDC__ || defined __cplusplus) && USE_ASM_ALIASES_THAT_BREAK_LTO
 #  define _INTL_REDIRECT_ASM
 # else
 #  ifdef __cplusplus
 #   define _INTL_REDIRECT_INLINE
 #  else
 #   define _INTL_REDIRECT_MACROS
 #  endif
 # endif
 #endif
 /* Auxiliary macros.  */
 #ifdef _INTL_REDIRECT_ASM
 # define _INTL_ASM(cname) __asm__ (_INTL_ASMNAME (__USER_LABEL_PREFIX__, #cname))
```

## Parting words

`lto-dump` and `.res` files make it very easy to debug `LTO`
build failures and explore `LTO` build process and there are a few
useful hints:

- `-g` and friends make linker errors more comprehensible of what went
  wrong
- `-save-temps` dumps a lot of precise information where used symbols
  are located (and how they are partitioned when parallel `LTO` building
  happens).
- some advanced tricks with `asm` symbols don't quite work for `LTO`
  and sometimes require a fallback to `c` code or non-`LTO` build.

Have fun!
