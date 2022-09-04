---
title: "Porting to pure Bison API"
date: September 04, 2022
---

A few weeks ago I started code cleanup for
[Ski](https://github.com/trofi/ski/commits/master).
Upstream `Ski` released in 2007 last time. I think it was actively
developed in 2003. That makes `Ski` about a 20 years old codebase.
It was more of a proof-of-concept makeshift project to help early
adopters than a piece of finished and maintained work. But it works
surprisingly good even today.

I would like to add a few medium sized features to `Ski`. I am a bit
afraid to make non-trivial changes. I decided to do do minor cleanup
first.

To deal with code health of old projects that I pretend to own (and thus
have the ability to do large cleanup changes) I have a few hacks
available. It mostly boils down to enabling a few compiler warnings to
use as suggestions for making things a bit more explicit.

I'll outline some tricks I used for `Ski` and then will focus on
`Bison` API update.

## Simple tricks

C has a few language warts that allow you write code that is very likely
incorrect and yet standard compliant. A good example is implicit
function declaration behaviour. Luckily `gcc` has a set of warning to
catch those.

### Implicit variable and function declarations

To catch implicit function and variables `gcc` has at least the following
flags:

- `-Werror=implicit-int`
- `-Werror=implicit-function-declaration`
- `-Werror=strict-prototypes`

I used these warnings successfully in
[xmms2](https://github.com/xmms2/xmms2-devel/commit/1dc66e4099e5b08f59bca86d7979f057fd82eba7),
[linux kernel](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=80970472179a45609c0b11b80619bc8c32b15f77)
and and other projects.

Linux kernel is especially prone to these bugs as it heavily uses top
level macros a lot. lack of header inclusion for such a macro usually
turns macro call into a function declaration without any build failure.

Related build failures are still being fixed in `linux` kernel to this
day. Header inclusion changes are happening there all the time to speed
builds up and huge amount of `CONFIG` options increases the chance of
detting into a combination where a few headers got lost.

I'd say it's a must have set of warning flags for a `C`-based project.

### Modernized configure.ac/Makefile.am

If the outdated project is `autotools`-based then chances are it uses
many deprecated and invalid constructs. Sometimes a project has complex
`./autogen.sh` script. My goal is usuallly to turn that script into a
single `autoreconf -i -f -W all` invocation. Ideally `configure.ac` and
`Makefile.am` should be enough to configure all the other details.
`-W all` helps catching deprecated macros and other lint errors.

For `Ski` I did the following changes:

- ported from autoconf `2.13`-ish to `autoconf-2.70`
- used `AC_CONFIG_AUX_DIR([build-aux])` to move most auxiliary files
  into a `build-aux/` subdirectory
- used `AM_SILENT_RULES([yes])` to make builds less verbose to make
  warnings stand out
- made `Makefile.am` non-recursive to make cross-directory dependencies
  simpler
- used `ACLOCAL_AMFLAGS = -I m4 -I macros --install` to vendor external
  `m4` macros as separate `.m4` files instead of globbing them together
  into a single `aclocal.m4`

### Catch inconsistent function prototypes and mark functions local

`Ski` is a bit special in a way it handles prototypes. For an example
file `foo.c` with `void foo(void)` function there was usually no
according `foo.h` available. Instead various `bar.c` and `baz.c` files
had their own local imports of `void foo(void)` duplicating the prototype.

The scheme has a few problems: build does not break when you change `foo()`'s
prototype in `foo.c` alone, 10x prototype duplication is common making
it harder to change, function prototypes don't always agree on the
argument count a function has ([example](https://github.com/trofi/ski/commit/c4de04f3b27424dd0caf5e89bf93ceab8cd59af1)).

To find such stray prototypes I use a few tricks:

- Use `-Wmissing-declarations` to detect cases where `foo.c` does
  not have according header to include to verify prototype consistency.
  That way I spot lack of `foo.h` headers, create them and consolidate
  such prototype there. It's also a good time to sprinkle `static`
  keyword if a symbol happens to be used only in a single `.c` file.
- Use `-flto` to detect prototype mismatches across such `.c` files.
- Use `-Wunused-function -Wunused-variable` to detect unused
  newly marked `static` functions.
- Use [uselex.rb](https://trofi.github.io/posts/186-announce-uselex.rb-useless-exports-extinguisher.html)
  to spot more needlessly exported symbols to sprinkle even more `static`
  annotations around.
- Use `-ffunction-sections -fdata-sections -Wl,--gc-sections -Wl,--print-gc-sections`
  to catch even more unused code and variables where `uselex.rb` was not
  able to do it. Linkers are som much better at traversing graphs :)

### Make headers self-contained and minimal

When I start adding `#include "foo.h"` around I frequently notice that
some of them are incomplete and require including other headers themselves.
I usually use syntax-check the headers to fund such cases:

```
$ for h in $(find -name '*.h'); do
    gcc -DHAVE_CONFIG_H=1 -fmax-errors=1 -I. -Isrc{,/decoder,/encoder} -fsyntax-only -c $h ||
      echo "BROKEN H: $h"
  done
```

To get rid of unused header inclusions I usually use
[include-what-you-use](https://github.com/include-what-you-use/include-what-you-use).

### Switch from lex and yacc to flex and bison APIs

`lex` and `yacc` interfaces have a few warts in their APIs. The major
one I would say is that both heavily use global variables to pass data
from one to another (and for user to write semantic actions):
`yylval`, `yyin`, `yyout` and a bunch of helper macros to work with them.

One of immediate benefits of switching from `yacc` to `bison` is ability
to enable `--warnings` reporting.
It is able to flag various grammar deciciencies like [this one](https://github.com/trofi/ski/commit/89c94225c3b4851f09daa54c5b0286a5726c6af0),
or [this one](https://github.com/skvadrik/re2c/commit/7e7c4b97af51f5e343faccacde2a58b9da5a1192).

More advanced benefit is the opportunity to switch to `pure` API: `pure` is the
one that uses function parameters to pass things around instead of global
variables. A few switch examples are [this one](https://github.com/trofi/ski/commit/5af00cd51f99127979280278f30cabd2de61d74a)
and [this one](https://github.com/skvadrik/re2c/commit/8161d996f0ae0b7f782fff602bc261667c3e95d2).

The gist of it is move away from global `yylval` to explicit parameter threading.

## Bison APIs

When I fist tried to switch `Ski` to `pure` `Bison` API I failed miserably.
I did not know what `Bison` generates, what `flex` generates and what
user is supposed to define. I also enabled function rename from `yy*()`
to `expr*()` and quickly got lost in errors and header inclusion cycles.

I stepped back and crafted simple pair of lexer and parser examples.
Then gradually upgraded them to modern world discovering minor API
gotchas one at a time.

The experience allowed me to finally port both `Ski` and `re2c` to more
modern `Bison` API.

### Simple example

Our running example will be the following trivial grammar:

```
<expression> ::= <digit> | <expression> "+" <digit>
<digit> ::= "0" | "1" | ... | "9"
```

It allows us write an expression of a digit (like `"1"` or `"2"`) or an
arbitrary sum of digits (like `"1+2+3"`). We will also allow whitespace
around.

We'll need 3 files:

- `Bison` parser: `p.y`
- `Flex` lexer: `l.l`
- `main()` function: `main.c`

Simple implementation would be the following:

```c
/* $ cat p.y */
%{
#include <stdio.h>

#include "lex.expr.h"
/* local declarations */
static void yyerror (const char * err);
%}

%union {
    int ival;
}

%token <ival> NUM
%type <ival> expr

%%
result : expr       { printf("RESULT: %d\n", $1); }
expr : NUM          { $$ = $1; }
     | expr '+' NUM { $$ = $1 + $3; printf("I-RESULT: %d\n", $$); }
     ;

%%
void yyerror (const char * err) { fprintf(stderr, "PARSE ERROR: %s\n", err); }
```

Here we handle our grammar almost as it's written in `BNF`. A few things
to note:

- Tokens are represented as instances of `union YYSTYPE { int ival; }`.
  Usually tokens are more complicated and have a few more union
  alternatives.
- Our parse result is the `stdout` output of intermediate and final
  computation step. We don't collect any of the syntax information here.

Note that parser's definition requires lexing function prototype. Thus we
include all of autogenerated header via `#include "lex.expr.h"`.

Moving on to lexer:

```c
/* $ cat l.l */
%option noyywrap
%option nodefault
%{
#include "parse.expr.h"
%}
%%

[ \t\n] { /* skip whitespace */ ; }
[+] { return '+'; }
[0-9] { yylval.ival = yytext[0] - '0'; return NUM; }

. { return YYUNDEF; }
```

The lexer is trivial:

- We support a few whitespace types (tab, space and newline). Whitespace
  has no token representation. We just skip through them in this example.
- Actual tokens are `'+'` operation (returned as is) and a digit returned
  via `YYSTYPE yylval` global variable of type `union YYSTYPE { int ival; }`.

Note that lexer's implementation requires `union YYSTYPE` declaration provided
by `"parse.expr.h"` (while parser's implementation clearly requires lexer's
`yylex()`declaration). Thus we pull all of autogenerated lexer header via
`#include "parse.expr.h"`.

And our `main()` function:

```c
/* $ cat main.c */
#include <stdio.h>

#include "parse.expr.h"

int main(int argc, char * argv[])
{
    printf("Parsing input from stdin. Press ^D when finished.\n");
    yyparse();
    return 0;
}
```

It's literally just one `yyparse()` call! Let's build it:

```
$ bison --warnings -Wcounterexamples --output=parse.expr.c --header=parse.expr.h --warnings p.y
$ flex --outfile=lex.expr.c --header-file=lex.expr.h l.l
$ gcc lex.expr.c parse.expr.c main.c -o example1
```

The program happens to work on it's stdin:

```
$ ./example1
Parsing input from stdin. Press ^D when finished.
1       +2
I-RESULT: 3
+4
I-RESULT: 7
^D
RESULT: 7
```

Here we typed `1+2+4` (with a bit of whitespace) and saw intermediate
results of `1+2` and intermediate/final result of the whole expression.

Fun fact: thanks to left recursion our grammar allows evaluating parts
of the expression before full expression is available.

Let's have a peek at defined non-code symbols (data, rodata, undefined)
of generated files:

```
$ gcc -c lex.expr.c
$ nm lex.expr.o | grep -v -P 't|T'
                 U clearerr
                 U ferror
                 U fileno
                 U fread
                 U free
                 U malloc
                 U realloc
0000000000000130 r yy_base
0000000000000048 b yy_c_buf_p
0000000000000190 r yy_chk
0000000000000150 r yy_def
0000000000000020 r yy_ec
0000000000000018 B yy_flex_debug
0000000000000040 b yy_hold_char
0000000000000008 B yyin
0000000000000000 B yyleng
0000000000000000 D yylineno
                 U yylval
0000000000000044 b yy_n_chars

$ gcc -c parse.expr.c
$ nm parse.expr.o | grep -v -P 't|T'
                 U free
                 U malloc
                 U memcpy
0000000000000000 B yychar
000000000000011b r yycheck
                 U yylex
0000000000000004 B yylval
0000000000000008 B yynerrs
0000000000000126 r yyr1
000000000000012b r yyr2
```

Lexer defines quite a few globals: `yyin`, `yyleng`, `yylineno`, `yy_flex_debug`.
Parser does not define as many, but still has a few: `yychar`, `yylval`, `yynerrs`.

I did not realize it's so much state scattered around.

### Pure example

Let's now turn our example to string input (instead of `stdin` input)
and switch to pure API.

The main change is:

- add `%option reentrant bison-bridge` to `flex` lexer
- add `%define api.pure full`
- make it all compile

The above set of directives extends `int yylex(void)` with extra
parameters passed around. But not `yyparse()`! That will require
explicit extension with `%param {yyscan_t scanner}` (as it may be
lexer-dependenct).

Here is an updated parser:

```c
/* $ cat p.y */
%define api.pure full
%param {yyscan_t scanner}

%code requires {
    typedef void * yyscan_t;
}

%{
#include <stdio.h>

#include "parse.expr.h"
#include "lex.expr.h"

/* local declarations */
static void yyerror (yyscan_t scanner, const char * err);
%}

%union {
    int ival;
}

%token <ival> NUM
%type <ival> expr

%%
result : expr       { printf("RESULT: %d\n", $1); }
expr : NUM          { $$ = $1; }
     | expr '+' NUM { $$ = $1 + $3; printf("I-RESULT: %d\n", $$); }
     ;

%%
static void yyerror (yyscan_t scanner, const char * err) { fprintf(stderr, "PARSE ERROR: %s\n", err); }
```

Compared to original example `%param {yyscan_t scanner}` extends
`yyparse()` declaration with `yyscan_t scanner` parameter (and also pass
it to every `yy*()` call including `yylex()`, `yyerror()` and many others).

We include extra `#include "parse.expr.h"` to make sure generated header
has the same prototype as `.c` file that implements it.

You might have noticed that `typedef void * yyscan_t;` bit. It unties
circular dependency between `"parse.expr.h"` header and `"lex.expr.h"`
header. Figuring out specific details of the dependency is an exercise
to the reader. Try to remove it and see what breaks. That error threw
me off when I initially tried `Ski` conversion.

Luckily `flex` guarantees that `yyscan_t` is an opaque type and will
always be `typedef void * yyscan_t;`. Thus we can open code it's
declaration directly.

Note that we still print our results to `stdin`. In a real world example
you would probably want to pass another parameter to store final result via
something like `%parse-param {output_t * output}`.

Updated lexer:

```c
/* $ cat l.l */
%{
#include "parse.expr.h"
%}

%option warn
%option noyywrap
%option nodefault
%option reentrant
%option bison-bridge
%%

[ \t\n] { /* skip whitespace */ ; }
[+] { return '+'; }
[0-9] { yylval_param->ival = yytext[0] - '0'; return NUM; }

. { return YYUNDEF; }
```

Semantic action now uses `union YYSTYPE * yylval_param`. It is added by
`%option bison-bridge` to `yylex()` signature. `%option reentrant` has
another effect on the `yy*()` API: it adds `yyscan_t scanner` parameter.
Let's look at the `yy*()` call site in `main()` function:

```c
/* $ cat main.c */
#include <stdio.h>

#include "parse.expr.h"
#include "lex.expr.h"

int main(int argc, char * argv[])
{
    printf("Parsing input from argv:\n");

    for (int i = 1; i < argc; ++i) {
        printf("argv[%i]='%s':\n", i, argv[i]);

        yyscan_t scanner;
        YY_BUFFER_STATE buf;

        yylex_init (&scanner);
        buf = yy_scan_string (argv[i], scanner);
        yyparse(scanner);

        yy_delete_buffer(buf, scanner);
        yylex_destroy (scanner);

    }
    return 0;
}
```

Here `flex`'s `%option reentrant` option requires us to thread
`yyscan_t scanner`. `scanner` holds full lexing context and does not
rely on any global variables.

The `YY_BUFFER_STATE buf; buf = yy_scan_string (..., scanner); yy_delete_buffer(buf, scanner);`
is the `flex` way to switch from `FILE *` based API to `const char *`
as an input buffer. It is not directly related to `Bison`'s `pure` API.

Let's check out used globals now:

```
$ gcc -c lex.expr.c
$ nm lex.expr.o | grep -v -P 't|T'
                 U clearerr
                 U ferror
                 U fileno
                 U fread
                 U free
                 U malloc
                 U realloc
0000000000000130 r yy_base
0000000000000190 r yy_chk
0000000000000150 r yy_def
0000000000000020 r yy_ec

$ gcc -c parse.expr.c
$ nm parse.expr.o | grep -v -P 't|T'
                 U free
                 U malloc
                 U memcpy
000000000000011b r yycheck
                 U yylex
0000000000000126 r yyr1
000000000000012b r yyr2
```

No globals! `r` are static read-only lexer and parser tables. Yay!

### Bonus: Makefile for pure example

When I was working on an example I wanted to craft the `Makefile` that
tracks the dependencies precisely to rebuild all the artifacts. Be it
`Makefile`, `C` source, or input to any of generators change. Though
straightforward It ended up being wordy:

```Makefile
# $ cat makefile
FLEX = flex
BISON = bison
BISON_FLAGS = --warnings -Wcounterexamples

TARGETS = a
GENERATED_SOURCES = lex.expr.c lex.expr.h parse.expr.c parse.expr.h
OBJECTS = lex.expr.o parse.expr.o main.o

a: $(OBJECTS)
	$(CC) $(LDFLAGS) $(OBJECTS) -o $@

# generator dependencies and rules

lex.expr.c lex.expr.h: l.l
	$(FLEX) --outfile=lex.expr.c --header-file=lex.expr.h $<

parse.expr.c parse.expr.h: p.y
	$(BISON) $(BISON_FLAGS) --output=parse.expr.c --header=parse.expr.h --warnings $<

# extra build dependencies

$(GENERATED_SOURCES) $(OBJECTS) $(TARGETS): Makefile

lex.expr.o parse.expr.o: parse.expr.h
parse.expr.o: lex.expr.h

main.o: lex.expr.h parse.expr.h

.PHONY: clean

clean:
	$(RM) $(GENERATED_SOURCES) $(OBJECTS) $(TARGETS)
```

It took me a while to populate `extra build dependencies` section but
luckily [make --shuffle](http://trofi.github.io/posts/249-an-update-on-make-shuffle.html)
kept finding the issues until I got something that works most of the time.
Looking at `-MMD` output I think it's an accurate list of extra
dependencies on top of implicit `.c.o` ones.

## Bison version requirements

`Bison`'s `--warnigns` flag was implemented in 2006, around `2.3a`
version. `%define api.pure` flag was implemented in 2007, around `2.3b`
version. Both should be safe to assume as widely available.

## Parting words

Pure `Bison` API is a nice cleanup to do for a project. It should not
take much code to implement: just add `%define api.pure full` and adapt
to API extension. The benefit is a slightly more explicit API readily
usable in multi-threaded and nested parser contexts.

Have fun!
