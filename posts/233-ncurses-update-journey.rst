---
title: "ncurses update journey"
date: January 03, 2022
---

:PostID: 233
:Title: "ncurses update journey"
:Keywords: ncurses, nixpkgs, hydra
:Categories: notes

A few months ago I decided to upgrade a few packages in **nixpkgs** to
the latest available versions upstream. Some packages like **gzip**,
**grep**, **texinfo**, **linuxHeaders** or **terminus_font** were
straightforward to handle. Most of them required 1-2 (or none) reverse
dependencies to fix. Sometimes upstream already had available fixes to pull.

ncurses breakage
----------------

But one surprising package was **ncurses**: upstream had **6.3** version
released for a while while **nixpkgs** had **6.2**. Should be trivial to
update I thought. Probably just a bunch of new terminals added into **terminfo**.

I updated **ncurses** locally and instantly got a few build failures. Most
of those looked similar to the following **mtr** failure:

.. code-block::

    ui/curses.c:765:42:
      error: format not a string literal and no format arguments [-Werror=format-security]
      765 |         mvprintw(rowstat - 1, startstat, msg);
          |                                          ^~~

The build error popped up because new release of **ncurses** enabled
**printf**-like annotations to a few it's frequently used functions:

.. code-block:: diff

    --- a/include/curses.h.in
    +++ b/include/curses.h.in
    @@ -559,7 +557,7 @@
     #ifndef GCC_PRINTFLIKE
    -#if defined(GCC_PRINTF) && !defined(printf)
    +#ifndef printf
     #define GCC_PRINTFLIKE(fmt,var) __attribute__((format(printf,fmt,var)))
     #else
     #define GCC_PRINTFLIKE(fmt,var) /*nothing*/
    @@ -822,2 +816,4 @@
    -extern NCURSES_EXPORT(int) vwprintw (WINDOW *, const char *,va_list) GCC_DEPRECATED(use vw_printw);    /* implemented */
    -extern NCURSES_EXPORT(int) vw_printw (WINDOW *, const char *,va_list); /* implemented */
    +extern NCURSES_EXPORT(int) vwprintw (WINDOW *, const char *, va_list) GCC_DEPRECATED(use vw_printw)    /* implemented */
    +               GCC_PRINTFLIKE(2,0);
    +extern NCURSES_EXPORT(int) vw_printw (WINDOW *, const char *, va_list) /* implemented */
    +               GCC_PRINTFLIKE(2,0);

This change enables **GCC_PRINTF** by default and adds extra annotations
to **vwprintw** and friends. I think **mtr** was broken by **GCC_PRINTF**
default change.

The package fixes are usually trivial. **mtr** one looks like
https://github.com/traviscross/mtr/pull/411:

.. code-block:: diff

    --- a/ui/curses.c
    +++ b/ui/curses.c
    @@ -675,1 +675,1 @@
    -                mvprintw(rowstat - 1, startstat, msg);
    +                mvprintw(rowstat - 1, startstat, "%s", msg);

Does error flag a real problem?
-------------------------------

Is this kind of build errors a big deal or just a nuisance? After all
all these programs probably work just fine if nobody noticed the infelicity.

Let's craft a small example:

.. code-block:: c

    // $cat a.c
    #include <stdio.h>
    #include <stdarg.h>
    
    #if defined(GCC_PRINTF)
    #    define GCC_PRINTFLIKE(fmt,var) __attribute__((format(printf,fmt,var)))
    #else
    #    define GCC_PRINTFLIKE(fmt,var) /* nope */
    #endif
    
    int just_like_printf(const char * fmt, ...) GCC_PRINTFLIKE(1,2);
    int just_like_printf(const char * fmt, ...)
    {
        va_list ap;
        va_start(ap, fmt);
        va_end(ap);
    }
    
    int main(int argc, char *argv[])
    {
        just_like_printf(argv[0]);
    }

.. code-block::

    $ gcc a.c -o a -Wall
    $ ./a
    ./a

Here we just print program's name as is via **printf(argv[0])**.
Does not look too bad, right?

No. Uncontrolled format strings allow for arbitrary memory access.

We can read register and stack values (needs a tiny bit of platform
**ABI** knowledge):

.. code-block::

    $ ln -s a '%#lx%#lx%#lx%#lx%#lx%#lx%#lx%#lx'
    $ ./%#lx%#lx%#lx%#lx%#lx%#lx%#lx%#lx
    ./0x7ffcc15c01c80x7ffcc15c01d80x7fe1dbea759800x7fe1dbea97e00x7ffcc15c01c80x1000000000x401200

And if we are creative we can also write chosen data at addresses in
registers or stack:

.. code-block::

    $ ln -s a '%n%n%n%n%n%n%n%n%n%n'
    $ ./%n%n%n%n%n%n%n%n%n%n
    Segmentation fault (core dumped)

**suid** CLI tools are especially vulnerable as commandline
arguments and environment also reside on stack. We can store
"any" address there just by running the tool with extra
environment variables.

**sudo** had a similiar vulnerability in the past that allows
arbitrary code execution: https://www.vnsecurity.net/research/2012/02/16/exploiting-sudo-format-string-vunerability.html

Mounting a shell exploit on modern systems is an exercise for the reader :)

Thus yes, passing uncontrolled text to **printf()**-like function is a
direct path to arbitrary code execution. In case of networking services
it's an RCE. In case of CLI tools that interpret external input (say,
mp3 players or network packet visualisers) it can easily become
user-assisted RCE.

-Wformat=
---------

This kind of bugs used to be a very common programming error until
compilers learned to catch it when asked:

.. code-block::

    $ gcc a.c -o a -Wall -DGCC_PRINTF -Wformat=2
    a.c: In function 'main':
    a.c:25:5: warning: format not a string literal and no format arguments [-Wformat-security]
       25 |     just_like_printf(argv[0]);
          |     ^~~~~~~~~~~~~~~~

Note that by default it's just a warning that you need to opt into with
**-Wformat=2**. Many distributions enable **-Wformat=2** by default either
in toolchain directly or in package build process.

For quite a while **nixpkgs** enables **-Werror=format-security** by
default which turns warnings on suspicious code into errors:

.. code-block::

    $ gcc a.c -o a -Wall -DGCC_PRINTF -Werror=format-security
    a.c: In function 'main':
    a.c:25:5: error: format not a string literal and no format arguments [-Werror=format-security]
       25 |     just_like_printf(argv[0]);
          |     ^~~~~~~~~~~~~~~~
    cc1: some warnings being treated as errors

Package maintainers usually weed out all these problems when they add a
new package version to the distribution.

But case of **ncurses** is a bit special: it's a very popular decades
old library with many users. For dacedes there were plenty chances
of using it's interfaces in an unsafe manner.

Fixing all the failures
-----------------------

Given that **mtr** had this deficiency how many more tools would be broken
like that? I hoped for "under 10" and started fixing packages one by one.

After fixing 40 packages (**mtr**, **libcdio**, **aewan**, **tty-solitaire**,
**bastet**, **mcabber**, **bwm_ng**, **toxic**, **nudoku**, **ecasound**,
**bemenu**, **tasknc**, **smemstat**, **freesweep**, **gfs2-utils**, **gmu**,
**hexcurse**, **irqbalance**, **hstr**, **pinfo**, **lifelines**, **tty-clock**,
**nethogs**, **host**, **jnettop**, **mp3blaster**, **multitail**, **musikcube**,
**ncdc**, **pacvim**, **powertop**, **profanity**, **sngrep**, **tiptop**,
**trafficserver**, **vimpc**, **wiimms-iso-tools**, **souffle**)
I realized I would not be able to find (or fix) all the breakages myself and would
need help.

Having asked around I found out I could get a **hydra** build run against a
https://github.com/NixOS/nixpkgs/pull/146685 pull request. I requested a run
on **#infra:nixos.org** matrix room.

Vladimir suggested basing the PR against **master** commit with already present
**hydra** run. That way **hydra** UI would allow for easy regression comparison
via "compare-to" drop down. First run (targeted **x86_64-linux** only) took ~5
days: 1 to 6 Dec. It was a full rebuild of all packages as **ncurses** is a
**bash** dependency via **readline**. 5 days felt as very quick given that this
run had low priority against other preempting jobs: builds of **master** itself
and builds of **staging-next**.

The initial run looked promising: https://hydra.nixos.org/eval/1727143?compare=1727080&full=0.

It reported "Newly Failing Jobs (94)". Some of these were unrelated flaky failures
but ~10 of them were more **ncurses** fallouts that I missed from local runs:
**postgresqlPackages.pg_auto_failover**, **rogue**, **wyrd**, **squishyball**,
**haskellPackages.ncurses**, **liboping**, **linuxPackages.sysdig**, **tcptrack**,
**pamix**, **tlf**. Fixing them was straightforward.

While hydra was running through packages I fixed ~10 more:
**_2048-in-terminal**, **liboping**, **noice**, **samtools**,
**libviper**, **vwm**, **gptfdisk**, **spdk**, **nfstrace**.

I hope to get **ncurses-6.3** merged within first days of 2022 \\o/

Does it scale?
--------------

Fixing 60 packages took 2 months. Usually it takes sending a fix
upstream, proposing a pull request to **nixpkgs** and following
up both down to acceptance :)

That gives us one fix per day on average.

I have a vague plan of trying to get **gcc-10** updated to **gcc-11**.
That has order of magnitude more failures lurking. **gcc-12** has even
more surprises pending. I won't be able to fix or even report all the
failures and will need more scalable plan.

Tl;DR
-----

- **__attribute__((format(...)))** with **-Wformat=...** is still
  a good way to catch trivial format string vulnerabilities.
- While disruptive it's still useful to add format attributes to
  **printf()**-like library APIs.
- There were ~60 packages to fix after **ncurses** update.
- It takes about 5 days to build all of **x86_64-linux** **nixpkgs**
  on **hydra**.

Have fun!
