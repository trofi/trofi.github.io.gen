---
title: "A year on gcc from git"
date: March 07, 2021
---

:PostID: 224
:Title: "A year on gcc from git"
:Keywords: gentoo, gcc-10, git
:Categories: notes

Almost `a year </posts/213-gcc-10-in-gentoo.html>`_ passed since
**gcc-10** arrival in gentoo. This means **gcc-11** will release
very soon!

Around that time I decided to switch
my main workstation to **gcc** on git master branch to see how
long it would survive before it destroys something due to a grave
compiler bug. Nothing like that happened yet :)

Having volatile compiler version has all sorts of nasty consequences:
you could have a library or a binary miscompiled due to some transient
compiler issue that was fixed a day after it was introduced.

My solution to stale binaries is to rebuild all packages periodically.
To ease the load on the system I have a cron job that rebuilds all
the packages older than a month on my system. It has an effect of
upgrading compiler itself once a month and rebuilds about 3% of all
packages daily.

In this post I'll share a few issues that came up for such a bleeding
edge system. It's not quite a full list, but it touches majority of
encountered bugs.

SHF_GNU_RETAIN breaks kernel boot
---------------------------------

New **SHF_GNU_RETAIN** section flag managed to break kernel boot:
https://gcc.gnu.org/PR99113 (originally reported as https://sourceware.org/PR27412).

The problem: **gcc-11** changed semantics of existing
**__attribute__((__used__))** code (kernel happens to use it a lot):

.. code-block:: c

  struct s {
    void *print_fmt;
  };
  struct s print_fmt_napi_poll[1];
  __attribute__((__used__)) struct s event_napi_poll = {print_fmt_napi_poll};


- Before **gcc-11**: variable was placed into **.date** section
- In **gcc-11**: variable was placed into **.data.event_napi_poll**

Normally such a section name change is not a problem for userspace
programs as binutils linker script can handle both names.
But linux kernel has custom linker scripts that did not account for new names.

The fix: **gcc-11** reverted semantic change and now enables it only
with new **__attribute__((__retain__))** attribute: https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=6347f4a0904fce17eedf5c071be6f3c118680290

The bug effect: kernel did not boot, kernel build log was full of
unknown section warnings.

-fipa-modref breaks glibc's printf
----------------------------------

New **-fipa-modref** gcc optimisation managed to break **glibc**:
https://sourceware.org/PR26690 (originally reported as https://gcc.gnu.org/PR97264).

The problem: **glibc** had an interesting aliasing violation bug
in **vfprintf** family of functions. In it's simpler form bug looked like that:

.. code-block:: c

    void vfprintf_internal(const char * format_string, ...) {
        const char * p = format_string;
        ...
        // [1]: somewhere in another helper function:
        ++(const unsinged char **)&p;
        ...
        // continue using 'p':
    }

- Before **gcc-11**: variable **p** was advanced with **[1]**
- In **gcc-11**: variable **p** did not change with **[1]** as gcc assumed
  **char** and **unsinged char** (and their pointers) are two different
  types that don't alias and thus operations on them are independent. 

The fix: the aliasing was fixed in **glibc** with https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=c0e9ddf59e73e21afe15fca4e94cf7b4b7359bf2

The bug effect: **uname -p** did hang up and caused
**gcc-11** Gentoo users some inconvenience: https://bugs.gentoo.org/750992.

This bug is very scary: it specifically breaks digit parsing
in format strings like **"%10s"** and has the potential of causing
silent data corruption.

-fipa-modref breaks return value optimization
---------------------------------------------

New **-fipa-modref** gcc optimisation managed to break **libsass**
library tests and exposed a compiler flaw: https://gcc.gnu.org/PR98499

The sample example is:

.. code-block:: c++

    struct string {
      char * _M_buf;
      // local store
      char _M_local_buf[16];
    
      string() : _M_buf(_M_local_buf) {}
    
      ~string() {
        if (_M_buf != _M_local_buf)
          __builtin_trap();
      }
    };

    static string dir_name() { return string(); }

    int main() {
       ...
       string s = dir_name(); 
       ...
    }

Before **gcc-11**:

.. code-block::

    $ g++-10.2.0 -O2 main.cc -o a && ./a
    <ok>


In **gcc-11**:

.. code-block::

    $ g++-11.0.0 -O2 main.cc -o a && ./a
    Illegal instruction     (core dumped) ./a

**gcc-11** managed to optimize **if (_M_buf != _M_local_buf) __builtin_trap();**
into **__builtin_trap();** instead of expected no-op. It happened because **gcc**
**moderf** analysis wrongly assumed that if **s** is a local variable then
it's **s._M_buf** is also a local variable and never escapes.

Unfortunately Return Value Optimization effectively turns local variables
into non-local variables as they escape outside the function they are defined.

The fix: **gcc** was fixed to make less optimistic escaping rules for values
touched by **RVO**: https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=11056ab7687f7156846e93557c9171b77713bd7e.

Longer term **gcc** will improve on **RVO** tracking: https://gcc.gnu.org/PR98925

The bug effect: `libsass <https://github.com/sass/libsass>`_ tests started
failing for `libsass haskell bindings <https://github.com/jakubfijalkowski/hlibsass>`_.

It took me quite a while (perhaps a week of winter evenings) to extract the reasonably
sized test case. It was very elusive because bug only appears in cases when **inliner**
did not do a reasonable job due to various corner cases while **ipa-modref** did
(that's why actual test case is full of **__attribute__((noinline))** annotations.

I also used it as an opportunity to explore and fix my first non-trivial **gcc** bug :)

firefox deadlock in LTO+PGO
---------------------------

New change in **-fprofile-generate** around most frequent value
tracking introduced a deadlock scenario in **firefox**:
https://gcc.gnu.org/PR97461. Firefox's code is similar to:

.. code-block:: c

    // Override default malloc with it's own allocator
    void * malloc(size_t size) {
        // something with TOPN profile, like:
        ...
        return per_bucket_implementation[size % N]();
    }
    
    int main() {
        void * p = malloc(8);
        return p != 0;
    }

Code, generated by **gcc** in **-fprofile-generate** mode converts it
to:

.. code-block:: c

    // inserted by gcc
    static counter __gcov3_malloc;

    // Override default malloc with it's own allocator
    void * malloc(size_t size) {
        // something with TOPN profile, like:
        ...
        // inserted by gcc
        gcov_topn_add_value(__gcov3_malloc, pper_bucket_implementation[size % N]);
        ...
        return per_bucket_implementation[size % N]();
    }
    
    int main() {
        void * p = malloc(8);
        return p != 0;
    }

New implementation of **gcov_topn_add_value** now uses dynamic
memory allocation: https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=871e5ada6d53d5eb
(previously it used static (**GCOV_TOPN_VALUES = 4**).

This started causing infinite recursion:
**malloc() -> gcov_topn_add_value() -> malloc()**.

The fix: **gcc**'s **gcov** library now uses **mmap()** directly to
track **TOPN** counters and avoids **malloc()**:
https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=00d79dc4be0b86ec564cfa2b32c47de6c07449e6

Array attributes typecheck
--------------------------

**gcc** failed to compile **emacs-27.1** that uses unusual **VLA** declaration
in function parameters: https://gcc.gnu.org/PR97413

.. code-block::

    $ cat fns.c
    int a(long b, const int a1[b], long c, const int a2[c], int a3[c]);
    
    $ gcc-11.0.0 -c fns.c.c
    fns.c.c:1:1: error: wrong number of arguments specified for 'access' attribute
        1 | int a(long b, const int a1[b], long c, const int a2[c], int a3[c]);
          | ^~~
    fns.c.c:1:1: note: expected between 1 and 3, found 4

The fix: **gcc** now correctly tracks function attributes:
https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=34efa5c2a84a8c7c7beb855dec24a321eebcbd6b

Webkit miscompilation
---------------------

**gcc** incorrectly compiled **webkit-gtk-2.28.4** around placement new:
https://gcc.gnu.org/PR98130. The following code generates a crashing program:

.. code-block:: c++

    #include <new>
    
    typedef int *T;
    
    static unsigned char storage[sizeof (T)] alignas (T);
    static T *p = (T *) storage;
    
    static inline __attribute__((__always_inline__)) void
    foo (T value)
    {
      new (p) T(value);
    }
    
    int
    main ()
    {
      int a;
      foo (&a);
      if (!*p)
        __builtin_abort ();
    }

Here **gcc-11** assumed that **new (p) T(value);** has no
side-effect and removed '*p = value' part entirely as a
dead store as it did not know if **p** is dereferenced.

The fix: **gcc-11** avoids the optimization for placement operator new:
https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=78c4a9feceaccf487516aa1eff417e0741556e10

That was not the only **webkit** miscompilation related to pointer
handling.

Unhandled template specialization
---------------------------------

A few programs based on **gtkmm** library failed to type check:
https://gcc.gnu.org/PR98115. In my case it was **pavucontrol**.
Small examle:

.. code-block:: c

    template <class> class Stringify;
    template <long N> class Stringify<const char[N]>;

.. code-block::

    $ g++ layout.ii -c
    layout.ii:2:25: error: partial specialization ‘class Stringify<const char [N]>’ is not more specialized than [-fpermissive]
        2 | template <long N> class Stringify<const char[N]>;
          |                         ^~~~~~~~~~~~~~~~~~~~~~~~
    layout.ii:1:24: note: primary template ‘template<class> class Stringify’
        1 | template <class> class Stringify;
          |                        ^~~~~~~~~

**gcc** considered **class** equally unspecialized to **long N**.

The fix: tweak type comparison in **gcc**'s type checker:
https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=9d0eb0ae948f0fbee208cfb9a86133abea650f81

Overriding target attribute behaviour
-------------------------------------

At some point **gcc** failed **grub** linking with very obscure linker
failure. I'll provide only the source example without linker failure:

.. code-block:: c

    // build as:
    //    gcc-11.0.0 -O1 -mcmodel=large -S a.c
    
    extern void grub_memmove(void);
    
    __attribute__((__target__("bmi2"))) void a_bmi(void) {
      // expect:
      //   movabsq $grub_memmove, %rbx
      //   call    *%rbx
      // actual (bug):
      //   call    grub_memmove
      for (;;)
        grub_memmove();
    }

Here **gcc** effectively removed **-mcmodel=large** due to presence
of **__attribute__((__target__("bmi2")))** and generated invalid
assembly (**-mcmodel=medium**, assumed **.text** is within 4GB reach).

The fix: **gcc** was fixed not to ignore **-mcmodel=** and friends
when overriding target attributes are used: https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=8f1cb70d7ca6a8da7f6bc7f43fb5e758c0ce88b5

This was a nasty bug. We were lucky that **gcc** managed to generate
a relocation that linker does not understand instead of silently
generating invalid **grub**.

Other minor gcc bugs
--------------------

A few more smaller bugs if you like that kind of stuff:

- https://gcc.gnu.org/PR98753: **-Wfree-nonheap-object** false positive
- https://gcc.gnu.org/PR98286: **g++** accepts **void d(void) { typename foo; }** as valid code
- https://gcc.gnu.org/PR98306: **g++** accepts invalid template code as valid
- https://gcc.gnu.org/PR97615: dolphin ICE in SLP (vectorization patterns need a tweak)
- https://gcc.gnu.org/PR96863: dolphin varasm ICE
- https://gcc.gnu.org/PR97206: array typecheck bug
- https://sourceware.org/PR27004: glibc tls reference bug
- https://gcc.gnu.org/PR98403: grub build hangup
- https://gcc.gnu.org/PR98597: ICE in print_mem_ref
- https://gcc.gnu.org/PR97830: iCE in sccvn

Parting words
-------------

Running a compiler from a development branch is a lot of fun. Every
compiler upgrade fixed some existing bugs and introduced new ones.
It is important to make sure fixes are intentional and not just
masking bugs by other changes. And always keep a backup ready.
You will need it.

Many of the above regressions were not caught with regression tests
as they were frequently not trivial feature interactions. Real
world applications keep surprising **gcc**.

`Gentoo <https://www.gentoo.org/>`_ makes it very easy to break
(and fix!) your system if you really want it. It's perhaps the
primary reason I keep using it for about 15 years. You might want
to give it a try as well :)

Have fun!
