---
title: "stack growth direction: how hard can it be?"
date: June 26, 2017
---

:PostID: 202
:Title: "stack growth direction: how hard can it be?"
:Keywords: gentoo, ia64, glibc, kernel, ruby, ski, elf
:Categories: notes

This is another horror story with lots of technical details
on how **glibc** was unhappy (again!) on **ia64**.

A week ago I've decided to give **ia64** more love in gentoo
by cleaning up backlog of bugs related to ia64 team: `hotlist <https://bugs.gentoo.org/buglist.cgi?email2=ia64%40gentoo.org&emailassigned_to2=1&emailcc2=1&emailtype2=substring&list_id=3571042&order=opendate%2Cbug_id&query_format=advanced&resolution=--->`_
The backlog was not too large: about 150 bugs.

ruby garbage collection bug
===========================

Many bugs were blocked on `SIGSEGVing ruby <https://bugs.gentoo.org/show_bug.cgi?id=561780>`_.

The curious fact is that **ruby-2.1.9** used to work on **ia64**
and broke at some point. These kinds of bugs (as opposed to bring
software support for the first time) are usually easy to fix.
Something got slightly off and exposed a SIGSEGV. We need to find
that something and tweak it a tiny bit.

Let's look at the **SIGSEGV** (it happens as you compile **ruby** on **ia64**):

.. code-block::

      Program received signal SIGSEGV, Segmentation fault.
      mark_locations_array (objspace=0x6000000000045db0, x=0x0, n=864692227966763116) at gc.c:3297
      3297            v = *x;
      (gdb) bt
      #0  mark_locations_array (objspace=0x6000000000045db0, x=0x0, n=864692227966763116) at gc.c:3297
      #1  0x400000000014a040 in gc_mark_locations (objspace=0x6000000000045db0, start=0x0, end=0x6000080000000368) at gc.c:3310
      #2  0x400000000014b3a0 in mark_current_machine_context (objspace=0x6000000000045db0, th=0x60000000000455b0) at gc.c:3500
      #3  0x400000000014dfe0 in gc_mark_roots (objspace=0x6000000000045db0, full_mark=0, categoryp=0x0) at gc.c:4105
      #4  0x400000000014e6b0 in gc_marks_body (objspace=0x6000000000045db0, full_mark=0) at gc.c:4164
      #5  0x400000000014f260 in gc_marks (objspace=0x6000000000045db0, full_mark=0) at gc.c:4526
      #6  0x40000000001525c0 in garbage_collect_body (objspace=0x6000000000045db0, full_mark=0, immediate_sweep=0, reason=256) at gc.c:5024
      #7  0x400000000013c010 in heap_prepare_freepage (objspace=0x6000000000045db0, heap=0x6000000000045dc0) at gc.c:1219
      #8  0x400000000013c140 in heap_get_freeobj_from_next_freepage (objspace=0x6000000000045db0, heap=0x6000000000045dc0) at gc.c:1237
      #9  0x400000000013c360 in heap_get_freeobj (objspace=0x6000000000045db0, heap=0x6000000000045dc0) at gc.c:1259
      #10 0x400000000013c950 in newobj_of (klass=0, flags=40, v1=0, v2=0, v3=0) at gc.c:1303
      #11 0x400000000013ccc0 in rb_newobj_of (klass=0, flags=40) at gc.c:1356
      #12 0x4000000000163740 in hash_alloc (klass=0) at hash.c:289
      #13 0x4000000000163860 in rb_hash_new () at hash.c:309
      #14 0x400000000050e420 in Init_BareVM () at vm.c:2822
      #15 0x40000000000f6b60 in ruby_setup () at eval.c:54
      #16 0x40000000000f6f50 in ruby_init () at eval.c:75
      #17 0x400000000001b010 in main (argc=9, argv=0x60000fffffffb1d8) at main.c:35

I've added a bunch of **printf** calls into **ruby** runtime to figure out
where NULL pointer dereference comes from. It became immediately obvious:
one of **ia64**-specific pointers was not initialized and kept default **NULL**.

While I was adding **printf** calls I've noticed a lot of **#ifdef __ia64**
in every place that deals with threads: thread creation, thread switch,
garbage colection. I had almost no idea what code is supposed to do but
very basic understanding of how garbage collection works suggested
variable **native_main_thread.register_stack_start** should never
be **NULL** on **ia64**.

Cooking up the fix was trivial (proposed `pull request upstream <https://github.com/ruby/ruby/pull/1625>`_):

.. code-block:: diff

    diff --git a/thread_pthread.c b/thread_pthread.c
    --- a/thread_pthread.c
    +++ b/thread_pthread.c
    @@ -740,100 +740,100 @@ ruby_init_stack(volatile VALUE *addr
     void
     ruby_init_stack(volatile VALUE *addr
     #ifdef __ia64
         , void *bsp
     #endif
         )
     {
         native_main_thread.id = pthread_self();
    +#ifdef __ia64
    +    if (!native_main_thread.register_stack_start ||
    +        (VALUE*)bsp < native_main_thread.register_stack_start) {
    +        native_main_thread.register_stack_start = (VALUE*)bsp;
    +    }
    +#endif
     #if MAINSTACKADDR_AVAILABLE
         if (native_main_thread.stack_maxsize) return;
         {
            void* stackaddr;
            size_t size;
            if (get_main_stack(&stackaddr, &size) == 0) {
                native_main_thread.stack_maxsize = size;
                native_main_thread.stack_start = stackaddr;
                reserve_stack(stackaddr, size);
                goto bound_check;
            }
         }
     #endif
     #ifdef STACK_END_ADDRESS
         native_main_thread.stack_start = STACK_END_ADDRESS;
     #else
         if (!native_main_thread.stack_start ||
             STACK_UPPER((VALUE *)(void *)&addr,
                         native_main_thread.stack_start > addr,
                         native_main_thread.stack_start < addr)) {
             native_main_thread.stack_start = (VALUE *)addr;
         }
     #endif
    -#ifdef __ia64
    -    if (!native_main_thread.register_stack_start ||
    -        (VALUE*)bsp < native_main_thread.register_stack_start) {
    -        native_main_thread.register_stack_start = (VALUE*)bsp;
    -    }
    -#endif
         {
     #if defined(HAVE_GETRLIMIT)
     #if defined(PTHREAD_STACK_DEFAULT)
     # if PTHREAD_STACK_DEFAULT < RUBY_STACK_SPACE*5
     #  error "PTHREAD_STACK_DEFAULT is too small"
     # endif
            size_t size = PTHREAD_STACK_DEFAULT;
     #else
            size_t size = RUBY_VM_THREAD_VM_STACK_SIZE;
     #endif
            size_t space;
            int pagesize = getpagesize();
            struct rlimit rlim;
             STACK_GROW_DIR_DETECTION;
            if (getrlimit(RLIMIT_STACK, &rlim) == 0) {
                size = (size_t)rlim.rlim_cur;
            }
            addr = native_main_thread.stack_start;
            if (IS_STACK_DIR_UPPER()) {
                space = ((size_t)((char *)addr + size) / pagesize) * pagesize - (size_t)addr;
            }
            else {
                space = (size_t)addr - ((size_t)((char *)addr - size) / pagesize + 1) * pagesize;
            }
            native_main_thread.stack_maxsize = space;
     #endif
         }
    
     #if MAINSTACKADDR_AVAILABLE
       bound_check:
     #endif
         /* If addr is out of range of main-thread stack range estimation,  */
         /* it should be on co-routine (alternative stack). [Feature #2294] */
         {
            void *start, *end;
            STACK_GROW_DIR_DETECTION;
    
            if (IS_STACK_DIR_UPPER()) {
                start = native_main_thread.stack_start;
                end = (char *)native_main_thread.stack_start + native_main_thread.stack_maxsize;
            }
            else {
                start = (char *)native_main_thread.stack_start - native_main_thread.stack_maxsize;
                end = native_main_thread.stack_start;
            }
    
            if ((void *)addr < start || (void *)addr > end) {
                /* out of range */
                native_main_thread.stack_start = (VALUE *)addr;
                native_main_thread.stack_maxsize = 0; /* unknown */
            }
         }
     }

The fix is to move initializaton code before exit from function happens.

I think **ruby_init_stack** used to work because **MAINSTACKADDR_AVAILABLE**
was not defined in older **glibc**. Perhaps due to missing **HAVE_PTHREAD_GETATTR_NP**
support or something similar. See how compilate detection of **STACKADDR_AVAILABLE**
is:

.. code-block:: c

    #if defined HAVE_PTHREAD_GETATTR_NP || defined HAVE_PTHREAD_ATTR_GET_NP
    #    define STACKADDR_AVAILABLE 1
    #elif defined HAVE_PTHREAD_GET_STACKADDR_NP && defined HAVE_PTHREAD_GET_STACKSIZE_NP
    #    define STACKADDR_AVAILABLE 1
    #    undef MAINSTACKADDR_AVAILABLE
    #    define MAINSTACKADDR_AVAILABLE 1
    void *pthread_get_stackaddr_np(pthread_t);
    size_t pthread_get_stacksize_np(pthread_t);
    #elif defined HAVE_THR_STKSEGMENT || defined HAVE_PTHREAD_STACKSEG_NP
    #    define STACKADDR_AVAILABLE 1
    #elif defined HAVE_PTHREAD_GETTHRDS_NP
    #    define STACKADDR_AVAILABLE 1
    #elif defined __HAIKU__
    #    define STACKADDR_AVAILABLE 1
    #elif defined __ia64 && defined _HPUX_SOURCE
    #    include <sys/dyntune.h>
    ...

But now **STACKADDR_AVAILABLE** is defined and **goto bound_check** skips
**native_main_thread.register_stack_start** initialization completely.

My patch worked and I was happy. But still it was slightly confusing to see
all that **ia64**-specific code for stack handling. What is so special about
it's stack?

Let's look at a code example in **cont.c** file that scans stack for heap pointers:

.. code-block:: c

    static void
    cont_mark(void *ptr)
    {
        rb_context_t *cont = ptr;
    
        RUBY_MARK_ENTER("cont");
        rb_gc_mark(cont->value);
    
        rb_thread_mark(&cont->saved_thread);
        rb_gc_mark(cont->saved_thread.self);
    
        if (cont->vm_stack) {
    #ifdef CAPTURE_JUST_VALID_VM_STACK
            rb_gc_mark_locations(cont->vm_stack,
                                 cont->vm_stack + cont->vm_stack_slen + cont->vm_stack_clen);
    #else
            rb_gc_mark_locations(cont->vm_stack,
                                 cont->vm_stack, cont->saved_thread.stack_size);
    #endif
        }
    
        if (cont->machine.stack) {
            if (cont->type == CONTINUATION_CONTEXT) {
                /* cont */
                rb_gc_mark_locations(cont->machine.stack,
                                     cont->machine.stack + cont->machine.stack_size);
            } else {
                /* fiber */
                rb_thread_t *th;
                rb_fiber_t *fib = (rb_fiber_t*)cont;
                GetThreadPtr(cont->saved_thread.self, th);
                if ((th->fiber != fib) && fib->status == RUNNING) {
                    rb_gc_mark_locations(cont->machine.stack,
                                         cont->machine.stack + cont->machine.stack_size);
                }
            }
        }
    #ifdef __ia64
        if (cont->machine.register_stack) {
            rb_gc_mark_locations(cont->machine.register_stack,
                                 cont->machine.register_stack + cont->machine.register_stack_size);
        }
    #endif
    
        RUBY_MARK_LEAVE("cont");
    }

Additional code under **#ifdef __ia64** looked unusual but it didn't seem to
harm any **ruby** tests and I moved on.

binutils out-of-bounds bug
==========================

Next bug was lurking in **binutils-2.28** package which occasionally `crashed strip program <https://sourceware.org/bugzilla/show_bug.cgi?id=21669>`_.
In my case crash was happening only when I was building **gcc**. **gcc**'s build system
happens to call **strip** binary when compares **stage2** and **stage3** as one of stages
was built with debugging sections (using **-gtoggle** switch).

The **strip** **SIGSEGV** fix was also surprisingly trivial and not directly related to stack (or even **ia64**) specifics
(`upstream commit <https://sourceware.org/git/gitweb.cgi?p=binutils-gdb.git;a=commitdiff;h=5cc4ca837deac7dc962d8a3741aa120c50ab41da>`_):

.. code-block:: diff


    diff --git a/bfd/elf.c b/bfd/elf.c
    index 5f37e7f79c..76c6a5c6a7 100644
    --- a/bfd/elf.c
    +++ b/bfd/elf.c
    @@ -1283,7 +1283,8 @@ section_match (const Elf_Internal_Shdr * a,
     static unsigned int
     find_link (const bfd * obfd, const Elf_Internal_Shdr * iheader, const unsigned int hint)
     {
       Elf_Internal_Shdr ** oheaders = elf_elfsections (obfd);
       unsigned int i;
    
       BFD_ASSERT (iheader != NULL);
    
       /* See PR 20922 for a reproducer of the NULL test.  */
    -  if (oheaders[hint] != NULL
    +  if (hint < elf_numsections (obfd)
    +      && oheaders[hint] != NULL
           && section_match (oheaders[hint], iheader))
         return hint;

Here a mysterious **hint** was used to refer to out-of-bounds area and that caused **SIGSEGV**s.
My guess why it triggered mostly on **ia64** is because **ia64** has many more **ELF**-sections
than other architectures (**160** versus **32**).

glibc pthread_create bug
========================

The next failure happened for **glibc-2.24** package (**glibc-2.23** worked fine).
It looked like every threaded program crashed around
program shutdown (`upstream bug <https://sourceware.org/PR21672>`_).

That small reproducer was enough to make program crash:

.. code-block:: c

    // how to crash: gcc -O0 -ggdb3 -o r bug.c -pthread && ./r

    #include <pthread.h>

    static void * f (void * p)
    {
        return NULL;
    }

    int main (int argc, const char ** argv)
    {
        pthread_t t;
        pthread_create (&t, NULL, &f, NULL);

        pthread_join (t, NULL);
        return 0;
    }

Here we create a no-op thread and wait for it's shutdown.
The **SIGSEGV** happened at address **0x8** (another **NULL**-pointer dereference).

Backtrace was not very informative:

.. code-block::

    $ gcc -O0 -ggdb3 -o r bug.c -pthread && ./r
    Segmentation fault (core dumped)
    
    $  gdb r core
    ...
    Program terminated with signal SIGSEGV, Segmentation fault.
    #0  0x2000000000077da0 in start_thread (arg=0x0) at pthread_create.c:432
    432         __madvise (pd->stackblock, freesize - PTHREAD_STACK_MIN, MADV_DONTNEED);
    [Current thread is 1 (Thread 0x2000000000b6b1f0 (LWP 20912))]
    
    (gdb) list
    427     #ifdef _STACK_GROWS_DOWN
    428       char *sp = CURRENT_STACK_FRAME;
    429       size_t freesize = (sp - (char *) pd->stackblock) & ~pagesize_m1;
    430       assert (freesize < pd->stackblock_size);
    431       if (freesize > PTHREAD_STACK_MIN)
    432         __madvise (pd->stackblock, freesize - PTHREAD_STACK_MIN, MADV_DONTNEED);
    433     #else
    434       /* Page aligned start of memory to free (higher than or equal
    435          to current sp plus the minimum stack size).  */
    436       void *freeblock = (void*)((size_t)(CURRENT_STACK_FRAME
    
    #0  0x2000000000077da0 in start_thread (arg=0x0) at pthread_create.c:432
            pd = 0x0
            now = <optimized out>
            unwind_buf = <error reading variable unwind_buf (Cannot access memory at address 0xfffffffffffffd90)>
            not_first_call = <optimized out>
            pagesize_m1 = <optimized out>
            sp = 0x2000000000b6a870 ""
            freesize = <optimized out>
            __PRETTY_FUNCTION__ = "start_thread"
    #1  0x0000000000000000 in ?? ()

This crash did not make much sense. At first I thought it was caused by **pd->stackblock**
code where **pd** was somehow turned into **NULL**. But if we look a few lines above (`source link <https://sourceware.org/git/?p=glibc.git;a=blob;f=nptl/pthread_create.c;h=7a970ffc5bc6123bbad7d2e38e974ffd2bf859f9;hb=3f823e87ccbf3723eb4eeb63b0619f1a0ceb174e#l561>`_)
**pd** is used in that function all over the places. **strace** run also suggested that crash
happened after **madvise** syscall returned successfully (**pd->stackblock** has a sane value).

The whole `start_thread function <https://sourceware.org/git/?p=glibc.git;a=blob;f=nptl/pthread_create.c;h=7a970ffc5bc6123bbad7d2e38e974ffd2bf859f9;hb=3f823e87ccbf3723eb4eeb63b0619f1a0ceb174e#l378>`_
is quite large but very straightforward. It does tree main things:

1. setup environment for current thread: locale data, futex robust_lists (efficient mutex runtime support), signal masks
2. run user's code with this one line: **THREAD_SETMEM (pd, result, pd->start_routine (pd->arg));**
3. teardown environment: call thread-local destructors, call futex robust_lists and free thread's stack

Our crash happens in **3. teardown environment** phase right at the place of thread's stack teardown.

glibc's stack teardown is interesting: it does not free all the stack because code responsible
for stack cleanup uses that very same stack. Let's look at the code in detail:

.. code-block:: c

    /*   Mark the memory of the stack as usable to the kernel.  We free
         everything except for the space used for the TCB itself.  */
      size_t pagesize_m1 = __getpagesize () - 1;
    #ifdef _STACK_GROWS_DOWN
      char *sp = CURRENT_STACK_FRAME;
      size_t freesize = (sp - (char *) pd->stackblock) & ~pagesize_m1;
      assert (freesize < pd->stackblock_size);
      if (freesize > PTHREAD_STACK_MIN)
        __madvise (pd->stackblock, freesize - PTHREAD_STACK_MIN, MADV_DONTNEED);
    #else
      /* Page aligned start of memory to free (higher than or equal
         to current sp plus the minimum stack size).  */
      void *freeblock = (void*)((size_t)(CURRENT_STACK_FRAME
                                         + PTHREAD_STACK_MIN
                                         + pagesize_m1)
                                        & ~pagesize_m1);
      char *free_end = (char *) (((uintptr_t) pd - pd->guardsize) & ~pagesize_m1);
      /* Is there any space to free?  */
      if (free_end > (char *)freeblock)
        {
          size_t freesize = (size_t)(free_end - (char *)freeblock);
          assert (freesize < pd->stackblock_size);
          __madvise (freeblock, freesize, MADV_DONTNEED);
        }
    #endif

Here we see two major branches: **_STACK_GROWS_DOWN** and the **#else** one (not used on **ia64**).
**pd** knows precisely where thread's stack resides: it's in **[pd->stackblock, pd->stackblock + pd->stackblock_size)** range.
**_STACK_GROWS_DOWN** means that stack starts at address around **pd->stackblock + pd->stackblock_size**
and grows in backward direction (to clarify: stack pointer decreases when value is pushed to stack).

So far so good, no magic here. **x86_64** does the same.

First clue
==========

But why **madvise()** affects anyting? Isn't it just a hint to kernel's memory that can't go wrong
even if you messed up the arguments?

Does page deallocation happen at all? It's just a **madvise** after all. From **man 2 madvise**:

.. code-block::

   Conventional advice values

       The  advice  values  listed below allow an application to tell the kernel how it expects to use
       some mapped or shared memory areas, so that the kernel can choose  appropriate  read-ahead  and
       caching  techniques.   These  advice  values  do not influence the semantics of the application
       (except in the case of MADV_DONTNEED), but may influence its performance.  All  of  the  advice
       values  listed here have analogs in the POSIX-specified posix_madvise(3) function, and the val‐
       ues have the same meanings, with the exception of MADV_DONTNEED.

       MADV_DONTNEED
              Do  not  expect access in the near future.  (For the time being, the application is fin‐
              ished with the given range, so the kernel can free resources associated with it.)

              After a successful MADV_DONTNEED operation, the semantics of memory access in the speci‐
              fied  region  are  changed:  subsequent accesses of pages in the range will succeed, but
              will result in either repopulating the memory contents from the up-to-date  contents  of
              the  underlying  mapped  file  (for shared file mappings, shared anonymous mappings, and
              shmem-based techniques such as System V shared memory segments)  or  zero-fill-on-demand
              pages for anonymous private mappings.

              Note  that,  when  applied to shared mappings, MADV_DONTNEED might not lead to immediate
              freeing of the pages in the range.  The kernel is free to delay freeing the pages  until
              an appropriate moment.  The resident set size (RSS) of the calling process will be imme‐
              diately reduced however.

              MADV_DONTNEED cannot be applied to locked pages, Huge TLB  pages,  or  VM_PFNMAP  pages.
              (Pages  marked with the kernel-internal VM_PFNMAP flag are special memory areas that are
              not managed by the virtual memory subsystem.  Such pages are typically created by device
              drivers that map the pages into user space.)

Tl;DR variant: **madvise(p, size, MADV_DONTNEED)** works as **memset(p, 0, size)**. And it's the only **advice** value that changes program semantics.

That was the first clue: perhaps we are zeroing out some crucial data structure?
I've commented out **__madvise** call and **SIGSEGV** disappeared. Gah!

I've decided to check if **ia64** is an indeed a **_STACK_GROWS_DOWN** platform:

.. code-block::

    #include <pthread.h>
    #include <stdio.h>
    #include <unistd.h>
    
    static void g(int a, int b, int c, int d, int e, int f)
    {
        int v;
        printf ("sp   = %p\n", &v);
    }
    
    static void * f (void * p)
    {
        int v;
        printf ("sp   = %p\n", &v);
        g(1,2,3,4,5,6);
    
        return NULL;
    }
    
    int main (int argc, const char ** argv)
    {
        printf ("page = %u\n", getpagesize());
    
        pthread_t t;
        pthread_create (&t, NULL, &f, NULL);
    
        pthread_join (t, NULL);
        return 0;
    }

.. code-block::

    $ ia64-unknown-linux-gnu-gcc -O0 -ggdb3 -o stack stack.c -pthread && ./stack
    page = 65536
    sp   = 0x2000000000b7e860
    sp   = 0x2000000000b7e830
    ...
    madvise(start=0x20000000003b0000, len=0x790000, flags=0x4)

We see a few facts here:

- stack indeed grows by decreasing **sp** (aka **_STACK_GROWS_DOWN**)
- **ia64** pages are 64K
- madvise does not touch **sp** itself and stops at 3 pages away (**PTHREAD_STACK_MIN** value): **0x20000000003b0000 + 0x790000 = 0x2000000000B40000**

So why do things fail? I've tried to add more debug statements into kernel's **sys_madvise** implementation
and ran it under `ski emulator <http://trofi.github.io/posts/199-ia64-machine-emulation.html>`_.
**SIGSEGV** was still reproducible.

Then I've recalled strange **bsp** business and additional stack area
tracked by ruby's garbage collector. I wondered wher that additional
memory region resides:

.. code-block:: c

    #include <pthread.h>
    #include <stdio.h>
    #include <unistd.h>
    
    static void g(int a, int b, int c, int d, int e, int f)
    {
        int v;
        printf ("sp   = %p\n", &v);
        printf ("bsp  = %p\n", __builtin_ia64_bsp());
    }
    
    static void * f (void * p)
    {
        int v;
        printf ("sp   = %p\n", &v);
        printf ("bsp  = %p\n", __builtin_ia64_bsp());
        g(1,2,3,4,5,6);
    
        return NULL;
    }
    
    int main (int argc, const char ** argv)
    {
        pthread_t t;
        pthread_create (&t, NULL, &f, NULL);
    
        pthread_join (t, NULL);
        return 0;
    }

.. code-block::

    $ ia64-unknown-linux-gnu-gcc -O0 -ggdb3 -o stack2 stack2.c -pthread && ./stack2
    $ ./stack2
    sp   = 0x2000000000b7e860
    bsp  = 0x2000000000380090
    sp   = 0x2000000000b7e830
    bsp  = 0x20000000003800b8
    madvise(start=0x20000000003b0000, len=0x790000, flags=0x4)

See what happens here? **sp** and **bsp** grow from opposite directions of stack block
towards one another both staring the same stack area:

.. code-block::

    +--------------------------+
    | bsp_start:  0x2...3b0000 |
    | ...                      |
    | bsp:        0x2...3800b8 |
    +--------------------------+
    | ....                     |
    | guard page: 0x2...770000 |
    | ....                     |
    +--------------------------+
    | sp:         0x2...b7e830 |
    | ...                      |
    | sp_start:   0x2...b80000 |
    +--------------------------+

**BSP** means Backing Store Pointer. That memory area is used by CPU to backup
and restore CPU register values (but not other local variables) for each procedure
call/return for caller-save registers. Usually C programs dont need to care
about **bsp** value or area contents.

It means that we should try hard not to lose **bsp** area when we are tearing down
the stack because register spilling/loading hapens at unusual times: CPU can
defer or avoid spilling/reloading registers to speed up performance.

Thus the fix could look like that (`proposed upstream <https://sourceware.org/ml/libc-alpha/2017-06/msg01265.html>`_):

.. code-block:: diff

    diff --git a/nptl/pthread_create.c b/nptl/pthread_create.c
    index 7a970ffc5b..6e3f6db5b1 100644
    --- a/nptl/pthread_create.c
    +++ b/nptl/pthread_create.c
    @@ -555,10 +555,24 @@ START_THREAD_DEFN
       size_t pagesize_m1 = __getpagesize () - 1;
     #ifdef _STACK_GROWS_DOWN
       char *sp = CURRENT_STACK_FRAME;
    -  size_t freesize = (sp - (char *) pd->stackblock) & ~pagesize_m1;
    +  char *freeblock = (char *) pd->stackblock;
    +  size_t freesize = (sp - freeblock) & ~pagesize_m1;
       assert (freesize < pd->stackblock_size);
    +# ifdef __ia64__
       if (freesize > PTHREAD_STACK_MIN)
    -    __madvise (pd->stackblock, freesize - PTHREAD_STACK_MIN, MADV_DONTNEED);
    +    {
    +      /* On ia64 stack grows both ways!
    +         - normal "sp" stack (stack for local variables) grows down
    +         - register stack "bsp" grows up from the opposite end of stack block
    +
    +         Thus we leave PTHREAD_STACK_MIN bytes from stack block top
    +         and leave same PTHREAD_STACK_MIN at stack block bottom.  */
    +      freeblock += PTHREAD_STACK_MIN;
    +      freesize -= PTHREAD_STACK_MIN;
    +    }
    +# endif
    +  if (freesize > PTHREAD_STACK_MIN)
    +    __madvise (freeblock, freesize - PTHREAD_STACK_MIN, MADV_DONTNEED);
     #else
       /* Page aligned start of memory to free (higher than or equal
          to current sp plus the minimum stack size).  */

Here we skip **PTHREAD_STACK_MIN** bytes from both beginning and end of **pd->stackblock**.
This fixed pthread_create **SIGSEGV**s. Why it did not exhibit before? I have no idea!
My guess would be that older glibc used less stack space and didn't bother to reload
from **bsp** after **madvise** call.

Random facts about ia64
=======================

- default page size on linux is **64K**
- rare (unique?) setup of two stacks growing in opposite directions
- many garbage collector implementations have to special-case for **bsp** area as additional pointers can lurk there
- thread shutdown is delicate in **glibc**

Have fun!
