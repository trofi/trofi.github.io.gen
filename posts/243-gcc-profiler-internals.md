---
title: "gcc profiler internals"
date: May 01, 2022
---

I occasionally encounter profiler bugs in gcc. Two related examples I
remember are [tauthon (python-2 fork)](https://gcc.gnu.org/PR96394) and
[python](https://gcc.gnu.org/PR105282) ICEs in **PGO**  build mode. **PGO**
profile-guided optimisation.

I keep forgetting **gcc**'s internals aroung profiling handling and keep
re-tracing the same steps every time it comes up. I decided to write a
few words on it. I'll cover a few generic details first and then we'll
explore today's [python](https://gcc.gnu.org/PR105282) bug.

# using gcc profiler: -fprofile-generate / -fprofile-use

Let's start from a trivial example. To build our program we run **gcc**
on it:

```
$ gcc $CFLAGS prog.c -o prog
```

To build an optimised program using profile feedback we need to perform
3 steps:

```
$ gcc $CFLAGS -fprofile-generate prog.c -o prog
$ ./prog some-training-input-data
$ gcc $CFLAGS -fprofile-use      prog.c -o prog
```

These are:

1. build instrumented program
2. run it to get a profile
3. build optimized program:

**PGO** build mode is not the only use of profile instrumentation.
**GCC** allows gathering runtime execution stats using different types
and formats ([instrumentation options](https://gcc.gnu.org/onlinedocs/gcc/Instrumentation-Options.html)).

A few frequent uses are:

- call graph style profiling (**-pg** option): used to explore call
  graph in **gprof** format.
- execution statictics coverage (**-ftest-coverage** option): used to
  extract close to line-by-line execution coverage in **gcno** format.
- execution statistics profiles (**-fprofile-generate**): used to guide
  optimization in future compilation (**PGO**) in **gcda** format.

All these modes are not fundamentally different. They share
implementation and in-memory/on-disk format. I'll look at a **gcda**
form. We'll use the following sample code as our running example:

```c
#include <stddef.h>

/* Keep the functions around to have real indirect calls. */
#define DECL_F(fn) static void fn(void) __attribute__((noipa))
#define  DEF_F(fn) static void fn(void) {}

DECL_F(f0); DEF_F(f0);
DECL_F(f1); DEF_F(f1);
DECL_F(f2); DEF_F(f2);

int main (int argc, char *argv[]) {
    if (argc < 2) return 0;

    static const void(*ft[])(void) = { &f0, &f1, &f2, };
    size_t sz = sizeof (ft) / sizeof (ft[0]);

    for (const char * p = argv[1]; *p; p++)
        ft[*p % sz]();

    return 0;
}
```

This program accepts one string argument (**argv[1]**) and calls
function **f0()**, **f1()**, or **f2()** somewhat randomly (based on
char modulo) against each byte of input string. The idea is that
it's not immediately obvious which of the functions is called most
frequently.

This program has a few conditional branches and indirect calls. Let's
profile it and see what stats we can collect. Building:

```
$ gcc a.c -o a -fprofile-generate -fprofile-arcs -ftest-coverage -O2
```

Running our program 5 times:

```
$ ./a
$ ./a 123456789
$ ./a aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
$ ./a aa
$ ./a aaaa
$ ls
a  a.c  a.gcda  a.gcno
```

Note that each **./a** run creates or updates **a.gcda** and **a.gcno**.
We can dump profiling statistics with **gcov**:

```
$ gcov -a -b -c -f a
$ cat a.c.gcov
        -:    0:Source:a.c
        -:    0:Graph:a.gcno
        -:    0:Data:a.gcda
        -:    0:Runs:5
        -:    1:#include <stddef.h>
        -:    2:
        -:    3:/* Keep the functions around to have real indirect calls. */
        -:    4:#define DECL_F(fn) static void fn(void) __attribute__((noipa))
        -:    5:#define  DEF_F(fn) static void fn(void) {}
        -:    6:
function f0 called 3 returned 100% blocks executed 100%
        3:    7:DECL_F(f0); DEF_F(f0);
function f1 called 53 returned 100% blocks executed 100%
       53:    8:DECL_F(f1); DEF_F(f1);
function f2 called 3 returned 100% blocks executed 100%
        3:    9:DECL_F(f2); DEF_F(f2);
        -:   10:
function main called 5 returned 100% blocks executed 100%
        5:   11:int main (int argc, char *argv[]) {
        5:   12:    if (argc < 2) return 0;
        5:   12-block  0
branch  0 taken 4 (fallthrough)
branch  1 taken 1
        -:   13:
        -:   14:    static const void(*ft[])(void) = { &f0, &f1, &f2, };
        -:   15:    size_t sz = sizeof (ft) / sizeof (ft[0]);
        -:   16:
       63:   17:    for (const char * p = argv[1]; *p; p++)
        4:   17-block  0
       63:   17-block  1
branch  0 taken 59
branch  1 taken 4 (fallthrough)
       59:   18:        ft[*p % sz]();
       59:   18-block  0
call    0 returned 59
        -:   19:
        -:   20:    return 0;
        -:   21:}
```

Here we see that:

-  **ft\[\*p % sz\]()** gets called 59 times: 3 (**f0**) + 53 (**f1**) + 3 (**f2**)
- function **main()** gets called 5 times; a full length of all the inputs we passed to **./a** program

We also see frequency of **if (argc < 2) return 0;** branches:

- was taken once (**./a** case, no arguments)
- was not taken 4 times (all the other **./a ...** calls)

After profile collection we can build new binary using **.gcda** files
by changing **-fprofile-generate** to **-fprofile-use** in our **gcc**
calls:

```
$ gcc a.c -o a -fprofile-use -O2
```

Let's compare the result against unprofiled build:

```
$ gcc a.c -o a-unprof -O2
```

I'll skip disassembly as instructions do not change materially (the
program is too simple). The code order is slightly different in the two
binaries. Let's have a look:

```
$ nm -n a-unprof
...
0000000000400390 r __abi_tag
0000000000401000 T _init
0000000000401020 T main
0000000000401090 T _start
00000000004010c0 T _dl_relocate_static_pie
00000000004010d0 t deregister_tm_clones
0000000000401100 t register_tm_clones
0000000000401140 t __do_global_dtors_aux
0000000000401170 t frame_dummy
0000000000401180 t f0
0000000000401190 t f1
00000000004011a0 t f2
...
```

```
$ nm -n a
...
0000000000400390 r __abi_tag
0000000000401000 T _init
0000000000401020 T main
0000000000401080 t f1
0000000000401090 T _start
...
```

**nm -n** orders symbols in their in-memory order so we could
get the idea what goes where.

One can spot that in unprofiled build **main()** is quite far away from most
frequently called **f1()** function. While in profiled build
**f1()** immediately follows **main()**.

In more practical scenarios the effects are more pronounced:

- likely to execute code is laid out in a way that does not require branching
- inlining can rely on execution statistics instead of function size heuristics
- **-flto** could perform global program code reordering to speedup binary loading from disk
- and many other effects

The main takeaway here is that **PGO** requires two full builds
(**-fprofile-generate** and **-fprofile-use**) and a program training
run.

In our example training data is biased towards inputs with **'a'**
symbols and thus our program is probably slightly more efficient at
handling those.

In **PGO** builds the trick is to find suitable input training data.
Typical rookie mistake is to use **./prog --help** as a training run.
Do not do that: you will get program optimised for printing help text.
The rest will perform worse than typical unprofiled build.

# internals: gcov-dump tool

What exactly do these **.gcda** files contain? Let's have a peek with
**gcov-dump** tool shipped with **gcc**:

```
$ gcov-dump a.gcda

a.gcda:data:magic `gcda':version `B12*'
a.gcda:stamp 1137774494
a.gcda:  a1000000:   2:OBJECT_SUMMARY runs=5, sum_max=60
a.gcda:  01000000:   3:FUNCTION ident=108032747, lineno_checksum=0x0a7a17ea, cfg_checksum=0xc835c602
a.gcda:    01a10000:   8:COUNTERS arcs 4 counts
a.gcda:    01a90000:  16:COUNTERS indirect_call 8 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:  01000000:   3:FUNCTION ident=1567133468, lineno_checksum=0xdadb6f0d, cfg_checksum=0xa43083b8
a.gcda:    01a10000:   2:COUNTERS arcs 1 counts
a.gcda:    01a90000:   0:COUNTERS indirect_call 0 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:  01000000:   3:FUNCTION ident=1025457522, lineno_checksum=0x745daa69, cfg_checksum=0xa43083b8
a.gcda:    01a10000:   2:COUNTERS arcs 1 counts
a.gcda:    01a90000:   0:COUNTERS indirect_call 0 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:  01000000:   3:FUNCTION ident=1634904005, lineno_checksum=0x432c0dd4, cfg_checksum=0xa43083b8
a.gcda:    01a10000:   2:COUNTERS arcs 1 counts
a.gcda:    01a90000:   0:COUNTERS indirect_call 0 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
```

Here we something about **a.gcda** contents. It has something about:

- 5 program runs (**OBJECT_SUMMARY runs=5**)
- 4 FUNCTIONs, each contains information about:

  * **arcs**: conditional or unconditional branches
  * **indirect_call** indirect branch targets
  * **time_profiler**: count of times basic block was executed

Note that functions are identified not by name, but by a triple:

- **ident**: name hash, calculated with [coverage_compute_profile_id()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/coverage.cc;h=8ece5db680e614f8225d9e8407dd89bd27020b4d;hb=95874f95095f401405d3386e2e6695351b3f97b5#l548)
- **lineno_checksum**: source file name and line number hash, calculated with [coverage_compute_lineno_checksum()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/coverage.cc;h=8ece5db680e614f8225d9e8407dd89bd27020b4d;hb=95874f95095f401405d3386e2e6695351b3f97b5#l531)
- **cfg_checksum**: control flow graph hash, calculated with [coverage_compute_cfg_checksum()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/coverage.cc;h=8ece5db680e614f8225d9e8407dd89bd27020b4d;hb=95874f95095f401405d3386e2e6695351b3f97b5#l588)
  identifier stability can survive minor source code changes.

You need a **.gcno** file to resolve these obscure identifiers back
to source line numbers.

Let's look into individual counters of **.gcda** files. We can extract
then with **-l** option:

```
$ gcov-dump -l a.gcda

a.gcda:data:magic `gcda':version `B12*'
a.gcda:stamp 1137774494
a.gcda:  a1000000:   2:OBJECT_SUMMARY runs=5, sum_max=60
a.gcda:  01000000:   3:FUNCTION ident=108032747, lineno_checksum=0x0a7a17ea, cfg_checksum=0xc835c602
a.gcda:    01a10000:   8:COUNTERS arcs 4 counts
a.gcda:                   0: 1 4 59 4
a.gcda:    01a90000:  16:COUNTERS indirect_call 8 counts
a.gcda:                   0: 59 3 1025457522 53 1567133468 3 1634904005 3
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:                   0: 1
a.gcda:  01000000:   3:FUNCTION ident=1567133468, lineno_checksum=0xdadb6f0d, cfg_checksum=0xa43083b8
a.gcda:    01a10000:   2:COUNTERS arcs 1 counts
a.gcda:                   0: 3
a.gcda:    01a90000:   0:COUNTERS indirect_call 0 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:                   0: 3
a.gcda:  01000000:   3:FUNCTION ident=1025457522, lineno_checksum=0x745daa69, cfg_checksum=0xa43083b8
a.gcda:    01a10000:   2:COUNTERS arcs 1 counts
a.gcda:                   0: 53
a.gcda:    01a90000:   0:COUNTERS indirect_call 0 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:                   0: 2
a.gcda:  01000000:   3:FUNCTION ident=1634904005, lineno_checksum=0x432c0dd4, cfg_checksum=0xa43083b8
a.gcda:    01a10000:   2:COUNTERS arcs 1 counts
a.gcda:                   0: 3
a.gcda:    01a90000:   0:COUNTERS indirect_call 0 counts
a.gcda:    01af0000:   2:COUNTERS time_profiler 1 counts
a.gcda:                   0: 4
```

Counters are harder to interpret without the context.

For example **arcs**
just list count of each branch taken known to the current function.
The string "FUNCTION ident=108032747: arcs 4 counts: 1 4 59 4" means that first
arc was taken once, third was taken 59 times.

Looking at the above **a.c.gcov** I
would guess **FUNCTION ident=108032747** is our **main()** function,
first counter is our **if (argc < 2) return 0;** branch and the rest
are indirect call arcs to **f1()**, **f2()**, **f3()**.

**indirect_call** is more complicated: the string "indirect_call 8 counts: 59 3 1025457522 53 1567133468 3 1634904005 3"
means that there were 59 indirect calls, top 3 are listed, first
indirection had 53 calls (with **ident=1025457522** as a target, probably **f1()**),
second and third are taken 3 times (probably **f0()** and **f2()** are the targets).

**time_profiler** is the simplest one: it says how many times the
function itself was called.

Quiz question: why does **ident=108032747** (**main()** function) has
only one **time_profile** call and not 5?

# internals: libgcov

**.gcda** files are read at program startup and written at program
shutdown. Multiple program runs have an effect of merged profile statictics
from each run.

Some staticstics like **runs** are easy to merge:
just sum values together. But some like **arcs** are trickier:
how do we handle indirect calls with an unbound target fanout?
Should we store all of them? Or set a static or dynamic limit?

All these cases are handled by **-lgcov** library linked into
instrumented binaries. It's API resides in
[libgcc/libgcov.h](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov.h;h=487bd1464cd8e6f9ae4dc1ca25a68b0c8d836a74;hb=95874f95095f401405d3386e2e6695351b3f97b5#l282)
and has a few things to note:

- entry and exit points: **\_\_gcov_init()** / **\_\_gcov_exit()**.
- profiler conters: **\_\_gcov_interval_profiler()** / **\_\_gcov_time_profiler()** / **\_\_gcov_indirect_call_profiler_v4()**
- profile mergers: **\_\_gcov_merge_add()** / **\_\_gcov_merge_time_profile()** / **\_\_gcov_merge_topn()**
- counter readers: **gcov_get_counter()** / **gcov_get_counter_ignore_scaling()** / **gcov_get_counter_target()**
- hooks to catch program re-execution: **\_\_gcov_fork()** / **\_\_gcov_execl()**

Let's look at indirect call profiler details as an exampe. The other
metric types follow the same pattern.

[__gcov_indirect_call_profiler_v4()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov-profiler.c;h=57617857a93197fcbf99c682df11c66b9fb2b589;hb=95874f95095f401405d3386e2e6695351b3f97b5#l169)
implements an increment (in-memory) for **indirect_call**:

```c
void
__gcov_indirect_call_profiler_v4 (gcov_type value, void *cur_func)
{
  __gcov_indirect_call_profiler_body (value, cur_func, 0);
}

/* By default, the C++ compiler will use function addresses in the
   vtable entries.  Setting TARGET_VTABLE_USES_DESCRIPTORS to nonzero
   tells the compiler to use function descriptors instead.  The value
   of this macro says how many words wide the descriptor is (normally 2).

   It is assumed that the address of a function descriptor may be treated
   as a pointer to a function.  */

/* Tries to determine the most common value among its inputs. */
static inline void
__gcov_indirect_call_profiler_body (gcov_type value, void *cur_func,
                                    int use_atomic)
{
  /* If the C++ virtual tables contain function descriptors then one
     function may have multiple descriptors and we need to dereference
     the descriptors to see if they point to the same function.  */
  if (cur_func == __gcov_indirect_call.callee
      || (__LIBGCC_VTABLE_USES_DESCRIPTORS__
          && *(void **) cur_func == *(void **) __gcov_indirect_call.callee))
    __gcov_topn_values_profiler_body (__gcov_indirect_call.counters, value,
                                      use_atomic);

  __gcov_indirect_call.callee = NULL;
}

/* Tries to determine N most commons value among its inputs.  */

static inline void
__gcov_topn_values_profiler_body (gcov_type *counters, gcov_type value,
                                  int use_atomic)
{
  gcov_topn_add_value (counters, value, 1, use_atomic, 1);
}

/* Add key value pair VALUE:COUNT to a top N COUNTERS.  When INCREMENT_TOTAL
   is true, add COUNT to total of the TOP counter.  If USE_ATOMIC is true,
   do it in atomic way.  Return true when the counter is full, otherwise
   return false.  */

static inline unsigned
gcov_topn_add_value (gcov_type *counters, gcov_type value, gcov_type count,
                     int use_atomic, int increment_total)
{
  // ...
```

Here we see that **indirect_call** is a **topn** style counter as it's handled by
[gcov_topn_add_value()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov.h;h=487bd1464cd8e6f9ae4dc1ca25a68b0c8d836a74;hb=95874f95095f401405d3386e2e6695351b3f97b5#l487).


```c
static inline unsigned
gcov_topn_add_value (gcov_type *counters, gcov_type value, gcov_type count,
                     int use_atomic, int increment_total)
{
  if (increment_total)
    {
      /* In the multi-threaded mode, we can have an already merged profile
         with a negative total value.  In that case, we should bail out.  */
      if (counters[0] < 0)
        return 0;
      gcov_counter_add (&counters[0], 1, use_atomic);
    }

  struct gcov_kvp *prev_node = NULL;
  struct gcov_kvp *minimal_node = NULL;
  struct gcov_kvp *current_node  = (struct gcov_kvp *)(intptr_t)counters[2];

  while (current_node)
    {
      if (current_node->value == value)
        {
          gcov_counter_add (&current_node->count, count, use_atomic);
          return 0;
        }
  // ...
  return 0;
}
```

A few things to note here:

- **gcov_topn_add_value()** increments **counters[ix]** where **ix** matches **value** being counted.
- **gcov_topn_add_value()** increments **counters[0]** as it treats it as "total"
- negative "total" values are special and are related to merged values (how?)

Let's look at the merge function for [__gcov_merge_topn()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov-merge.c;h=89741f637e1efae86d193762b668015c36243098;hb=95874f95095f401405d3386e2e6695351b3f97b5#l89):

```c
/* The profile merging function for choosing the most common value.
   It is given an array COUNTERS of N_COUNTERS old counters and it
   reads the same number of counters from the gcov file.  The counters
   are split into pairs where the members of the tuple have
   meanings:

   -- the stored candidate on the most common value of the measured entity
   -- counter

   We use -TOTAL for situation when merging dropped some values.
   The information is used for -fprofile-reproducible flag.
   */

void
__gcov_merge_topn (gcov_type *counters, unsigned n_counters)
{
  gcc_assert (!(n_counters % GCOV_TOPN_MEM_COUNTERS));

  for (unsigned i = 0; i < (n_counters / GCOV_TOPN_MEM_COUNTERS); i++)
    {
      /* First value is number of total executions of the profiler.  */
      gcov_type all = gcov_get_counter_ignore_scaling (-1);
      gcov_type n = gcov_get_counter_ignore_scaling (-1);

      unsigned full = all < 0;
      gcov_type *total = &counters[GCOV_TOPN_MEM_COUNTERS * i];
      *total += full ? -all : all;

      for (unsigned j = 0; j < n; j++)
        {
          gcov_type value = gcov_get_counter_target ();
          gcov_type count = gcov_get_counter_ignore_scaling (-1);

          // TODO: we should use atomic here
          full |= gcov_topn_add_value (counters + GCOV_TOPN_MEM_COUNTERS * i,
                                       value, count, 0, 0);
        }

      if (full)
        *total = -(*total);
    }
}
```

Once again we see that first pair of key/value is treated as "total" count
of calls. Merger is the place where overflow of **gcov_topn_add_value()**
is detected and "total" is stored as a negative value to show this fact.

All the counter types are written on disk with
[write_once_data()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov-driver.c;h=d4517d269eb32ca84ee1dfa069bda4e872a1ac98;hb=95874f95095f401405d3386e2e6695351b3f97b5#l502).
From there we see that
[write_topn_counters()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov-driver.c;h=d4517d269eb32ca84ee1dfa069bda4e872a1ac98;hb=95874f95095f401405d3386e2e6695351b3f97b5#l432)
dumps them as:

```c
/* Store all TOP N counters where each has a dynamic length.  */

static void
write_topn_counters (const struct gcov_ctr_info *ci_ptr,
                     unsigned t_ix,
                     gcov_unsigned_t n_counts,
                     void (*dump_fn) (const void *, unsigned, void *),
                     void *(*allocate_fn)(unsigned, void *),
                     void *arg)
{
  // ...
  unsigned disk_size = GCOV_TOPN_DISK_COUNTERS * counters + 2 * pair_total;
  dump_unsigned (GCOV_TAG_FOR_COUNTER (t_ix), dump_fn, arg),
  dump_unsigned (GCOV_TAG_COUNTER_LENGTH (disk_size), dump_fn, arg);

  for (unsigned i = 0; i < counters; i++)
    {
      dump_counter (ci_ptr->values[GCOV_TOPN_MEM_COUNTERS * i], dump_fn, arg);
      dump_counter (list_sizes[i], dump_fn, arg);
      gcov_type start = ci_ptr->values[GCOV_TOPN_MEM_COUNTERS * i + 2];

      unsigned j = 0;
      for (struct gcov_kvp *node = (struct gcov_kvp *)(__INTPTR_TYPE__)start;
           j < list_sizes[i]; node = node->next, j++)
        {
          dump_counter (node->value, dump_fn, arg);
          dump_counter (node->count, dump_fn, arg);
        }
    }
}
```

This just writes key/value pairs on disk. Nothing fancy. No special handling
of negative values. This makes on-disk format rougly match in-memory format.

Now we can alswer our quiz question on why **time_profiler** still has a
value of **1** for **main()** even after 5 program runs.
[__gcov_merge_time_profile()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=libgcc/libgcov-merge.c;h=89741f637e1efae86d193762b668015c36243098;hb=95874f95095f401405d3386e2e6695351b3f97b5#l68)
tells us:

```
/* Time profiles are merged so that minimum from all valid (greater than zero)
   is stored. There could be a fork that creates new counters. To have
   the profile stable, we chosen to pick the smallest function visit time.  */
void
__gcov_merge_time_profile (gcov_type *counters, unsigned n_counters)
{
  unsigned int i;
  gcov_type value;

  for (i = 0; i < n_counters; i++)
    {
      value = gcov_get_counter_target ();

      if (value && (!counters[i] || value < counters[i]))
        counters[i] = value;
    }
}
```

The counter stores fastest execution for the function. Not a sum of
execution times (as I thought initially).

The takeaways here are:

- **gcc** implements a few types of counters: histogram-style **TOPN** counters,
  simpler numeric counters and a few others.
- merge policies for counters are dependent on their exact meaning; they are not cimplicated but have a few
  gotchas like negative values.
- **libgcov** provides runtime for both program being profiled and **gcov** standalone tool

# internals: gcc-emitted code

Let's check how counters are embedded into our program. I suggest looking at the
optimised **GIMPLE** tree. Here is our original program:

```c
#include <stddef.h>

/* Keep the functions around to have real indirect calls. */
#define DECL_F(fn) static void fn(void) __attribute__((noipa))
#define  DEF_F(fn) static void fn(void) {}

DECL_F(f0); DEF_F(f0);
DECL_F(f1); DEF_F(f1);
DECL_F(f2); DEF_F(f2);

int main (int argc, char *argv[]) {
    if (argc < 2) return 0;

    static const void(*ft[])(void) = { &f0, &f1, &f2, };
    size_t sz = sizeof (ft) / sizeof (ft[0]);

    for (const char * p = argv[1]; *p; p++)
        ft[*p % sz]();

    return 0;
}
```

We will dump final **GIMPLE** with **-fdump-tree-optimized** flag.
I'll paste the output in 3 pieces (as the output is quite big):

- **main()** function
- **f0()** function
- constructor/destructor

Let's dump **GIMPLE** first:

```
$ gcc -c a.c -fprofile-generate -fprofile-arcs -ftest-coverage -O2 -fdump-tree-optimized
$ cat a.c.244t.optimized
```

Her is our **main()**:

```
;; Function main (main, funcdef_no=3, decl_uid=1966, cgraph_uid=4, symbol_order=3) (executed once)

__attribute__((access ("^1[ ]", )))
int main (int argc, char * * argv)
{
  const char * p;
  static void (*<T34b>) (void) ft[3] = {f0, f1, f2};
  long unsigned int _1;
  long unsigned int _2;
  void (*<T34b>) (void) _3;
  char _4;
  long int PROF_edge_counter_14;
  long int PROF_edge_counter_15;
  long int PROF_edge_counter_16;
  long int PROF_edge_counter_17;
  long int PROF_edge_counter_18;
  long int PROF_edge_counter_19;
  long int PROF_edge_counter_20;
  long int PROF_edge_counter_21;
  long int _23;
  long int time_profile_24;
  long int time_profile_25;
  void * _26;
  char _39;
  char _40;

  <bb 2> [local count: 160390549]:
  _26 = __gcov_indirect_call.D.1993;
  if (_26 != 0B)
    goto <bb 3>; [20.00%]
  else
    goto <bb 4>; [80.00%]

  <bb 3> [local count: 160390549]:
  __gcov_indirect_call_profiler_v4 (108032747, main);

  <bb 4> [local count: 160390549]:
  _23 = __gcov7.main[0];
  if (_23 == 0)
    goto <bb 5>; [20.00%]
  else
    goto <bb 6>; [80.00%]

  <bb 5> [local count: 160390549]:
  time_profile_24 = __gcov_time_profiler_counter;
  time_profile_25 = time_profile_24 + 1;
  __gcov7.main[0] = time_profile_25;
  __gcov_time_profiler_counter = time_profile_25;

  <bb 6> [local count: 160390549]:
  if (argc_8(D) <= 1)
    goto <bb 7>; [26.36%]
  else
    goto <bb 8>; [73.64%]

  <bb 7> [local count: 42278949]:
  PROF_edge_counter_14 = __gcov0.main[0];
  PROF_edge_counter_15 = PROF_edge_counter_14 + 1;
  __gcov0.main[0] = PROF_edge_counter_15;
  goto <bb 11>; [100.00%]

  <bb 8> [local count: 118111600]:
  PROF_edge_counter_16 = __gcov0.main[1];
  PROF_edge_counter_17 = PROF_edge_counter_16 + 1;
  __gcov0.main[1] = PROF_edge_counter_17;
  p_11 = MEM[(char * *)argv_10(D) + 8B];
  _39 = *p_11;
  if (_39 != 0)
    goto <bb 9>; [89.00%]
  else
    goto <bb 10>; [11.00%]

  <bb 9> [local count: 955630226]:
  # _40 = PHI <_4(9), _39(8)>
  # p_41 = PHI <p_13(9), p_11(8)>
  _1 = (long unsigned int) _40;
  _2 = _1 % 3;
  _3 = ft[_2];
  __gcov_indirect_call.D.1994 = &__gcov4.main[0];
  __gcov_indirect_call.D.1993 = _3;
  _3 ();
  PROF_edge_counter_18 = __gcov0.main[2];
  PROF_edge_counter_19 = PROF_edge_counter_18 + 1;
  __gcov0.main[2] = PROF_edge_counter_19;
  p_13 = p_41 + 1;
  _4 = MEM[(const char *)p_13];
  if (_4 != 0)
    goto <bb 9>; [89.00%]
  else
    goto <bb 10>; [11.00%]

  <bb 10> [local count: 118111600]:
  PROF_edge_counter_20 = __gcov0.main[3];
  PROF_edge_counter_21 = PROF_edge_counter_20 + 1;
  __gcov0.main[3] = PROF_edge_counter_21;

  <bb 11> [local count: 160390549]:
  return 0;

}
```

While it's a lot of code each counter handling is straightforward:

- read the counter out
- update the counter
- write it back


Here we already see a few glbal variables being updated:

- **\_\_gcov_time_profiler_counter**: global timer counter gets incremented once.
- **\_\_gcov7.main[0]**: **main** timer counter gets incremented once.
- **\_\_gcov0.main[0,1,2,3]**: **main** arc counters (to **f0()**, **f1()**, **f2()**)

Note that **main()** does build explicitl arcs to
**f0()**/**f1()****f2()** but it does not know where indirections leads
it to. Thus **gcc** stores indirection to **\_\_gcov_indirect_call.D.1993**
global.

TO maintain the indirect counters **gcc** relies on target to increment it:

```
;; Function f2 (f2, funcdef_no=2, decl_uid=1960, cgraph_uid=3, symbol_order=2)

__attribute__((noipa, noinline, noclone, no_icf))
void f2 ()
{
  long int PROF_edge_counter_2;
  long int PROF_edge_counter_3;
  long int _4;
  long int time_profile_5;
  long int time_profile_6;
  void * _7;

  <bb 2> [local count: 1073741824]:
  _7 = __gcov_indirect_call.D.1993;
  if (_7 != 0B)
    goto <bb 3>; [20.00%]
  else
    goto <bb 4>; [80.00%]

  <bb 3> [local count: 1073741824]:
  __gcov_indirect_call_profiler_v4 (1567133468, f2);

  <bb 4> [local count: 1073741824]:
  PROF_edge_counter_2 = __gcov0.f2[0];
  PROF_edge_counter_3 = PROF_edge_counter_2 + 1;
  __gcov0.f2[0] = PROF_edge_counter_3;
  _4 = __gcov7.f2[0];
  if (_4 == 0)
    goto <bb 5>; [20.00%]
  else
    goto <bb 6>; [80.00%]

  <bb 5> [local count: 1073741824]:
  time_profile_5 = __gcov_time_profiler_counter;
  time_profile_6 = time_profile_5 + 1;
  __gcov7.f2[0] = time_profile_6;
  __gcov_time_profiler_counter = time_profile_6;

  <bb 6> [local count: 1073741824]:
  return;

}

;; Function f1 (f1, funcdef_no=1, decl_uid=1955, cgraph_uid=2, symbol_order=1)
... same as f0
;; Function f2 (f0, funcdef_no=0, decl_uid=1950, cgraph_uid=1, symbol_order=0)
... same as f2
```

Here the same **\_\_gcov_indirect_call.D.1993** is being used to increment the counter
in case **f2()** is indirection target. If **f2()** is called directly then this
global would not be populated.

```
;; Function _sub_I_00100_0 (_sub_I_00100_0, funcdef_no=4, decl_uid=2028, cgraph_uid=6, symbol_order=17) (executed once)

void _sub_I_00100_0 ()
{
  <bb 2> [local count: 1073741824]:
  __gcov_init (&*.LPBX0); [tail call]
  return;

}

;; Function _sub_D_00100_1 (_sub_D_00100_1, funcdef_no=5, decl_uid=2031, cgraph_uid=7, symbol_order=18) (executed once)

void _sub_D_00100_1 ()
{
  <bb 2> [local count: 1073741824]:
  __gcov_exit (); [tail call]
  return;
}
```

And here we see **-lgcov** startup and shutdown code in constructor and
destructor. Simple!

# the actual bug

Now if all the above has some sense to you then <https://gcc.gnu.org/PR105282>
should look less mysterious. There **python** managed to ICE **gcc** when
program was optimised with training data available.

Let's look at the minimised example:

```c
#include <stddef.h>

typedef void (*cb_t)(void);
#define F(__fn) static void __fn(void) {}

F(f00);F(f01);F(f02);F(f03);F(f04);F(f05);F(f06);F(f07);F(f08);F(f09);
F(f10);F(f11);F(f12);F(f13);F(f14);F(f15);F(f16);F(f17);F(f18);F(f19);
F(f20);F(f21);F(f22);F(f23);F(f24);F(f25);F(f26);F(f27);F(f28);F(f29);
F(f30);F(f31);F(f32);F(f33);F(f34);F(f35);F(f36);F(f37);F(f38);F(f39);
F(f40);F(f41);F(f42);F(f43);F(f44);F(f45);F(f46);F(f47);F(f48);F(f49);

static void f(int i) {
    /* Needs to be bigger than gcc's GCOV_TOPN_MAXIMUM_TRACKED_VALUES == 32
     * to overflow GCOV_COUNTER_V_INDIR couter type.
     */
    static const cb_t fs[] = {
        &f00,&f01,&f02,&f03,&f04,&f05,&f06,&f07,&f08,&f09,
        &f10,&f11,&f12,&f13,&f14,&f15,&f16,&f17,&f18,&f19,
        &f20,&f21,&f22,&f23,&f24,&f25,&f26,&f27,&f28,&f29,
        &f30,&f31,&f32,&f33,&f34,&f35,&f36,&f37,&f38,&f39,
        &f40,&f41,&f42,&f43,&f44,&f45,&f46,&f47,&f48,&f49,
    };
    size_t sz = sizeof (fs) / sizeof (fs[0]);
    fs[i % sz]();
}

int l(int argc, char * argv[]);

int main(int argc, char *argv[]) {
    if (argc == 1)
      for (unsigned int i = 0; i < 25; i++)
        f(i);
    if (argc == 2)
      for (unsigned int i = 25; i < 50; i++)
        f(i);
}
```

ICE on **gcc-12** or **gcc-11**:

```
$ gcc -flto -O0 a.c -fprofile-generate -o a
$ ./a # populate first 25 buckets
$ ./a 1 # populate 25 more buckets, cause overflow
$ gcc -flto -O0 a.c -fprofile-use -o a

during IPA pass: modref
a.c:36:1: internal compiler error: in stream_out_histogram_value, at value-prof.cc:340
   36 | }
      | ^
0x8351fb stream_out_histogram_value(output_block*, histogram_value_t*)
        ../../gcc-12-20220410/gcc/value-prof.cc:340
0x1c848c0 output_gimple_stmt
        ../../gcc-12-20220410/gcc/gimple-streamer-out.cc:192
0x1c848c0 output_bb(output_block*, basic_block_def*, function*)
        ../../gcc-12-20220410/gcc/gimple-streamer-out.cc:227
0xdc91ad output_function
        ../../gcc-12-20220410/gcc/lto-streamer-out.cc:2453
0xdc91ad lto_output()
        ../../gcc-12-20220410/gcc/lto-streamer-out.cc:2796
0xe57b11 write_lto
        ../../gcc-12-20220410/gcc/passes.cc:2762
0xe57b11 ipa_write_summaries_1
        ../../gcc-12-20220410/gcc/passes.cc:2826
0xe57b11 ipa_write_summaries()
        ../../gcc-12-20220410/gcc/passes.cc:2882
0xaac060 ipa_passes
        ../../gcc-12-20220410/gcc/cgraphunit.cc:2209
0xaac060 symbol_table::compile()
        ../../gcc-12-20220410/gcc/cgraphunit.cc:2282
0xaaea77 symbol_table::compile()
        ../../gcc-12-20220410/gcc/cgraphunit.cc:2262
0xaaea77 symbol_table::finalize_compilation_unit()
        ../../gcc-12-20220410/gcc/cgraphunit.cc:2530
```

Here the bug is in **stream_out_histogram_value()** function which
tried to stream out on disk perfectly valid profile details around
**main()**:

```
$ gcov-dump -l a.gcda
...
a.gcda:    01a90000: 528:COUNTERS indirect_call 66 counts
a.gcda:                   0: -50 32 1456173180 1 1792104613 1 918340114 1
a.gcda:                   8: 1406444659 1 263798468 1 1664310260 1 1063174467 1
a.gcda:                  16: 1596551981 1 54847898 1 533075953 1 1135316294 1
a.gcda:                  24: 601636648 1 2142348703 1 450479102 1 1186224457 1
a.gcda:                  32: 416313568 1 1153296983 1 617240633 1 2024260238 1
a.gcda:                  40: 1680162021 1 944285266 1 1480528956 1 72519307 1
a.gcda:                  48: 1631250666 1 1029141085 1 941945699 1 1682532820 1
a.gcda:                  56: 71228346 1 1481851149 1 1154596710 1 414983633 1
a.gcda:                  64: 2026608575 1
```

The **-50** is our "total" count of indirections. It's negative because
**25** calls from first run were merged with **25** calls from second call.
Histograms have a limit of **32** unique values. The other 18 were discarded.
Negative count signals this fact.

[gcc/value-prof.cc:stream_out_histogram_value()](https://gcc.gnu.org/git/?p=gcc.git;a=blob;f=gcc/value-prof.c;h=42748771192f8302cfe637d23d589848d9b8fcb7;hb=1eee4e08a64f6e931b0c5dd1bb854a2b7ad3d58f#l311)
was too strict on it's assumption around counter types:

```cc
void
stream_out_histogram_value (struct output_block *ob, histogram_value hist)
{
  // ...
  for (i = 0; i < hist->n_counters; i++)
    {
      /* When user uses an unsigned type with a big value, constant converted
         to gcov_type (a signed type) can be negative.  */
      gcov_type value = hist->hvalue.counters[i];
      if (hist->type == HIST_TYPE_TOPN_VALUES
          || hist->type == HIST_TYPE_IOR)
        /* Note that the IOR counter tracks pointer values and these can have
           sign bit set.  */
        ;
      else
        gcc_assert (value >= 0);

      streamer_write_gcov_count (ob, value);
    }
  if (hist->hvalue.next)
    stream_out_histogram_value (ob, hist->hvalue.next);
}
```

Here **gcc** asserts that every value in **TOPN** couters has to be non-negative.
Was trivial [to fix](https://gcc.gnu.org/git/?p=gcc.git;a=commitdiff;h=7b879564ec2bda6b5441fbaf231d70ec6359db01)
by skipping first "total" counter that could be negative on overflows like above:

```diff
--- a/gcc/value-prof.c
+++ b/gcc/value-prof.c
@@ -336,6 +336,10 @@ stream_out_histogram_value (struct output_block *ob, histogram_value hist)
        /* Note that the IOR counter tracks pointer values and these can have
           sign bit set.  */
        ;
+      else if (hist->type == HIST_TYPE_INDIR_CALL && i == 0)
+       /* 'all' counter overflow is stored as a negative value. Individual
+          counters and values are expected to be non-negative.  */
+       ;
       else
        gcc_assert (value >= 0);
```

So much behind this three-liner.

# final words

Profiler bugs are frequently hard. Mechanical reduction on real program
is usually very fragile: most material code changes change both generated
instrumented code and break the program on training run. It's "infeasible"
to incrementally reduce both program and input data for large projects
like **firefox** or **python**.

I say "infeasible" as I successully reduced
**tauthon** once in <https://gcc.gnu.org/PR96394>. It took me almost 2 months
to extract small reproducer manually. I don't think I'll do it ever again :)

First time it was fun: I found a lot more than I imagined about **python**
implementation: how it embeds it's own bytecode into executable, how bootstrap
python loads the rest of python modules using this bootstrap code.

When faced with another prof failure in **python** in <https://gcc.gnu.org/PR105282>
I initially thought it would be impossible for me to do it again from
scratch. On top of that it added **-flto** to make things even less
manageable. But I was lucky to reproduce the crash and derive the
reproducer out of **gdb** backtrace.

**PGO**-style builds have another unusual property: they are very dependent
on input training data and on any internal non-determinism your program
has. For example, if your program uses random-seeded hash tables you
will get slightly different profile outputs from run to run.

Slightly different profile outputs lead to slightly different
optimization decision during compilation. Different decisions lead to
different output binaries. Without special effort you will probably get
unique binary every time you build a real world project with **PGO**
support.

It might not be a big deal with regards to performance of final binary.
But it might be quite a headache if one of such rare states causes
compiler to generate invalid result. It might be very hard to reproduce
(and fix).

But if you have to deal with **PGO** bugs then try to look straight into
what **gcc** is doing.

Have fun!
