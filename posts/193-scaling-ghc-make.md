---
title: scaling ghc --make
date: October 1, 2016
---

## `Tl;DR`

To speed up build of a `haskell` package use:

```
$ ghc --make -j +RTS -A256M -qb0 -RTS <your-args>
```

## Story mode

In the previous post I've shown a tiny hack on how to make sense out of
`ghc` using `perf`. My actual goal was to get basic understanding on why
`ghc --make` does not scale perfectly.

The scalability problem is known as [`GHC` trac
#9221](http://ghc.haskell.org/trac/ghc/ticket/9221).

## Why `ghc --make -j` scalability matters

I care about `ghc --make` speed because I build a lot of `haskell`
packages as part of
[`gentoo-haskell`](https://github.com/gentoo-haskell/gentoo-haskell/commits/master)
overlay maintenance.

My workflow to add or update a new package is simple:

- quickly build new version of package (or packages) against my existing
  `haskell` world (`~1500` globally installed `haskell` packages) and run
  its tests
- install this new version in system and rebuild rest of `haskell`
  packages against it

Installation and rebuild usually happens overnight.

Quick build is a relatively interactive phase. It fails frequently as
packages require some tweaks along the way, like upper bound fixes or
minor API changes.

I use various hacks to speed up quick builds:

- build with optimisations disabled: `-O0`
- build in parallel `ghc --make -j` mode

## The symptoms

My machine is a `x86_64` desktop (`4x2` HT cores on a single die). Nothing
fancy. Should be an easy target to get perfect parallelism. Right?
At first I chose `highlightling-kate` package as it has at least 100
independent modules for various languages.
The `ghc --make -j<N>` histogram looks like that:

![image](https://ghc.haskell.org/trac/ghc/raw-attachment/ticket/9221/graph1.png)

The problem is that even on my machine `ghc --make -j3` works better
than `ghc --make -j8` and I could not get more than `1.5x` speedup.
I'd like to see `8x` speedup. Or at least `4x` :)
Another problem is quick performance degradation once you exceed number
of `CPUs`. Running `ghc --make -j20` on my machine makes things
seriously worse: compilation time is longer than in non-parallel mode!
That did not look good.

## The clues

So why are things so bad? And more importantly how would you diagnose
similar issue in another `haskell` program? The problem looked like a nice
thing to get basic experience in this area.
In [comment #9](https://ghc.haskell.org/trac/ghc/ticket/9221#comment:9)
Gintas found one of the problems with large `<N>` but I was not able
to understand his reasoning at that time as I knew too little about
`GHC` runtime.
Simon suggested larger heap sizes and allocation areas. I tried it and
got better performance numbers! Best numbers were at `-A 256M`:
`2.5x` speedup! But what is the real bottleneck here? How to estimate
the number in general case?
The bug contained a lot of speculation about lock contentions of sorts
but no simple cause of slowness.

## Down the rabbit hole

I've decided to try to find out what happens there. Maybe someone else
will have an **AHA!** moment and will fix things for everyone if I'll
manage to find out that useless `sleep(1)` somewhere :)

### `perf`, take 1

I tried to run `perf record` on my benchmark to figure out what
happens with the process. `perf` suggested `GHC` did mostly
`sched_yield()` which does not sound like a useful call.

### `threadscope`, take 1

Simon also suggested to try `ThreadScope` on `GHC` itself. In theory
it's quite simple. You need to relink `ghc-stage2` with `-eventlog`
as:

```Makefile
# mk/build.mk
GhcStage2HcOpts += -rtsopts -eventlog
```

Run `GHC` with event logging enabled as `ghc --make ... +RTS -l`.
Then running `threadscope ghc-stage2.eventlog` should show us nice
graph of all sorts of interesting things.
Alas `threadscope` crashed on generated `eventlog` file. I first thought
it's a `threadscope` bug and [filed
one](https://github.com/haskell/ThreadScope/issues/37).
After `threadscope` got a fix it still could not read my `eventlog`
profiles. I've misattributed a bug to another [ghc bug
#9003](https://ghc.haskell.org/trac/ghc/ticket/9003) where `GHC`
accidentally broke `eventlog` ABI by changing enum values for existing
events in one of `GHC` releases.
After `bug #9003` got fixed my dumps still did not render! I've filed
third [`ghc` bug #9384](https://ghc.haskell.org/trac/ghc/ticket/9384)
which basically says that calling `setNumCapabilities` in your `haskell`
program usually breaks your `eventlog`.
I didn't think of trying to fix it. In the process of finding the
trigger I've found out the workaround: if you specify rts option `-RTS
-N<N>` at program startup to the same value you try to
`setNumCapabilities` it will work just fine! Running `ghc --make
-j<N> +RTS -N<N> -l` will not change `GHC` behavior except it
will produce readable `eventlogs`!
I looked at the `eventlog` output and did not know what I should look at.

Later Alexander Vershilov fixed that bug with [2 lines of `C`
code](https://git.haskell.org/ghc.git/commitdiff/2edb4a7bd5b892ddfac75d0b549d6682a0be5c02).
Tl;DR: `GHC` did not handle dynamic resize of number of capabilities.
Thank you!

### `perf`, take 2

Half a year later many more things happened:

- Peter's `DWARF` support for `GHC` landed and I've found a few
  bugs in it:
  - [`ghc` bug
    #10655](https://ghc.haskell.org/trac/ghc/ticket/10665#comment:3)
    where `-g` breaks rewrite rules
  - [`ghc` bug
    #10667](https://ghc.haskell.org/trac/ghc/ticket/10667#comment:5)
    where `-g` breaks `GNU as` and produces broken assembly file
- I've mastered black magic of porting `GHC` to weird architectures:
  basically built unregisterised `GHC` on `amd64` and made it pass most
  tests. Some details are in this [blog
  post](/posts/187-fixing-ghc-on-sparc-ia64-and-friends.html).
  I've started Seeing Things!

I've attempted to use `perf` and found out the reason why `perf` can't
disassemble `haskell` function and wrote another blog post on how to [make
it see the
code](/posts/192-perf-on-haskell-programs.html)
(still needs more things to be upstreamed).
`perf` also suggested that one of most popular user space functions
being called was the `stg_BLACKHOLE_info()`.

## Ad-hoc `rts` patching

`Blackholes` are special kind of thunks that usually denote a thunk being
evaluated by another `haskell` thread. When a normal (closure) thunk is
being evaluated by a thread thunk could be updated at two points in
time: either before (eager) or after (lazy) actual thunk evaluation.
By default `GHC` uses lazy strategy (unless `-feager-blackholing` is
used). That means the same thunk can be evaluated by multiple execution
threads. But it is fine for a typical pure program.
It's not fine for sensitive things like `unsafePerformIO` thunks
which are guaranteed to be evaluated once.
For those sensitive thunks `GHC` uses `blackholes`: at enter closure
type is atomically rewritten to `BLACKHOLE` thunk which points to a
thread (`TSO`) that locks this thunk. When any other thread tries to
enter `blackhole` it blocks on that thunk. When first thread finishes
computation it rewrites a `TSO` pointed by `blackhole` to an actual
result of computation and resumes second thread. Second thread picks
result of evaluation and continues execution.
Thus my assumption was that the main scalability problem was large
amount of `unsafePerformIO` calls that causes high `haskell` thread
rescheduling churn.

I tried to modify `cmm` code to print an object pointed by `blackhole`
but most of the time those are not `TSO` objects but `I#`
(integers), labels and other things. `GHC` indeed uses
`unsafePerformIO` for global unique string pool. Those `I#` are
likely the sign of it (see `mkSplitUniqSupply`).

I tried to recompile `GHC` with `-feager-blackholing` option
enabled. `GHC` became slightly slower in single-threaded case
(expected), slightly slower in multi-threaded case (unexpected) and did
not degrade as fast as base case on large `-j<N>` (totally
unexpected!).
It means that `GHC` evaluates some expensive things more than once.
How would we find which ones? `GHC` needs to have something in `rts` or
in `eventlog` to expose stats for:

- `blackhole` synchronization (who stalls other threads?)
- things needlessly evaluated more than once (what wastes CPU/heap?)

## speeding up parallel `GC`: `-qb0`

The more I tried to play with various `GHC` changes the more I
realized my machine is not good enough to expose bottlenecks. I decided
to pick a 24-core virtual machine and gave `threadscope` another chance.
This time `haskell` threads were frequently stopped by garbage collector
threads. And garbage collector CPU load was unevenly balanced across
cores!
I started reading a [parallel `GC` paper by
Simon](http://community.haskell.org/~simonmar/papers/parallel-gc.pdf)
which describes in detail how things work.
Basically there are two phases of parallel GC:

- non-work-stealing phase when each `GC` thread scans it's own
  `Capability`'s `TSO` stacks and allocation area (aka generation-0, `g0`,
  `eden`, nursery)
- work-stealing phase when each `GC` thread either processes its queue
  of `from-space` pointers and copies things to `to-space` or steals work
  from other threads queues

`GHC` as a compiler does not take too much heap to compile a file.
Having huge allocation area (`-A256M` in my case) is enough to compile
an average file. Older generations (`g1+`) don't have much to scan
because there is nothing to put there!
It's fun that `+RTS -sstderr` always showed me that `GC` parallelism
number and I did not even notice it:

``` 
Parallel GC work balance: 22.34% (serial 0%, perfect 100%) # -qb1, default
...
Parallel GC work balance: 74.48% (serial 0%, perfect 100%) # -qb0
...
Parallel GC work balance: 80.03% (serial 0%, perfect 100%) # -qb0
```

The bottleneck is allocation area scan. `GHC` `RTS` happens to
have a knob to enable work-stealing on `g0`. The option is `-qb0`.
It was a magic knob that increased `GHC` parallelism from `2.5x` to
`7x` speed up on 24-core VM! I've got similar effect on my 8-CPU
desktop where speedup was from `2.5x` to `3.5x`.
Two pictures building `highlighting-kate` on 24-core VM without and with
`-qb0`:

- [`-N24 -j24`](http://code.haskell.org/~slyfox/T9221/ghc-stage2.eventlog.N24.j24.png)
- [`-N24 -j24 -A256 -qb0`](http://code.haskell.org/~slyfox/T9221/ghc-stage2.eventlog.N24.j24.A256M.qb0.png)

20 seconds to 9 seconds shrink. First picture shows that `GHC`
struggles to load half the CPUs at peak. While second picture eats all
24 at peak and then tails on slow modules. `GREEN` - mutation CPU usage,
`ORANGE` - `GC` CPU usage.
It also became obvious that having more `GC` threads than available CPUs
always hurts performance.
That's what Gintas meant by `setNumCapabilities` discrepancy. The
first fix [was
obvious](https://git.haskell.org/ghc.git/commitdiff/9d175605e52fd0d85f2548896358d96ee441c7e4).
It does not fix the normal case but at least does not make things worse
when `ghc --make -j<N>` is higher than the number of CPUs available.

To improve normal case `GHC` now [auto-tunes parallel scan based on
allocation area
size](https://git.haskell.org/ghc.git/commitdiff/a5d26f26d33bc04f31eaff50b7d633444192b4cb).

## multithreadng vs. multiprocessing

`Threadscope` also showed that load imbalance on `highlighting-kate` is
high due to uneven workload on `haskell` threads. Basically, not all
highlighting modules are equal in size. Which is obvious in the
hindsight. So much for a perfect benchmark :)
I've written synthetic benchmark `synth.bash` which generates a lot
of independent `haskell` sources of even size and measures its build
time:

```bash
#!/bin/bash

MODULES=128
FIELDS=100

GHC=ghc
GHC=~/dev/git/ghc-perf/inplace/bin/ghc-stage2

rm -rf src/
mkdir -p src

for m in `seq 1 ${MODULES}`; do
    {
        echo "module M${m} where"
        echo "data D = D0"
        for f in `seq 1 ${FIELDS}`; do
            echo "   | D${f} { f${f} :: Int}"
        done
        echo "    deriving (Read, Show, Eq, Ord)"
    } > src/M${m}.hs
done

#perf record -- \
time \
$GHC \
    -hide-all-packages -package=base \
    \
    --make src/*.hs -j +RTS -A256M -RTS "$@"
```

Nice to tweak numbers to adapt to small and large machines. It's also
interesting to compare with multiprocess `GHC` using the following
`Makefile`:

``` Makefile
OBJECTS := $(patsubst %.hs,%.o,$(wildcard src/*.hs))
all: $(OBJECTS)
src/%.o: src/%.hs
        ~/dev/git/ghc-perf/inplace/bin/ghc-stage2 -c +RTS -A256M -RTS $< -o $@
clean:
        $(RM) $(OBJECTS)
.PHONY: clean
```

Note the difference: here we run separate `ghc -c` processes on each
`.hs` file and let `GNU make` do the scheduling.
This benchmark raises even more questions! It shows ([see comment
#65](https://ghc.haskell.org/trac/ghc/ticket/9221#comment:65)) that
multiprocess compilation scales perfectly and is fastest at `make
-j<N>` where `N` is the number of `CPUs`. It's even faster that `ghc
--make -j` (both for 8-CPU and 24-CPU machines).
It means there is still a few issues to understand here :)
This benchmark also shows what best performance I should expect from
`GHC` on these machines. Multiprocess (`Makefile`-based) benchmark
is expected to have ideal scalability. If not it means bottleneck is
somewhere outside `GHC` (kernel/RAM bandwidth/etc.).

`4x2` HT desktop:

``` 
$ make clean; time make -j1
real    1m2.561s
user    0m56.523s
sys     0m5.560s

$ make clean; time make -j4

real    0m18.936s
user    1m7.549s
sys     0m6.857s

$ make clean; time make -j8

real    0m15.964s
user    1m52.058s
sys     0m9.929s
```

It's `3.93x` (almost exactly `4x`). Almost ideal saturation of 4
physical cores.

`12x2` HT VM:

``` 
$ make clean; time make -j1

real    1m33.147s
user    1m20.836s
sys     0m11.556s

$ make clean; time make -j12

real    0m10.537s
user    1m36.276s
sys     0m16.948s

$ make clean; time make -j24

real    0m7.336s
user    2m15.936s
sys     0m19.004s
```

This is `12.7x` speedup. Better-than-ideal saturation of 12 physical
cores.
Note how confusing user time in `-j1` / `-j12` / `-j24` cases is.
It claims CPU does twice as much work. But it should not as I throw the
same amount of work at CPUs in all runs.
It's one of confusing parts about `hyperthreading`: operating system lies
to you how much actual work was actually done.

## `perf`, take 3

Trying to slice and dice metrics reported by `perf` I tried to get an
idea where the overhead appears in multi-threaded case.
I've noticed `GHC` works way faster when compiled with
`-fno-worker-wrapper -fno-spec-constr` flags.
It instantly sped `GHC` up by 10% on synth benchmark and made `GHC`
bootstrap [5%
faster](https://perf.haskell.org/ghc/#revision/a48de37dcca98e7d477040b0ed298bcd1b3ab303).
`GHC` managed to inflate tiny snippet:

``` haskell
cmmExprNative :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
cmmExprNative referenceKind expr = do
     dflags <- getDynFlags
     let platform = targetPlatform dflags
         arch = platformArch platform
     case expr of
        CmmLit (CmmLabel lbl)
           -> do
                cmmMakeDynamicReference dflags referenceKind lbl
```

into the following code:

``` 
_______
       │      cmmExprNative :: ReferenceKind -> CmmExpr -> CmmOptM CmmExpr
       │      cmmExprNative referenceKind expr = do
  0,11 │        cmp    $0x3,%rax
       │      ↑ jb     3ceb930 <cFO7_info+0x8b0>
       │                 -- we must convert block Ids to CLabels here, because we
       │                 -- might have to do the PIC transformation.  Hence we must
       │                 -- not modify BlockIds beyond this point.
       │
       │              CmmLit (CmmLabel lbl)
       │                 -> do
  2,02 │        add    $0x890,%r12
       │        cmp    0x358(%r13),%r12
       │      ↑ ja     3cf456f <cFIc_info+0x7df>
  0,16 │        mov    0x7(%rbx),%rax
  0,59 │        lea    ghc_DynFlags_DynFlags_con_info,%rbx
  0,05 │        mov    %rbx,-0x888(%r12)
  3,41 │18e9:   mov    0x50(%rsp),%rbx
  0,05 │        mov    %rbx,-0x880(%r12)
  0,32 │        mov    0x58(%rsp),%r14
       │        mov    %r14,-0x878(%r12)
       │        mov    0x60(%rsp),%rbx
       │        mov    %rbx,-0x870(%r12)
  0,05 │        mov    0x68(%rsp),%r14
       │        mov    %r14,-0x868(%r12)
       │        mov    0x70(%rsp),%rbx
       │        mov    %rbx,-0x860(%r12)
       │        mov    0x78(%rsp),%r14
  0,11 │        mov    %r14,-0x858(%r12)
  0,05 │        mov    0x80(%rsp),%rbx
       │        mov    %rbx,-0x850(%r12)
  0,05 │        mov    0x88(%rsp),%r14
       │        mov    %r14,-0x848(%r12)
       │        mov    0x90(%rsp),%rbx
       │        mov    %rbx,-0x840(%r12)
  0,05 │        mov    0x98(%rsp),%r14
  0,05 │        mov    %r14,-0x838(%r12)
  0,11 │        mov    0xa0(%rsp),%rbx
       │        mov    %rbx,-0x830(%r12)
       │        mov    0xa8(%rsp),%r14
       │        mov    %r14,-0x828(%r12)
  0,05 │        mov    0xb0(%rsp),%rbx
       │        mov    %rbx,-0x820(%r12)
       │        mov    0xb8(%rsp),%r14
... <a few more pages of it>
```

That appeared to be a bug of specialiser being too eager to move out as
many strict fields from structs to separate function arguments (aka
worker arguments) even if it means pulling out 180 arguments.
Optimization should be controlled by `-fmax-worker-args=<N>` flag
but it got lost when demand analyzer was rewritten a few years ago.
I've noticed it by accident when passing through `DynFlags` structure
in `GHC`. Many of flags were unused (including `-fmax-worker-args=<N>`).

That gave rise to [`ghc` bug
#11565](https://ghc.haskell.org/trac/ghc/ticket/11565).

# Final result

On my 8-CPU desktop final improvement is `3.72x` (22 seconds vs. 82
seconds):

``` 
$ ./synth.bash -j1 +RTS -sstderr

 100,430,153,096 bytes allocated in the heap
   3,970,134,600 bytes copied during GC
     145,432,032 bytes maximum residency (16 sample(s))
       1,639,792 bytes maximum slop
             643 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       363 colls,     0 par    7.065s   7.060s     0.0194s    0.0496s
  Gen  1        16 colls,     0 par    1.485s   1.484s     0.0928s    0.1705s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.002s  (  0.002s elapsed)
  MUT     time   64.242s  ( 73.611s elapsed)
  GC      time    8.550s  (  8.544s elapsed)
  EXIT    time    0.012s  (  0.015s elapsed)
  Total   time   72.821s  ( 82.172s elapsed)

  Alloc rate    1,563,303,909 bytes per MUT second

  Productivity  88.3% of total user, 89.6% of total elapsed

gc_alloc_block_sync: 0
whitehole_spin: 0
gen[0].sync: 0
gen[1].sync: 0

real    1m22.213s
user    1m21.061s
sys     0m1.139s
```

``` 
$ ./synth.bash +RTS -sstderr
...
 100,608,799,232 bytes allocated in the heap
   3,959,638,248 bytes copied during GC
     173,982,704 bytes maximum residency (8 sample(s))
       2,947,048 bytes maximum slop
            2556 MB total memory in use (1 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0        53 colls,    53 par   15.480s   2.055s     0.0388s    0.0767s
  Gen  1         8 colls,     7 par    3.943s   0.558s     0.0697s    0.0897s

  Parallel GC work balance: 75.64% (serial 0%, perfect 100%)

  TASKS: 19 (1 bound, 18 peak workers (18 total), using -N8)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.002s  (  0.002s elapsed)
  MUT     time  131.375s  ( 19.329s elapsed)
  GC      time   19.423s  (  2.613s elapsed)
  EXIT    time    0.016s  (  0.018s elapsed)
  Total   time  150.830s  ( 21.962s elapsed)

  Alloc rate    765,815,750 bytes per MUT second

  Productivity  87.1% of total user, 88.1% of total elapsed

gc_alloc_block_sync: 370037
whitehole_spin: 0
gen[0].sync: 564078
gen[1].sync: 520393

real    0m22.068s
user    2m45.042s
sys     0m4.318s
```

On 24-CPU VM speedup is around `8x` (9 seconds versus 75 seconds)
This suggests `GC` takes only 12% of elapsed time in both cases. I'm not
sure if `200M` allocation difference could be attributed to throw-away
work done by `haskell` threads.
CPU `MUT` and `GC` time got `2x` increase. It's not the throw-away work
but a hyperthreading artifact.

# Conclusions

I have not solved the problem yet. And don't fully understand it
either! But I feel we are on the right track :)
Some achievements:

- `ghc --make` is slightly faster now!
- Now I (and you!) understand `haskell` runtime performance a bit better
- we've fixed a few seemingly unrelated bugs :)
- `eventlog` works in programs calling `setNumCapabilities`
- `threadscope` can read `GHC` traces
- large `-A` size auto-tunes parallel `GC` parameters
- `ghc --make` does not create more Capabilities than CPUs available
- we've learned `hyperthreading` is tricky :)

How `you` can improve `GHC`:

- `GHC` is full of low-hanging performance improvements and they are
  easy to fix :)
- `haskell` profiling tools need an improvement to see where
  `BLACKHOLES` come from. (I suspect most of them come from
  [`mkSplitUniqSupply`
  function](https://github.com/ghc/ghc/blob/9306db051ff5835b453d55f32783d081ac79ec28/compiler/basicTypes/UniqSupply.hs#L77))
- `perf` is still too hard to use on `GHC`-built programs

Have fun!
