---
title: "New make --shuffle mode"
date: February 23, 2022
---

# TL;DR:

I implemented new **\-\-shuffle** option for **GNU make** to simulate
non-deterministic build order in parallel makefiles.

# Background

About 11 years ago I was a year old Gentoo dev who just started getting
downstream bug reports on mysterious **ghc** build failures like
<https://bugs.gentoo.org/326347>.

The symptoms were seemingly simple: some file was inacessible while
it was being written to, or executed.

Years later I mastered the intricacies of **ghc**'s build system on how
to debug it effectively. But at that time I did not really know what to
do. My main working machine was a Core 2 duo HP laptop which could not do
more than **-j2**. And even that required a bit of swap for **ghc**'s
linking stage. Throwing more parallelism was not really an option to
trigger such bugs.

# Makefile target ordering

**Makefiles** are fundamentally simple: it's a graph of dependencies
with a sequence of shell commands attached to a node. There are numerous
caveats, but they should not break this model too much.

In theory you can topologically sort the graph and execute the dependencies
in various conformant orders and expect the same result. Modulo missing
dependencies in the graph.

In practice **GNU make** happens to traverse the graph in very specific
topological order: it maintaing syntactic ordering as much as possible.

Here is an example **Makefile**:

```Makefile
# cat Makefile
all: a b c

b: b1 b2 b3
a: a1 a2 a3
c: c1 c2 c3

a b c a1 a2 a3 b1 b2 b3 c1 c2 c3:; @echo $@ && sleep 1
```

(I added **sleep 1** to make it more visible when next goal schedules).

Here is the sequential execution by **GNU make**:

```
$ make
a1
a2
a3
a
b1
b2
b3
b
c1
c2
c3
c
```

The seen order is exactly **all**'s prerequisites left-to-right
recursively.

Adding parallelism does not change the order too much: **make** still
traverses prerequisited in the same order and starts as many targets
with satisfied dependencies as possible.

Parallel example:

```
$ make -j4
a1
a2
a3
b1

b2
b3
c1
c2

c3
a
b

c
```

I added newlines where 1-second pauses visibly happen.

Note that in this **Makefile** **a1** does not depend on
**c2**. But **c2** practically always starts execution after **a1**
finishes.

The "only" way to run **a1** and **c2** them in parallel
is to run **make** with at least **-j8**. Which is a lot.

Or do something with the system that stalls task execution for
indefinite amount of time (like, adding various nice levels
or put system under high memory or CPU pressure).

Very occasionally already stressed system naturally gets into
unusual task execution order. You get the failure and struggle
to repeat it again. Which makes it very hard to test the fix
unless you know where exactly to put the **sleep** command
to make it more reproducible.

# An old idea

Even then it was clear that CPU count per device will only increase
and it will be increasingly painful to work with sequentially built
projects :) Bugs will come back again and again on you the more cores
you throw at the **Makefiles**.

I had a silly idea back then ([in russian](/posts/143-make-idea.html)):
what if we arbitrarily reorder the prerequisites in **Makefile**s? Or
maybe even trace spawned processes to know for sure what files targets
access? That might allow us to weed out most of the parallel bugs with
some sort of stress test on a low-core machine.

Fast forward 11 years I attempted to enable build parallelism by default
in [nixpkgs](https://discourse.nixos.org/t/rfc-make-stdenv-to-build-in-parallel-by-default/15684/8).
A few packages still had [some issues](/posts/230-when-make-j-nproc-fails.html).

I recalled the idea and tried to implement target random shuffle within
**GNU make**!

# Better reproducer: make \-\-shuffle

Initial idea was very simple: pick target order at **Makefile**
parse time and reshuffle the lists randomly. To pick an example
above one of the example shuffles would be:

```Makefile
$ cat Makefile
all: c b a

b: b2 b1 b3
a: a3 a2 a1
c: c1 c2 c3

a b c a1 a2 a3 b1 b2 b3 c1 c2 c3:; @echo $@ && sleep 1
```

I wrote a patch and proposed it to GNU make community:
<https://lists.gnu.org/archive/html/bug-make/2022-02/msg00005.html>.

The example run of patched **make** shows less determinism now:

```
$ ~/dev/git/make/make --shuffle -j4
c2
c3
c1
a2

a1
b2
a3
b3

b1

c
b
a
```

Paul did not seem to object too much to the idea and pointed out
that implementation will break more complex **Makefiles** as there
is a simple way to refer to individual prerequisites by number.

To pick Pauls' example:

```Makefile
%.o : %.c
	$(CC) $(CFLAGS) -c -o $@ $<

foo.o: foo.c foo.h bar.h baz.h

#

foo%: arg%1 arg%2 arg%3 arg%4
	bld $< $(word 3,$^) $(word 2,$^) $(word 4,$^)
```

In both cases syntactic reshuffling breaks the build rules
by passing wrong file name.

To fix it I came up with a way to store two orders at the same time:
syntactic and shuffled and posted patch as
<https://lists.gnu.org/archive/html/bug-make/2022-02/msg00042.html>.

# Running 'make \-\-shuffle' on real projects

While I was waiting for the feedback I ran the build tests against
**nixpkgs** packages.

First, I almost instantly got build failures on the projects that already
explicitly disable parallel builds to avoid known failures: **groff**,
**source-highlight**, **portaudio**, **slang**, **gnu-efi**, **bind**,
**pth**, **libomxil**, **dhcp**, **directfb**, **doxygen**, **gpm**,
**judy** and a few others. That was a good sign.

A bit later I started getting failures I did not encounter before in
**ghc**(!), **gcc**(!!), **automake**(!!!), **pulseaudio**,
**libcanberra**, many **ocaml** and some **perl** packages.

All the failures looked genuine missing dependencies. For example
**gcc**'s **libgfortran** is missing a **libquadmath** build dependency.
It is natural not to encounter it in real world as **libquadmath** is
usually built along with other small runtimes way before **g++** or
**gfortran** is ready.

Fun fact: while running the build I stumbled on a **GNU make** bug
not related to my change: <https://lists.gnu.org/archive/html/bug-make/2022-02/msg00037.html>.
The following snippet make **GNU make** to loop for a while until
it crashes with argument list exhaustion (or inode exhaustion in
**/tmp**):

```
$ printf 'all:\n\techo $(CC)' | ./make -sf -
<hung>
```

This bug is not present in any releases yet. And hopefully will not be.

I'd like to land the **\-\-shuffle** change upstream in some form before
sending bug reports and trivial fixes to upstreams.

# How to try

If you are keen to try this shuffling mode on your **make**-based
projects (manually written, **automake**-based, **cmake**-based are all
fine) here is a rough instruction to do it:

- grab current git tree of **GNU make** at <https://git.savannah.gnu.org/git/make.git>
- apply [v4-0001-Add-shuffle-argument-support.patch](https://savannah.gnu.org/bugs/download.php?file_id=52908) from <https://savannah.gnu.org/bugs/index.php?62100>
- build **make** with **./bothstrap && configure && make**
- use it as **path/to/make \-\-shuffle \<your-typical-make-arguments\>** against your project
- check if the build succeeds

Both sequential and parallel modes should work fine. I suggest trying
both. The shuffling overhead should be negligible.

# How do failures look like

When build fails it reports the shuffling mode and seed used. Let's try it
on a concrete **cramfsswap** example:

```
$ git clone https://github.com/julijane/cramfsswap.git
$ cd cramfsswap

$ ~/dev/git/make/make clean && ~/dev/git/make/make
rm -f cramfsswap
gcc -Wall -g -O -o cramfsswap -lz cramfsswap.c
strip cramfsswap

$ ~/dev/git/make/make clean && ~/dev/git/make/make
rm -f cramfsswap
strip cramfsswap
/nix/store/v56r6fqg2z5yc5nyjn737hiakc2xm0gh-binutils-2.38/bin/strip: 'cramfsswap': No such file
make: *** [Makefile:10: strip] Error 1 --shuffle=1645603370
```

Here we see a successful run and a failed run. Failed run reports
specific seed that might trigger the failure: **\-\-shuffle=1645603370**.

We can use this seed explicitly:

```
$ ~/dev/git/make/make --shuffle=1645603370
strip cramfsswap
strip: 'cramfsswap': No such file
make: *** [Makefile:10: strip] Error 1 --shuffle=1645603370
$ ~/dev/git/make/make --shuffle=1645603370
strip cramfsswap
strip: 'cramfsswap': No such file
make: *** [Makefile:10: strip] Error 1 --shuffle=1645603370
$ ~/dev/git/make/make --shuffle=1645603370
strip cramfsswap
strip: 'cramfsswap': No such file
make: *** [Makefile:10: strip] Error 1 --shuffle=1645603370
```

Note how ordering is preserved across the runs with fixed seed.

# Parting words

Implementing the shuffling idea took a weekend. I should have tried
it earlier :) The result instantly found existing and new missing
dependencies in a small subset of real projects. Some of these failures
are very hard to trigger otherwise.

It would be nice to get **\-\-shuffle** upstreamed to ease reporting
ordering problems to other projects without too much detail on how to get
unusual build orders. Otherwise this post would have to do.

Have fun!
