---
title: "GNU make's 'Secondary Expansion' feature"
date: September 16, 2022
---

People occasionally ask me when
[`make --shuffle`](/posts/238-new-make-shuffle-mode.html) feature will
be released. The short answer is: I don't know. I would like to have it
released sooner but I also understand that stabilization requires real
work to get the release out.

But fear not, not all is lost! Paul sent out
[an announcement](https://lists.gnu.org/archive/html/bug-make/2022-09/msg00003.html)
a few days ago that the new release is in the works!

To get some confidence that `make --shuffle` is not completely broken I
gave current `master` a go.

The setup was straightforward as usual: I added current `make` snapshot
locally into `nixpkgs` and tried to build my desktop system with it
using `nix build -f. nixos system`.

Quiz question: did `make --shuffle` still work? :)

Let's find out.

## `autoconf`

A few seconds into the build the first failed package was
`autoconf-2.71` (and `autoconf-2.69` slightly later). The symptom
was the following suspicious build failure:

```
$ make
...
bash: line 1: env: command not found
make  all-am
bash: line 1: make: command not found
make: *** [Makefile:928: all] Error 127
```

`make: command not found` suggested something broke the `PATH`
environment variable. Grepping `automake`s source for `PATH` I was lucky
to find this code snippet:

```Makefile
# In cfg.mk:
export PATH = $(shell echo "`pwd`/tests:$$PATH")
```

Until then I had no idea about `export` keyword in `GNU make`! This keyword
exports `make`-level variables to child sub-processes. Mostly
intended for sub-`make`. But `$(shell ...)` calls work as well.

If you are not very familiar with `GNU make` the code above might not look
tricky. Let's talk a bit about various variable assignments.
`GNU make` has that many:

```
     IMMEDIATE = DEFERRED
     IMMEDIATE ?= DEFERRED
     IMMEDIATE := IMMEDIATE
     IMMEDIATE ::= IMMEDIATE
     IMMEDIATE :::= IMMEDIATE-WITH-ESCAPE
     IMMEDIATE += DEFERRED or IMMEDIATE
     IMMEDIATE != IMMEDIATE
```

Yeah, it's 7 types of them. We use deferred one in the example
above. From what I understand `POSIX` defines only the first
`IMMEDIATE = DEFERRED`.

I'll quote `info make` here for details:

```
3.7 How 'make' Reads a Makefile
===============================

GNU 'make' does its work in two distinct phases.  During the first phase
it reads all the makefiles, included makefiles, etc.  and internalizes
all the variables and their values and implicit and explicit rules, and
builds a dependency graph of all the targets and their prerequisites.
During the second phase, 'make' uses this internalized data to determine
which targets need to be updated and run the recipes necessary to update
them.

   It's important to understand this two-phase approach because it has a
direct impact on how variable and function expansion happens; this is
often a source of some confusion when writing makefiles.  Below is a
summary of the different constructs that can be found in a makefile, and
the phase in which expansion happens for each part of the construct.

   We say that expansion is "immediate" if it happens during the first
phase: 'make' will expand that part of the construct as the makefile is
parsed.  We say that expansion is "deferred" if it is not immediate.
Expansion of a deferred construct part is delayed until the expansion is
used: either when it is referenced in an immediate context, or when it
is needed during the second phase.
```

The main ones I'll focus on here are `:=` (immediate) vs `=` (deferred).

Here is one possible example that illustrates the difference:

```Makefile
AI := $(BI)
AD  = $(BD)

all:
	@echo "AI=$(AI) BI=$(BI)"
	@echo "AD=$(AD) BD=$(BD)"

BI := BI-val
BD  = BD-val
```

Running:

```
$ make
AI= BI=BI-val
AD=BD-val BD=BD-val
```

Here `AI` refers to `BI` value before `BI` is defined. While `AD` refers
to `BD` value after `BD` is defined. `:=` is very useful for cases when
right hand side is computationally non-trivial and thus should not be
duplicated.

Typical deferred assignment error is an accidental recursion:

```Makefile
C = $(C) also-bar

all:
	@echo "C=$(C)"
```

Running:

```
$ LANG=C make
Makefile:1: *** Recursive variable 'C' references itself (eventually).  Stop.
```

Going back to our initial example of `export PATH = $(shell echo $$(pwd)/tests:$$PATH")`
here `export` attempts to export a `PATH` make variable as environment
for ran subprocesses like `$(shell ...)` call in this example. It also
tries to base the new `PATH` value on existing shell-defined `PATH`
value. So what happens first? Variable export before `shell` call?
Or variable definition after `shell` call? If `:=` were to be used
instead then it would be more straightforward: export would probably
happen after.

The answer is ... `make` did change the actual behavior recently. To
quote the [`NEWS` file](https://git.savannah.gnu.org/cgit/make.git/commit/NEWS?id=98da874c43035a490cdca81331724f233a3d0c9a):

```
* WARNING: Backward-incompatibility!
  Previously makefile variables marked as export were not exported to commands
  started by the $(shell ...) function.  Now, all exported variables are
  exported to $(shell ...).
  To detect this change search for 'shell-export' in the .FEATURES variable.
```

To avoid this ambiguity `autoconf` was trivially fixed with
[the following patch](https://git.savannah.gnu.org/cgit/autoconf.git/commit/?id=31f673434ee402258b45e958c88acc8725d82b1a)
upstream:

```diff
--- a/cfg.mk
+++ b/cfg.mk
@@ -20,2 +20,3 @@
 # Build with our own versions of these tools, when possible.
-export PATH = $(shell echo "`pwd`/tests:$$PATH")
+export PATH := $(or $(PWD),$(shell pwd))/tests:$(PATH)
```

Meanwhile, `GNU make` also
[added graceful fallback](https://git.savannah.gnu.org/cgit/make.git/commit/?id=70ba0357a080f72b9f5912f16b3ffc095db381e6)
to this case as exporting empty variable is probably not very useful.
Thus, existing `autoconf` releases should still compile successfully
with `GNU make` from `master`.

Phew. This failure was not related to `--shuffle`.

## `glibc`

Once `autoconf` was fixed I resumed world rebuild. The next failure was
in `glibc`:

```
    $ make --shuffle
    ...
    make  -C localedata install-locales
    make: invalid shuffle mode: '1662724426r'
```

`GNU make` complains at unexpected `r` trailing letter in
`--shuffle=1662724426r` parameter. That suffix comes from ... `glibc`
own `Makefile`:

```
# In glibc/Makerules:
# Don't define any builtin rules.
MAKEFLAGS := $(MAKEFLAGS)r
```

Normally `MAKEFLAGS` contains options passed to `make`:

```
$ printf 'all:; @echo MAKEFLAGS=$(MAKEFLAGS)' | make -f -
MAKEFLAGS=
$ printf 'all:; @echo MAKEFLAGS=$(MAKEFLAGS)' | make -f - -s
MAKEFLAGS=s
$ printf 'all:; @echo MAKEFLAGS=$(MAKEFLAGS)' | make -f - -s -r
MAKEFLAGS=rs
$ printf 'all:; @echo MAKEFLAGS=$(MAKEFLAGS)' | make -f - -s -r --shuffle
MAKEFLAGS=rs --shuffle=1663776045
```

Note that `MAKEFLAGS` value does not contain dashes in option names.

`GNU make` also allows extending `MAKEFLAGS` from within `Makefile`:

```Makefile
MAKEFLAGS := $(MAKEFLAGS) --no-builtin-rules
all:
	@echo MAKEFLAGS=$(MAKEFLAGS)
```

Running:

```
$ make
MAKEFLAGS=r
$ make --no-builtin-variables
MAKEFLAGS=rR
$ make --no-builtin-variables --no-print-directory
MAKEFLAGS=rR --no-print-directory
```

`--no-builtin-variables` and `-R` are equivalent. `GNU make` picks short
form of an option if available.

Note how short single-letter options get globbed together in the first
word while long options (without short option equivalent) are passed
separately. `NEWS` file tells us it's another recent behavior change:

```
* WARNING: Backward-incompatibility!
  Previously only simple (one-letter) options were added to the MAKEFLAGS
  variable that was visible while parsing makefiles.  Now, all options
  are available in MAKEFLAGS.
```

[The fix](https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=2d7ed98add14f75041499ac189696c9bd3d757fe)
exceeded 1 liner bucket as `glibc` also introspected `MAKEFLAGS` in
other places for `-s` (`--silent`) option presence. Relevant part of
the fix:

```diff
--- a/Makerules
+++ b/Makerules
@@ -796,2 +796,2 @@ endif
 # Don't define any builtin rules.
-MAKEFLAGS := $(MAKEFLAGS)r
+MAKEFLAGS := $(MAKEFLAGS) -r
```

No complications here.

Again, not exactly `--shuffle`-specific bug. Using of any long option
would break `glibc` build.

## `ghc`

At this point `autoconf` and `glibc` fixes above unblocked many other
package builds. Most other projects' `Makefile` are not that
complicated and don't rely on `GNU make` extensions. They usually limit
themselves to `POSIX make` features. `glibc` is a notable exception.
That makes it a good project to test new `GNU make` versions.

Believe it or not there is even heavier user of `GNU make`! It is
`ghc` (the `Glasgow Haskell Compiler`)! The small caveat is that it
migrates off `GNU make` to own `haskell`-based `hadrian` build system.
Many distributions still package previous versions of `ghc` and use
`GNU make` to build it.

In case of `ghc`, `GNU make` managed to `SIGSEGV` itself:

```
$ make --shuffle
...
Configuring ghc-bin-9.0.2...
Warning: 'data-dir: ..' is a relative path outside of the source tree. This
will not work when generating a tarball with 'sdist'.
"rm" -f utils/deriveConstants/dist/build/.depend.haskell.tmp
"rm" -f utils/deriveConstants/dist/build/.depend.c_asm.tmp
"rm" -f utils/genprimopcode/dist/build/.depend.c_asm.tmp
make: *** [Makefile:124: all] Segmentation fault (core dumped) shuffle=1663318833
make: INTERNAL: Exiting with 14 jobserver tokens available; should be 16!
```

It might not be obvious, but `Segmentation fault` happens within
`GNU make` itself, not just some tool it runs. `jobserver` token loss
is another sign of things went wrong with `make` process itself.

This time crash happened only if I used `make --shuffle` option.

I was not able to craft a simple crashing example. I spent some time in
`gdb` to understand the failures mode. I found that it has something to
do with another `GNU make` extension: `Secondary Expansion`. I'll
quote another bit of `GNU make` manual entry:

```
3.9 Secondary Expansion
=======================

Previously we learned that GNU 'make' works in two distinct phases: a
read-in phase and a target-update phase (*note How 'make' Reads a
Makefile: Reading Makefiles.).  GNU make also has the ability to enable
a _second expansion_ of the prerequisites (only) for some or all targets
defined in the makefile.  In order for this second expansion to occur,
the special target '.SECONDEXPANSION' must be defined before the first
prerequisite list that makes use of this feature.
```

I'll start from non-working example to describe an intent for typical
use of `Secondary Expansion`. Suppose you want to use the list of
prerequisites dynamically read from external command (`ghc` for example
parses `.cabal` build files to extract prerequisites). I will emulate
external command with a simple `$(shell echo i1 i2 i3)` call.

Naive non-working approach to achieve would look like that:

```Makefile
all: $(ALL_INPUTS)
	@echo Inputs: $^

i%:
	: # no-op

# Has to go after rule defined above. In ghc it's based on external
# command that dynamically builds a list of prerequisites:
ALL_INPUTS = $(shell echo i1 i2 i3)
```

Running:

```
$ make
Inputs:
```

The example above attempted to get `i1 i2 i3` as a dynamic input and
failed. `$(ALL_INPUTS)` is expanded to an empty string. `GNU make`
allows this style of dependencies when `.SECONDEXPANSION:` phony
target is present in the `Makefile`:

Here is a working example closer to what `ghc` uses:

```Makefile
.SECONDEXPANSION:

all: $$(ALL_INPUTS)
	@echo Inputs: $^

i%:
	: # $@ no-op

# Has to go after rule defined above. In ghc it's based on external
# command that dynamically builds a list of prerequisites:
ALL_INPUTS := $(shell echo i1 i2 i3)
```

Running:

```
$ make
: # i1 no-op
: # i2 no-op
: # i3 no-op
Inputs: i1 i2 i3
```

Now we get our dynamic input as expected.

The magic happens around `$$(ALL_INPUTS)` expression: first it is
expanded to `$(ALL_INPUTS)` and on second expansion it uses already
available result. There are other simpler ways to get the same effect
(like moving variable assignment earlier). But that's what `ghc` decided
to use.

While this simple example did not crash `GNU make` it did show me a
symptom of the problem. `make --shuffle` had no effect on prerequisite
traversal order:

```
$ make --shuffle
: # i1 no-op
: # i2 no-op
: # i3 no-op
Inputs: i1 i2 i3

$ make --shuffle
: # i1 no-op
: # i2 no-op
: # i3 no-op
Inputs: i1 i2 i3

$ make --shuffle
: # i1 no-op
: # i2 no-op
: # i3 no-op
Inputs: i1 i2 i3
```

Prerequisites were never reordered. But they were supposed to! Yet again
`NEWS` entry hinted at why it started happening only recently:
```
* GNU make was performing secondary expansion of all targets, even targets
  which didn't need to be considered during the build.  In this release
  only targets which are considered will be secondarily expanded.
```

In other words before the change order of events was:

1. read
2. expand
3. _second expand (of everything)_
4. _shuffle_
5. execute

After the change:

1. read
2. expand
3. **shuffle**
4. **second expand (of built targets only)**
5. execute

The bug mechanics: `shuffle` step assumed no changes in prerequisite
lists would happen after. Moving `second expand` step behind it broke
that assumptions: it canceled shuffling effect (minor problem)
and introduced dangling references to freed memory (major problem).

Once understood the fix was trivial: refresh shuffle data if prerequisite
list was changed. The patch is
[a few-liner](https://git.savannah.gnu.org/cgit/make.git/commit/?id=ca4234c4b550618df2194e0617c43bb12524f820):

```diff
--- a/src/file.c
+++ b/src/file.c
@@ -576,6 +577,7 @@ expand_deps (struct file *f)
   struct dep **dp;
   const char *fstem;
   int initialized = 0;
+  int changed_dep = 0;
 
   if (f->snapped)
     return;
@@ -664,6 +666,7 @@ expand_deps (struct file *f)
       if (new == 0)
         {
           *dp = d->next;
+          changed_dep = 1;
           free_dep (d);
           d = *dp;
           continue;
@@ -672,6 +675,7 @@ expand_deps (struct file *f)
       /* Add newly parsed prerequisites.  */
       fstem = d->stem;
       next = d->next;
+      changed_dep = 1;
       free_dep (d);
       *dp = new;
       for (dp = &new, d = new; d != 0; dp = &d->next, d = d->next)
@@ -688,6 +692,12 @@ expand_deps (struct file *f)
       *dp = next;
       d = *dp;
     }
+
+    /* Shuffle mode assumes '->next' and '->shuf' links both traverse the same
+       dependencies (in different sequences).  Regenerate '->shuf' so we don't
+       refer to stale data.  */
+    if (changed_dep)
+      shuffle_deps_recursive (f->deps);
 }
 
 /* Add extra prereqs to the file in question.  */
```

We track all the places where prerequisite list is modified and then
rebuild shuffle list if any changes happened to the list.

The fix restored shuffling property and fixed `SIGSEGV` when building
`ghc`:

```
$ make --shuffle
: # i2 no-op
: # i3 no-op
: # i1 no-op
Inputs: i1 i2 i3

$ make --shuffle
: # i1 no-op
: # i3 no-op
: # i2 no-op
Inputs: i1 i2 i3
```

This one was clearly `--shuffle`-related bug.

## Parting words

Trying out a pre-release was totally worth it. I found out about
existence of `export` and `Secondary Expansion` extensions.

The test
uncovered two bugs in upstream projects. Upstreams were very quick to
accept fixes. The bugs happened in somewhat obscure parts of `GNU make`
specific extensions: environment variable exports, `MAKEFLAGS` variable
update (and introspection).

The test also exposed a bug in `make --shuffle` implementation for an
advanced `Secondary Expansion` feature which was also an easy one to
fix.

After the fixes above I did not find any other related breakages.

Have fun!
