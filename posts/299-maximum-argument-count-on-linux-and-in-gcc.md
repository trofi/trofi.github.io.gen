---
title: "Maximum argument count on linux and in gcc"
date: September 21, 2023
---

## Tl;DR

By default on `linux` argument list (and environment) is limited by
less than `2MB` all bytes calculated across all arguments and environment
(including `argv/envp` array overheads and null terminators).

`ulimit -s` can increase this limit to `6MB`. Individual command line
and environment `K=V` pairs are limited to `128KB`.

And due to internal implementation deficiencies of `gcc` argument list
for `gcc` happens to be limited by the same `128KB` limit.

## Story mode

My [previous attempt](/posts/298-unexpected-runtime-dependencies-in-nixpkgs.html)
to "just add" a bunch of `-fmacro-prefix-map=` flags to each `nixpkgs`
package worked for most packages. But it started failing for `qemu` as:

```
Command line: `gcc -m64 -mcx16 /build/qemu-8.1.0/build/meson-private/tmpbyikv8nc/testfile.c \
  -o /build/qemu-8.1.0/build/meson-private/tmpbyikv8nc/output.exe -D_FILE_OFFSET_BITS=64 \
  -O0 -Wl,--start-group -laio -Wl,--end-group -Wl,--allow-shlib-undefined` -> 1
stderr:
gcc: fatal error: cannot execute 'cc1': execv: Argument list too long
compilation terminated.
```

The failure happens because we have exhausted some command line argument
limit. In case of `qemu` we pass around a few thousands of
`-fmacro-prefix-map=` options (`qemu` is a big package and has many
build inputs).

Ideally I would like to be able to pass a lot more options without
hitting the arguments limit (10x? 100x?). Luckily `gcc` and other tools
like `ld` do support a way to pass many options indirectly via response
files:

```
$ gcc -g -O2 -c a.c
```

The above should be equivalent to the below:

```
$ echo "-g"   > a.rsp
$ echo "-O2" >> a.rsp
$ gcc @a.rsp -c a.c
```

But I'll leave response files to another post as it ended up being
it's own rabbit hole.

Instead let's explore how many arguments you can pass to a single
command in `linux`.

## Exploring the argument count limits

So what are the actual limits we are hitting against here? How many
arguments can we pass to `gcc` without any problems?

Let's explore it! I'll start by adding more and more `-g` options to
`gcc` call until it starts failing for a command line limit:

```
$ nix shell nixpkgs#gcc
$$ set -x; touch a.c; args=(-g); while gcc ${args[@]} -c a.c; do args+=("${args[@]}"); done; echo ${#args[@]}

+ set -x
+ touch a.c
+ args=(-g)
+ gcc -g -c a.c
+ args+=("${args[@]}")
+ gcc -g -g -c a.c
+ args+=("${args[@]}")
+ gcc -g -g -g -g -c a.c
+ args+=("${args[@]}")
+ gcc -g -g -g -g -g -g -g -g -c a.c
...
gcc: fatal error: cannot execute 'cc1': execv: Argument list too long
compilation terminated.
+ echo 32768
32768
```

Our limit is somewhere below 32K (this is a lot lower than I expected).

In the above snippet we double the length of argument list to speed the
search up a bit, thus it's not an exact value and some closest
power-of-2 ceiling.

Let's extend this snippet a bit and build more flexible argument count
probe that returns us precise value. I called it `probe-argsize.bash`:

```bash
#!/usr/bin/env bash

# $1 - payload
# $2... - prober command to test against argument list

# create an aray of enough elements to start failing the allocation:
large=("$1"); shift

while "$@" "${large[@]}" >/dev/null 2>/dev/null; do
    # double the array lenght until execution start failing
    large+=("${large[@]}")
done

# Use binary search to find largest successfully running prober
l=0
u=${#large[@]}

while (( l < u )); do
    m=$(( (l + u + 1) / 2 ))
    if "$@" "${large[@]:0:m}" >/dev/null 2>/dev/null; then
        # can survive
        (( l = m ))
    else
        (( u = m - 1 ))
    fi
done

echo $l
```

The first half of the script does the same 2x argument list growth on
every step as before. And the second half does binary search for an
exact value.

Let's try it out:

```
$$ touch a.c; ./probe-argsize.bash -g gcc -c a.c

26118
```

~26K parameters. Seems to work!

What if we make our argument a bit larger? Say, pass `-ggdb3` instead of
`-g`?

```
$ touch a.c; ./probe-argsize.bash -ggdb3 gcc -c a.c
14510
```

Just ~14K. That degrades very quickly. The available length is decreased
by half! (or something like that)

How about longer option? I'll try ~100 bytes long one:

```
$ touch a.c; ./probe-argsize.bash -I0123456789-90123456789-0123456789-0123456789-0123456789-0123456789-01234567889-0123456789-0123456789 \
  gcc -c a.c
1209
```

1209 is extremely low. That is on par with what `qemu` exercises in
`nixpkgs`. Looks like our limit here is about `~120K` bytes if we sum up
all our argument lengths to `gcc`.

What if the problem is in some internal `gcc` limit and not the OS
itself? Let's `strace` `gcc` call just to make sure:

```
$$ strace -f gcc ${args[@]} -c a.c
...
[pid 1360260] execve("cc1", ["cc1", "-quiet", "-idirafter", ..., "-g", ...], \
    0x1474b80 /* 103 vars */) = -1 E2BIG (Argument list too long)
```

Here we see that `E2BIG` comes right from an `execve()` system call.
Thus it's kernel's limitation of some sort.

## Getting the formula

Can we easily increase the limit? Let's find out how `linux` implements
limits in [fs/exec.c](https://github.com/torvalds/linux/blob/2cf0f715623872823a72e451243bbf555d10d032/fs/exec.c#L1888C1-L1894C13).
Maybe there is a `linux`-specific hack somewhere we could pull out.

There are a few places where `-E2BIG` is returned. This code looks most
relevant:

```c
static int do_execveat_common(int fd, struct filename *filename,
			      struct user_arg_ptr argv,
			      struct user_arg_ptr envp,
			      int flags)
{
	struct linux_binprm *bprm;
	int retval;
	// ...

	retval = count(argv, MAX_ARG_STRINGS);
	if (retval < 0)
		goto out_free;
	bprm->argc = retval;

	retval = count(envp, MAX_ARG_STRINGS);
	if (retval < 0)
		goto out_free;
	bprm->envc = retval;

	// ...

	retval = bprm_stack_limits(bprm);
	if (retval < 0)
		goto out_free;

	// ...

	retval = copy_strings(bprm->envc, envp, bprm);
	if (retval < 0)
		goto out_free;

	retval = copy_strings(bprm->argc, argv, bprm);
	if (retval < 0)
		goto out_free;

	// ...
}
```

I wondered if `MAX_ARG_STRINGS` could be one of our limits once
we solve the smaller limit we are bumping into now, but nope it's defined
in [include/uapi/linux/binfmts.h](https://github.com/torvalds/linux/blob/2cf0f715623872823a72e451243bbf555d10d032/include/uapi/linux/binfmts.h#L9)
as:

```c
/*
 * These are the maximum length and maximum number of strings passed to the
 * execve() system call.  MAX_ARG_STRLEN is essentially random but serves to
 * prevent the kernel from being unduly impacted by misaddressed pointers.
 * MAX_ARG_STRINGS is chosen to fit in a signed 32-bit integer.
 */
#define MAX_ARG_STRLEN (PAGE_SIZE * 32)
#define MAX_ARG_STRINGS 0x7FFFFFFF
```

Which is a ridiculously large number:

```
$ printf "%d\n" 0x7FFFFFFF
2147483647
```

Maybe it's a stack limit then? `bprm_stack_limits()` looks promising:

```c
static int bprm_stack_limits(struct linux_binprm *bprm)
{
	unsigned long limit, ptr_size;

	/*
	 * Limit to 1/4 of the max stack size or 3/4 of _STK_LIM
	 * (whichever is smaller) for the argv+env strings.
	 * This ensures that:
	 *  - the remaining binfmt code will not run out of stack space,
	 *  - the program will have a reasonable amount of stack left
	 *    to work from.
	 */
	limit = _STK_LIM / 4 * 3;
	limit = min(limit, bprm->rlim_stack.rlim_cur / 4);
	/*
	 * We've historically supported up to 32 pages (ARG_MAX)
	 * of argument strings even with small stacks
	 */
	limit = max_t(unsigned long, limit, ARG_MAX);
	/*
	 * We must account for the size of all the argv and envp pointers to
	 * the argv and envp strings, since they will also take up space in
	 * the stack. They aren't stored until much later when we can't
	 * signal to the parent that the child has run out of stack space.
	 * Instead, calculate it here so it's possible to fail gracefully.
	 *
	 * In the case of argc = 0, make sure there is space for adding a
	 * empty string (which will bump argc to 1), to ensure confused
	 * userspace programs don't start processing from argv[1], thinking
	 * argc can never be 0, to keep them from walking envp by accident.
	 * See do_execveat_common().
	 */
	ptr_size = (max(bprm->argc, 1) + bprm->envc) * sizeof(void *);
	if (limit <= ptr_size)
		return -E2BIG;
	limit -= ptr_size;

	bprm->argmin = bprm->p - limit;
	return 0;
}
```

This well-commented function tells us that formula here is. I'll
compress it as:

```c
limit = _STK_LIM / 4 * 3;
limit = min(limit, bprm->rlim_stack.rlim_cur / 4);
limit = max(limit, ARG_MAX);
```

Let's inline as many constants as we can here:

```
// from include/uapi/linux/resource.h
#define _STK_LIM  (8*1024*1024)
// from include/uapi/linux/limits.h
#define ARG_MAX 131072 /* # bytes of args + environ for exec() */
```

We'll get this 3-liner:

```c
limit = 6MB; // 8MB / 4 * 3
limit = min(limit, CURRENT_STACK_LIMIT / 4);
limit = max(limit, 128K);
```

The above in plain words: argument limits (in bytes) are at least 128K
and at most are 6MB. And by default it's `CURRENT_STACK_LIMIT / 4`.

`CURRENT_STACK_LIMIT` default is set to `_STK_LIM` as well:

```
$ ulimit -s
8192
```

Thus the argument length limit by default is `2MB`. And wee can raise up
to 6MB (3x) maximum if we set `ulimit -s` up to `24MB`. Setting stack
to anything higher would not affect argument limit.

That is the theory. Does it match the practice?

What did I miss? Why do we get only ~128K or argument limit for `gcc`
and not ~2MB?

There is a small catch: I kept exploring limits of `gcc` executable. In
`nixpkgs` it's a big shell wrapper. Part of wrapper's work is to set
various environment variables. Not just pass through the arguments.
And each environment variable is treated roughly like a command line
parameter.

Let's use known simple `printf` binary instead (it should not set any
environment variables internally) and see what are it's limits:

```
$ ./probe-argsize.bash -g $(which printf) "%s" --
189929
```

189K arguments! This looks more like `400KB` of argument bytes. But if you
know anything about `char * argv[]` parameter to `main()` you might know
that it's a pointer array. And pointers probably take most overhead
here.

Let's use longer arguments to mitigate pointer overhead:

```
$ filler=0123456789-90123456789-0123456789-0123456789-0123456789-0123456789-01234567889-0123456789-0123456789
$ ./probe-argsize.bash $filler $(which printf) "%s" --
19165
```

19K arguments 100 bytes each: that looks more like `~2MB` limit. Phew!
We finally got our theoretical limit.

## Ultimate argument count `linux` allows today

Given that argument count somehow depends on stack size via
`CURRENT_STACK_LIMIT` can we just change default stack size and set
`10x` limit for argument count?

What is The Largest argument count we can pass on `linux`?

It's not very hard to figure it out from the `bprm_stack_limits()` code
above.

To recap our environment structure in memory (on stack) is an array of
pointers to the null-terminated string pool:

```
 | (char *)argv[0] -> "arg0\0"
 | (char *)argv[1] -> "arg1\0"
 | (char *)argv[2] -> "arg2\0"
 | (char *)argv[3] -> "arg3\0"
 | (char *)argv[4] -> "arg4\0"
 | (char *)argv[5] -> "arg5\0"
 | ...
 | (char *)argv[N] -> "argN\0"
 | (char *)NULL
```

This means that our most memory-efficient input would be an array of
zero-byte arguments. Note, that in this case main overhead is on
pointer array and not on the arguments themselves.

That would be something like:

```
 | (char *)argv[0] -> "\0"
 | (char *)argv[1] -> "\0"
 | (char *)argv[2] -> "\0"
 | (char *)argv[3] -> "\0"
 | (char *)argv[4] -> "\0"
 | (char *)argv[5] -> "\0"
 | ...
 | (char *)argv[N] -> "\0"
 | (char *)NULL
```

On 64-bit systems this gives us 9 bytes per argument. That gives us
`2MB / 9 = 233016` arguments. The experiment confirms that we are very
close:

```
$ ./probe-argsize.bash "" env -i $(which printf) "%s" --
232134
```

An exercise for the reader: why is it 822 arguments shorter that our
maximum theoretical value? A word of warning: it's not a very simple
question.

Given that `2MB` is derived from `1/4 * CURRENT_STACK_LIMIT` we can
increase that as well using `ulimit -s`. Let's add 100x:

```
$ ulimit -s
8192
$ ulimit -s 819200

$ ./probe-argsize.bash "" env -i $(which printf) "%s" --
698168
```

Note that it's only a 3x improvement (and not a 100x improvement).

This is exactly our `limit = 6MB;` absolute limit above. Once again: i
you need to get maximum out of your argument limits on today's `linux`
it's enough to set `ulimit -s` from `8MB` to `24MB`.

Which gives us final formula of `6MB / 9 = 699050` argument count on
64-bit systems.

Fun fact: on 32-bit host kernel the limit should probably be slightly higher
due to shorter pointer size:

- `2MB / 5 = 419430` arguments (compared to `233016` on `64-bit`)
- `6MB / 5 = 1258291` arguments (compared to `699050` on `64-bit`)

That is 1.8x larger than 64-bit systems!

Once again: it's a pretty silly benchmark as it's not very useful to
pass a million empty strings to the program. But it's a good model to
understand the absolute limits.

We can do more realistic estimates if we know average argument length
in our use case. Say, if the bulk of our parameters are paths to the
`/nix/store` we can safely say those are at least 50 bytes long.

For 100-bytes long use case we would get:

- `2MB / 109 = 19239` (64-bit), `2MB / 105 = 19972` (32-bit)
- `6MB / 109 = 57719` (64-bit), `6MB / 105 = 59918` (32-bit)

Thus for simple case we should be able to pull out almost `~20K` by
default and almost `~60K` with larger stack size.

## `nixpkgs` `gcc` wrapper mystery

So why does `gcc` have so much overhead of `-g` options? We get about
`190K` options for for `printf -g` and only 20K for `gcc -g` above.
That is almost 10x reduction.

Could `nixpkgs`'s `gcc` wrapper artificially inflate it's arguments
somehow? `strace` should help us verify that:

```
$ nix shell nixpkgs#gcc
$$ touch a.c && strace -etrace=execve -s 10000 -f -v -olog gcc -Ihow-many-duplcates -c a.c

$$ grep cc1 log
1290300 execve(".../cc1", [..., , "-I", "how-many-duplcates", ...], [..., "COLLECT_GCC_OPTIONS='-I' 'how-many-duplcates' ...", ...]) = 0
```

Not too bad: we see 2x explosion here:

- one option is in argument list
- another is in `COLLECT_GCC_OPTIONS=` environment

The `2x` explosion itself does not explain 10x reduction.

There is also an extra catch: the way I tried to set the variable in
`qemu` is via `NIX_CFLAGS_COMPILE=` environment variable. Those get
translated by package setup hooks not visible in `nix shell`. Let's
check their full effect.

I'll write a complete small derivation to demonstrate the explosion
closer to `qemu` failure mode.

Here is a `default.nix` derivation that should demonstrate the point:

```nix
{ pkgs ? import <nixpkgs> { } }:
pkgs.stdenv.mkDerivation {
  name = "probe-wrapper";
  dontUnpack = true;
  nativeBuildInputs = [ pkgs.strace ];

  NIX_CFLAGS_COMPILE = "-Ihow-many-duplcates";

  postInstall = ''
    touch a.c
    strace -etrace=execve -s 10000 -f -v -o$out -- $CC -c a.c -o a.o
  '';
}
```

Now we can quickly build it and explore it's contents:

```
$ nix-build
$ grep cc1 result

31 execve(".../cc1", [..., "-I", "how-many-duplcates", ...], "NIX_CFLAGS_COMPILE_x86_64_unknown_linux_gnu=... -Ihow-many-duplcates ...", "NIX_CFLAGS_COMPILE=-Ihow-many-duplcates ...", "COLLECT_GCC_OPTIONS=... '-I' 'how-many-duplcates' ..."]) = 0
```

Things are a bit worse here: we see 4x explosion:

- [as before] one option is in argument list
- [as before] another is in `COLLECT_GCC_OPTIONS=` environment
- [new] `NIX_CFLAGS_COMPILE` variable, the one we added and are using for injection
- [new] `NIX_CFLAGS_COMPILE_x86_64_unknown_linux_gnu` variable, the one
  `pkgs/build-support/cc-wrapper/setup-hook.sh` setup hook set for us

And if we throw cross-compilation into the picture (ideally I wanted to
avoid `__FILE__` leaks in all cases) and use all three
`NIX_CFLAGS_COMPILE_FOR_BUILD`, `NIX_CFLAGS_COMPILE` and
`NIX_CFLAGS_COMPILE_FOR_TARGET`:

```nix
{ pkgs ? import <nixpkgs> { crossSystem = "riscv64-linux"; } }:
pkgs.stdenv.mkDerivation {
  name = "probe-wrapper";
  dontUnpack = true;
  nativeBuildInputs = [ pkgs.buildPackages.strace ];

  NIX_CFLAGS_COMPILE_FOR_BUILD =  "-Ihow-many-duplcates";
  NIX_CFLAGS_COMPILE =            "-Ihow-many-duplcates";
  NIX_CFLAGS_COMPILE_FOR_TARGET = "-Ihow-many-duplcates";

  postInstall = ''
    touch a.c
    strace -etrace=execve -s 10000 -f -v -o$out -- $CC -c a.c -o a.o
  '';
}
```

```
$ nix-build
$ grep cc1 result

35 execve(".../cc1", [..., "-I", "how-many-duplcates", ...], "NIX_CFLAGS_COMPILE_riscv64_unknown_linux_gnu=... -Ihow-many-duplcates ...", "NIX_CFLAGS_COMPILE_FOR_BUILD=-Ihow-many-duplcates ...", "NIX_CFLAGS_COMPILE=-Ihow-many-duplcates ...", NIX_CFLAGS_COMPILE_FOR_TARGET=-Ihow-many-duplcates ...", "COLLECT_GCC_OPTIONS=... '-I' 'how-many-duplcates' ..."]) = 0
```

Here we see 6x explosion:

- [as before] one option is in argument list
- [as before] another is in `COLLECT_GCC_OPTIONS=` environment
- [as before] `NIX_CFLAGS_COMPILE` variable, the one we added and are using for injection
- [as before] `NIX_CFLAGS_COMPILE_riscv64_unknown_linux_gnu` variable, the one
  `pkgs/build-support/cc-wrapper/setup-hook.sh` setup hook set for us
- [new] `NIX_CFLAGS_COMPILE_FOR_BUILD` variable, we set ourselves
- [new] `NIX_CFLAGS_COMPILE_FOR_TARGET` variable, we set ourselves

None of these variables looks redundant: they serve the purpose to
propagate flags across shell wrappers. Thus we'll have to keep in mind
this 6x explosion.

And yet. 6x explosion does not explain why mere `-g` option can be
present only 20K times instead of 200K times.

## Another nasty limit

I skimmed through maximum individual variable length limit above as if
it did not exist. But it's there! Look:

```
$ $(which printf) "%s" $(printf "%0*d" 100000) $(printf "%0*d" 100000) $(printf "%0*d" 100000) $(printf "%0*d" 100000) >/dev/null; echo $?
0
$ $(which printf) "%s" $(printf "%0*d" 200000) >/dev/null; echo $?
-bash: /run/current-system/sw/bin/printf: Argument list too long
126
```

More precise limit is `128K`:

```
$ $(which printf) "%s" $(printf "%0*d" $((2 ** 17)) ) >/dev/null; echo $?
-bash: /run/current-system/sw/bin/printf: Argument list too long
126
$ $(which printf) "%s" $(printf "%0*d" $((2 ** 17 - 1)) ) >/dev/null; echo $?
0
$ echo $((2 ** 17))
131072
```

It comes from `copy_string()` from the same
[fs/exec.c](https://github.com/torvalds/linux/blob/2cf0f715623872823a72e451243bbf555d10d032/fs/exec.c#L523):

```c
static int copy_strings(int argc, struct user_arg_ptr argv,
			struct linux_binprm *bprm)
{
	struct page *kmapped_page = NULL;
	char *kaddr = NULL;
	unsigned long kpos = 0;
	int ret;

	while (argc-- > 0) {
		const char __user *str;
		int len;
		unsigned long pos;

		ret = -EFAULT;
		str = get_user_arg_ptr(argv, argc);
		if (IS_ERR(str))
			goto out;

		len = strnlen_user(str, MAX_ARG_STRLEN);
		if (!len)
			goto out;

		if (!len)
			goto out;

		ret = -E2BIG;
		if (!valid_arg_len(bprm, len))
			goto out;
	// ...
out:
	// ...
	return ret;
}

static bool valid_arg_len(struct linux_binprm *bprm, long len)
{
	return len <= MAX_ARG_STRLEN;
}
```

Here `MAX_ARG_STRLEN` (`128K`) limits individual entries in arguments
and environment variable entries:

```
$ E=$(printf "%0*d" $((2 ** 17 - 3)) ) $(which printf) "1" >/dev/null; echo $?
0
$ E=$(printf "%0*d" $((2 ** 17 - 2)) ) $(which printf) "1" >/dev/null; echo $?
-bash: /run/current-system/sw/bin/printf: Argument list too long
126
```

Here we run `printf 1` with a large `E=000000...000` variable and expose
the same `128K` limit.

This is bad news. As we saw above `gcc` uses single `COLLECT_GCC_OPTIONS`
variable to pass all options around. And on top of that it quotes each
argument.

As a result `gcc -g -g ...` turns into
`COLLECT_GCC_OPTIONS="'-g' '-g' ..."`. This means that we are limited by
a `128K` limit when it comes to `gcc` flags.

To estimate our best limit `-g` gets translated to `"'-g' "`: 2 bytes
get turned into 5. That means we can put `128K / 5 = 26214` entries.

That is very close to our `26118` limit we got above. Again, why the
values differ by `96` entries is another exercise to the reader.

## The variable budget

Let's see what worst-case scenario for the longest option we have for
`-fmacro-prefix-map=` on my system for directories that have `include`
subdirectory:

```
$ for d in *; do [[ -d "$d/include" ]] && echo "${#d} $d"; done | sort -k 1 -n | tail -n 10
87 s2zpr18vjr274yaalgz5c4g7dx51cvb9-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
87 s5gn9gwwwpm8ln84apjb8hbrhihq72d4-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
87 scihnclavazxzk0vgblhp4371s3gr40c-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
87 sdf4v7s0avfkgnnnx1hq000ydl7bz9sd-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
87 xfl1c6qmr8b39bxgpmjp004afknm54av-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
87 y00bh71xvsrkazdjy5w4ii8pdkdwb5hr-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
87 z8k642hwqflwmp16ql3jj7z1pslyznzq-mcfgthreads-x86_64-w64-mingw32-unstable-2023-06-06-dev
88 6lw175bhq8d669jl1ddvwy49hcmlysi0-gmp-with-cxx-static-x86_64-unknown-linux-musl-6.3.0-dev
89 ljrwhpdcv7xzmxbank3jdi2xyga553s5-gmp-with-cxx-static-aarch64-unknown-linux-musl-6.3.0-dev
94 r5gzvnjnq024gr830nvpkcyd7n4ip33v-libnetfilter_conntrack-static-x86_64-unknown-linux-musl-1.0.9
```

About 90 bytes. With added `/nix/store` prefix that would be `100`
characters long paths. Nice round number.

This makes `-fmacro-prefix-map=/nix/store/...=/nix/store/...` options
around 220 bytes.

How many of those can we realistically set for our `2MB` budget?

- for 4x overhead: `2 * 1024 * 1024 / (220 + 9) / 4 = 2289` variables
- for 6x overhead: `2 * 1024 * 1024 / (220 + 9) / 6 = 1526` variables

But given `COLLECT_GCC_OPTIONS` limitation for `128K` our calculation
becomes even more pessimistic:

- for no overhead: `128 * 1024 / (220 + 9) / 4 = 143` variables!

It might sound like a lot but it's not that much of a budget: some
packaging systems (like `haskell`'s `hackage`) do like small
fine-grained packages and occasionally do install `C` header files.
`nix` itself favours smaller packages to speed up rebuilds and shrink
runtime closure. `pkg-config` is geared towards installing packages
into individual directories.

And what is worse: `NIX_CFLAGS_COMPILE` is not the only option
that exhibits this behaviour. Here is the longer list following
`mangleVarList` used in `nixpkgs`:

```
# from pkgs/build-support/cc-wrapper/add-flags.sh
NIX_CFLAGS_COMPILE
NIX_CFLAGS_COMPILE_BEFORE
NIX_CFLAGS_LINK
NIX_CXXSTDLIB_COMPILE
NIX_CXXSTDLIB_LINK
NIX_GNATFLAGS_COMPILE

# from ./pkgs/build-support/bintools-wrapper/add-flags.sh
NIX_IGNORE_LD_THROUGH_GCC
NIX_LDFLAGS
NIX_LDFLAGS_BEFORE
NIX_DYNAMIC_LINKER
NIX_LDFLAGS_AFTER
NIX_LDFLAGS_HARDEN
NIX_HARDENING_ENABLE

# from pkgs/build-support/cc-wrapper/add-gnat-extra-flags.sh
NIX_GNATMAKE_CARGS

# from pkgs/build-support/pkg-config-wrapper/add-flags.sh
PKG_CONFIG_PATH
```

Thus worst case we get to set at most ~150 entries for all of these
variables.

## Parting words

Even on `linux` command line argument limits are hard. If you can try to
use files to pass inputs of unbounded sizes.

`linux` has unreachable `0-x7fffFFFF` argument count limit when executing
the commands. It does have an overall limit `2MB` limit that one can
increase to `6MB`. And on top of that individual arguments are limited
by `128K` limit that you can't raise.

The above effectively means that argument passing overheads define
argument count limit. For arguments of length `0` by default you can
pass `233016` empty strings on 64-bit kernel and `419430` on 32-bit one.

If you increase the default stack size with `ulimit -s` you can get up
to `1258291` empty string arguments on 32-bits and `699050` on 64-bits.

For practical argument lengths actual values are way smaller: in order
of thousands to tens of thousands.

`gcc` is a special `COLLECT_GCC_OPTIONS` case and it has a limit of
`128KB` making argument limits onto hundreds.

Initially I planned to workaround `qemu` failure by using `gcc`'s
response files. I thought it would save the problem completely.
Unfortunately `COLLECT_GCC_OPTIONS` contains already expanded response
file contents and thus response files will only remove multiplication
factor but will not sidestep 128KB limit.

On the bright side `COLLECT_GCC_OPTIONS` is an internal `gcc`
implementation detail that should be fixable without much external
impact. Even if we move it to proper argument list it should already
unlock `2MB` limit. And if we could pass response files through we cloud
sidestep the limit entirely. Filed <https://gcc.gnu.org/PR111527> to
`gcc` upstream.

But meanwhile I'll try to patch `gcc` (and maybe `clang`?) just for
`nixpkgs` to apply programmatic mangling similar to:

```
# Pseudocode. real regexps do not work for `gcc`:
-fmacro-prefix-map=s,$NIX_STORE/[a-z0-9]{32}-,$NIX_STORE/eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee-
```

Have fun!

