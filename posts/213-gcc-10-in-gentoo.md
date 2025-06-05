---
title: "GCC 10 in Gentoo"
date: March 23, 2020
---

New `gcc` releases are always great fun to have: new optimisations, new
CPUs and platforms supported, new diagnostic warnings.
But also occasional new bugs or fixes that break backwards
compatibility.
I was wondering the other day: what does it take to get live `gcc`
right from `git` tree into `gentoo`.
That would allow people to try `gcc` in VMs or chroots in a full
`gentoo` environment. We would be able to find out breakages before
the release comes out, would allow users to test early fixes and (most
important) have another way to break our systems.

I cleaned up `toolchain.eclass` a bit and made [live `gcc-10`
ebuild](https://gitweb.gentoo.org/repo/gentoo.git/tree/sys-devel/gcc/gcc-10.0.1_pre9999.ebuild).
It should be as easy to create live `ebuild` for stable `gcc` branches.
You just need to copy the `ebuild` and drop `EGIT_BRANCH=master` line.
`toolchain.eclass` should do the right thing.
To test that `ebuild` actually works I decided to switch my main desktop
to live `gcc` form `master` to see how good/bad it is.

## `gcc-config` bug

First thing to break was `sys-devel/gcc-config` tool.
`gcc-config` allows you to switch compilers at runtime by juggling
`/usr/bin/gcc` symlink (and a few nearby symlinks).
`gcc-config` previously assumed that `gcc` versions could be
lexicographically sorted (used `sort` tool). As a result it always
picked `gcc-9` as the latest even when `gcc-10` was present.

`gcc-config` is also a low-dependency tool written in `bash` mostly
to allow you to recover system from broken state with inactive `gcc`.
It has no direct access to advanced sorting functions, but it needs to
solve a simple problem of ordering `gcc` versions (like `9.2.0`, `10.0.1`,
`11.0.0`) to pick the most recent `gcc` version as a primary
`libstdc++.so` provider.

``` 
$ printf '%s\n' 9.2.0 10.0.1 11.0.0 | sort
10.0.1
11.0.0
9.2.0
```

Here is a quiz question 1 for you: how would you implement simple
version-aware sorting assuming `<number>.<number>.<number>`
versioning?

Quiz answer 1: one of the solutions is [(spoiler) fix in
`gcc-config`](https://gitweb.gentoo.org/proj/gcc-config.git/commit/?id=bc80e12ab133a00ece4059df40d672889fcf6bf0).

Looks like an obscure bug. People should not rely on lexicographical
number sorting generally, right?

Quiz question 2: guess how many more bugs I encountered related to the
fact that '10' < '9' as a string.

## `ebuild` bugs

The next bug was in `gcc` `ebuild` itself (well, in
`toolchain.eclass`). Ebuilds also happen to be written in `bash` and
you need to be careful with number arithmetic there:

``` bash
$ [[ 10 < 9 ]] && echo yes || echo no
yes
$ [[ 10 -lt 9 ]] && echo yes || echo no
no
```

`toolchain.eclass` did not get it right. I had to tweak it with [a
patch](https://gitweb.gentoo.org/repo/gentoo.git/commit/eclass/toolchain.eclass?id=12bfa1e4f9595dbbcbe0a442c6a63bc3ef890cc2).
It happened to work for all previous `gcc` versions. I wondered how
widespread this problem was in `ebuild`s:

``` 
$ git grep -E '\[\[.*[<>]\s*[0-9]+.*\]\]' | cat
eclass/kernel-2.eclass:if [[ -n ${KV_MINOR} &&  ${KV_MAJOR}.${KV_MINOR}.${KV_PATCH} < 2.6.27 ]] ; then
net-libs/webkit-gtk/webkit-gtk-2.24.4.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.9 ]] ; then
net-mail/mailgraph/mailgraph-1.14-r2.ebuild:if [[ ${REPLACING_VERSIONS} < 1.13 ]] ; then
net-wireless/cpyrit-cuda/cpyrit-cuda-0.5.0.ebuild:if tc-is-gcc && [[ $(gcc-version) > 4.8 ]]; then
profiles/prefix/windows/winnt/profile.bashrc:[[ ${#mysrcs[@]} < 2 ]] && exit 0
sys-cluster/corosync/corosync-2.3.5.ebuild:if [[ ${REPLACING_VERSIONS} < 2.0 ]]; then
sys-cluster/corosync/corosync-2.4.2.ebuild:if [[ ${REPLACING_VERSIONS} < 2.0 ]]; then
sys-cluster/torque/torque-4.1.7-r1.ebuild:if [[ -z "${REPLACING_VERSIONS}" ]] || [[ ${REPLACING_VERSIONS} < 4 ]]; then
sys-cluster/torque/torque-4.2.10-r1.ebuild:if [[ ${showmessage} > 0 ]]; then
sys-libs/libcxx/libcxx-10.0.0.9999.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.7 ]] ; then
sys-libs/libcxx/libcxx-10.0.0_rc3.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.7 ]] ; then
sys-libs/libcxx/libcxx-11.0.0.9999.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.7 ]] ; then
sys-libs/libcxx/libcxx-7.1.0.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.7 ]] ; then
sys-libs/libcxx/libcxx-8.0.1.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.7 ]] ; then
sys-libs/libcxx/libcxx-9.0.1.ebuild:if tc-is-gcc && [[ $(gcc-version) < 4.7 ]] ; then
...
```

These all are wrong. Note: it's a very crude grep. I'm sure there are
a lot more bugs like that.
<https://bugs.gentoo.org/705240> tracks a few cases I managed to grep
out. If you have found more of these please pile your bugs on!
After blockers fixed I got `gcc-10` installed and selected properly.

## `-fno-common` change

Then I attempted to build newly released version of `dev-lang/erlang`
as part of usual `ebuild` maintenance. The build failed with duplicate
symbol definition. I assumed it was a bug in the newly written code, and
hacked up a [fix for upstream](https://github.com/erlang/otp/pull/2503).
Slightly later I tried to build older `erlang` versions in repository
and discovered they were also broken. Searching around I encountered
<https://gcc.gnu.org/PR85678> which talks about switching `-fcommon`
default to `-fno-common` to effectively forbid code similar to below:

``` c
// a.c
int a;
// b.c
int a;
```

``` 
$ gcc-9.3.0 a.c b.c -o libab.so -shared -fPIC
<ok>
$ gcc-10.0.1 a.c b.c -o libab.so -shared -fPIC
ld: /tmp/ccWIg1Zj.o:(.bss+0x0): multiple definition of `a'; /tmp/ccdJsLyo.o:(.bss+0x0): first defined here
collect2: error: ld returned 1 exit status
```

Now you have to specify explicit `extern` to convert one definition
site to declaration and avoid duplicate (mergeable) definitions.

Quiz question 3: how many packages do you think are broken like that?
One? Ten? A hundred? Guess a number.

Luckily it's easy to find most of the packages using `gcc-9` before
`gcc-10` is released just by trying to build packages with
`CFLAGS="$CFLAGS -fno-common"`.
Toralf did just that using his magic [`tinderbox`
setup](https://github.com/toralf/tinderbox) and built an almost complete
list of affected packages. See blockers in [the blocker
bug](https://bugs.gentoo.org/705764).
If you got a new fancy failure please pile your new bug onto blocker
above. Also feel free to pick a bug from there and work on a patch for
`gentoo` and/or upstream. We will need many hands to fix those leftovers.
Luckily the fixes are very mechanical and can be done without too deep
understanding of projects' internals.
<https://wiki.gentoo.org/wiki/Gcc_10_porting_notes/fno_common> page has
more hints.
I spent some time fixing individual packages broken on my system and
then sent out wider announcement:
<https://archives.gentoo.org/gentoo-dev/message/086ce3c09dda598aa3bdee3fe55a3dca>

Quiz answer 3: <https://bugs.gentoo.org/705764> reports 585 packages
broken so far.

Aside from `-fno-common` bugs other things started popping up.

## `vim` crash

In <https://bugs.gentoo.org/706324> `gentoo` user `<lekto@o2.pl>` reported
`vim` crash on `gcc-10`. I was glad that someone else tried it and
found a subtle real issue. I tried to build-and-run `vim` on my
machine and managed to reproduce the failure.

Quiz question 4: guess what caused the crash! Is it a compiler bug or
not? How picky you think `vim` is to a `c` compiler and it's properties?
The crash backtrace looked like that:

``` 
#7  0x00007f43b3ee8359 in __libc_message (action=<optimized out>,
  fmt=fmt@entry=0x7f43b3fffd4c "*** %s ***: %s terminated\n")
    at ../sysdeps/posix/libc_fatal.c:181
#8  0x00007f43b3f81545 in __GI___fortify_fail_abort (need_backtrace=need_backtrace@entry=true,
  msg=msg@entry=0x7f43b3fffcd8 "buffer overflow detected")
    at fortify_fail.c:28
#9  0x00007f43b3f81581 in __GI___fortify_fail (
  msg=msg@entry=0x7f43b3fffcd8 "buffer overflow detected")
    at fortify_fail.c:44
#10 0x00007f43b3f7f720 in __GI___chk_fail () at chk_fail.c:28
#11 0x0000563edb430ad9 in strcpy (__src=0x563edb48b7a3 "0", __dest=0x563edc345bd1 "") at /usr/include/bits/string_fortified.h:90
#12 add_nr_var (nr=<optimized out>, name=0x563edb48b7a3 "0", v=<optimizedout>, dp=0x563edc345f68) at userfunc.c:625
```

Buffer overflow. Uh-oh, that should never happen, right?
`gcc` build log even reported the line as potentially having a buffer
overflow at the same `userfunc.c:625` line `gdb` pointed me to:

``` 
x86_64-pc-linux-gnu-gcc -c -I. -Iproto -DHAVE_CONFIG_H     -march=sandybridge -mtune=sandybridge -maes --param=l1-cache-size=32 --param=l1-cache-line-size=64 --param=l2-cache-size=8192 -O2 -pipe -fdiagnostics-show-option -frecord-gcc-switches -Wall -Wextra -Wstack-protector -g        -o objects/userfunc.o userfunc.c
In file included from /usr/include/string.h:494,
                 from os_unix.h:465,
                 from vim.h:234,
                 from userfunc.c:14:
In function 'strcpy',
    inlined from 'add_nr_var' at userfunc.c:625:5,
    inlined from 'call_user_func' at userfunc.c:858:5,
    inlined from 'call_func' at userfunc.c:1626:7:
/usr/include/bits/string_fortified.h:90:10: warning: '__builtin___memcpy_chk' writing 2 bytes into a region of size 1 overflows the destination [-Wstringop-overflow=]
   90 |   return __builtin___strcpy_chk (__dest, __src, __bos (__dest));
      |          ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
```

Turns out internally `vim` uses the following hack to implement
key/value store with variable length keys:

``` c
struct dictitem_S
 {
     typval_T di_tv;     // type and value of the variable
     char_u   di_flags;  // flags (only used for variable)
     char_u   di_key[1]; // key (actually longer!)
 };
//...
#define STRCPY(d, s)    strcpy((char *)(d), (char *)(s))
STRCPY(v->di_key, name);
```

And any string function like `strcpy()` or `memcpy()` is statically
known to `gcc` as a buffer overflow: `di_key` is always 1 byte long.
Runtime buffer overflow checking is enabled by passing
`-D_FORTIFY_SOURCE=2` to `gcc`. Many distributions enable overflow
checking by default. `gentoo` is no exception.
The workaround `vim` uses to avoid these failures is to disable buffer
overflow checks from being emitted by using `-D_FORTIFY_SOURCE=1`
define.
Except that in this case `-D_FORTIFY_SOURCE=1` was not applied. To see
why let's look at the `configure.ac` around `_FORTIFY_SOURCE`
handling:

``` 
gccversion=`$CC -dumpversion`
dnl ...
gccmajor=`echo "$gccversion" | sed -e 's/^\([[1-9]]\)\..*$/\1/g'`
dnl ...
AC_MSG_CHECKING(whether we need -D_FORTIFY_SOURCE=1)
if test "$gccmajor" -gt "3"; then
  dnl slightly simplified cimparing to actual code
  CFLAGS="$CFLAGS -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1"
  CPPFLAGS="$CPPFLAGS -U_FORTIFY_SOURCE -D_FORTIFY_SOURCE=1"
  AC_MSG_RESULT(yes)
else
  AC_MSG_RESULT(no)
fi
```

Do you see the bug here? `vim` assumes `gcc -dumpversion` has format
of `<digit>.<anything>`. But `gcc-10.0.1` is
`<digit><digit>.<anything>`. As a result `-D_FORTIFY_SOURCE=1`
was not applied and we got broken binary. The fix is trivial:
[patch](https://github.com/vim/vim/commit/7077892a7918845a00ce8d7833b43cc6cbed2081).

``` diff
-gccmajor=`echo "$gccversion" | sed -e 's/^\([[1-9]]\)\..*$/\1/g'`
+gccmajor=`echo "$gccversion" | sed -e 's/^\([[0-9]]\+\)\..*$/\1/g'`
```

Arguably `vim` should not use known-broken `c` constructs and use
something else instead. Be it manually managed `void *` memory chunks
or flexible arrays on modern compilers.
I would not be surprised if `gcc` already generates invalid code for
`vim` assuming that out-of-bounds array access is not supposed to
happen in the code. That would allow `gcc` to delete most of code
working with 1-byte arrays as dead.
From discussion in <https://github.com/vim/vim/issues/5581> it looks
like single-byte-sized array are staying for longer though.

Quiz answer 4: `gcc` version parsing did not expect two digits.

## `perl` crash

Somehow `perl` was also broken by `gcc-10`:

``` 
x86_64-pc-linux-gnu-gcc -c -DPERL_CORE -fwrapv -fpcc-struct-return -pipe -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64 -march=sandybridge -mtune=sandybridge -maes --param=l1-cache-size=32 --param=l1-cache-line-size=64 --param=l2-cache-size=8192 -O2 -pipe -fdiagnostics-show-option -frecord-gcc-switches -Wall -Wextra -Wstack-protector -g -Wall -fPIC gv.c
...
LD_LIBRARY_PATH=/tmp/portage/dev-lang/perl-5.30.1/work/perl-5.30.1 /tmp/portage/dev-lang/perl-5.30.1/work/perl-5.30.1/preload /tmp/portage/dev-lang/perl-5.30.1/work/perl-5.30.1/libperl.so.5.30.1 ./miniperl -w -Ilib -Idist/Exporter/lib -MExporter -e '<?>' || sh -c 'echo >&2 Failed to build miniperl.  Please run make minitest; exit 1'
Attempt to free unreferenced scalar: SV 0x5555ed3e1378.
/bin/sh: line 1: 4057907 Segmentation fault      (core dumped) 
```

This one is hard. Can you quickly guess what is suspiciously wrong here?
In this case the runtime (`perl` build-time) failure happens due to use of
`-fpcc-struct-return` flag that changes compiler's ABI:

``` 
-fpcc-struct-return:
    Return "short" "struct" and "union" values in memory
    like longer ones, rather than in registers.
```

Looking at the [upstream
fix](https://github.com/Perl/perl5/commit/6bd6308fcea3541e505651bf8e8127a4a03d22cd)
this flag is a result of configure script thinking it deals with `gcc-1`:

``` diff
-1*) dflt="$dflt -fpcc-struct-return" ;;
+1.*) dflt="$dflt -fpcc-struct-return" ;;
```

Once again version parsing did not expect two digits.

## `linux` crash

The next test for a new compiler is to try to boot into kernel built by
`gcc-10`.
Rebuilding and reinstalling `grub2` caused no problems. But rebuilding
the kernel made it unbootable on a real machine. Worst thing was that I
got no screen output at all after a boot loader prompt.
For some reason `qemu-system-x86_64` was able to boot kernel just
fine. Not easy to debug.
I needed some indication how far the boot process got. I managed to get
it in a few ways: via `efifb earlycon` and via `xdbc` (USB-3 debug
capability).
The simplest one that does not require second machine was `efifb
earlycon`.

## `efifb earlycon`

On `EFI` systems you can emit text output almost instantly at kernel
boot. As `EFI` is already initialized it provides kernel a graphical
frame buffer: a memory range to write your pixels in.
`EFI` frame buffer is slightly different from VGA text mode but not too
much.
Kernel only needs to find out where frame buffer memory resides to render
glyphs right there. Enabling early frame buffer appeared to be a bit
tricky though. We need two things:

1.  a few unusual features built into kernel
2.  kernel parameters to enable early console

Kernel config:

- `CONFIG_FB_EFI=y`
- `CONFIG_EFI_EARLYCON=y`
- `CONFIG_FB_SIMPLE=y`
- `X86_SYSFB=y`
- `SERIAL_8250=y`
- `SERIAL_8250_CONSOLE=y`

Kernel parameters: `"earlycon=efifb keep_bootcon"`.
This allowed me to get an early boot failure on screen:

``` 
Kernel panic — not syncing: stack-protector: Kernel stack is corrupted in: start_secondary+0x191/0x1a0
CPU: 1 PID: 0 Comm: swapper/1 Not tainted 5.6.0-rc5—00235—gfffb08b37df9 #139
Hardware name: Gigabyte Technology Co., Ltd. To be filled by O.E.M./H77M—D3H, BIOS F12 11/14/2013
Call Trace:
  dump_stack+0x71/0xa0
  panic+0x107/0x2b8
  ? start_secondary+0x191/0x1a0
  __stack_chk_fail+0x15/0x20
  start_secondary+0x191/0x1a0
  secondary_startup_64+0xa4/0xb0
-—-[ end Kernel panic — not syncing: stack—protector: Kernel stack is corrupted in: start_secondary+0x191
```

Woohoo! That I was able to work with. I looked at [`start_secondary()`
definition](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/x86/kernel/smpboot.c):

``` c
/*
 * Activate a secondary processor.
 */
static void notrace start_secondary(void *unused)
{
    /*
     * Don't put *anything* except direct CPU state initialization
     * before cpu_init(), SMP booting is too fragile that we want to
     * limit the things done here to the most necessary things.
     */
    cr4_init();
    // ...
    cpu_init();
    // ...
    check_tsc_sync_target();
    // ...
    set_cpu_online(smp_processor_id(), true);
    // ...

    /* to prevent fake stack check failure in clock setup */
    boot_init_stack_canary();

    // ...
    cpu_startup_entry(CPUHP_AP_ONLINE_IDLE);
}

/*
 * Initialize the stackprotector canary value.
 *
 * NOTE: this must only be called from functions that never return,
 * and it must always be inlined.
 */
static __always_inline void boot_init_stack_canary(void)
{
    u64 canary;
    u64 tsc;

    BUILD_BUG_ON(offsetof(struct fixed_percpu_data, stack_canary) != 40);
    /*
     * We both use the random pool and the current TSC as a source
     * of randomness. The TSC only matters for very early init,
     * there it already has some randomness on most systems. Later
     * on during the bootup the random pool has true entropy too.
     */
    get_random_bytes(&canary, sizeof(canary));
    tsc = rdtsc();
    canary += tsc + (tsc << 32UL);
    canary &= CANARY_MASK;

    current->stack_canary = canary;
    this_cpu_write(fixed_percpu_data.stack_canary, canary);
}
```

Here `start_secondary()` detected a stack corruption failure and
reported it with `__stack_chk_fail()`. Note: `start_secondary()`
is itself responsible for initial stack canary setup.
The workaround to make a kernel boot was to avoid stack protection of
`start_secondary()`:

``` diff
--- a/arch/x86/kernel/Makefile
+++ b/arch/x86/kernel/Makefile
@@ -11,6 +11,12 @@ extra-y+= vmlinux.lds

 CPPFLAGS_vmlinux.lds += -U$(UTS_MACHINE)

+# smpboot's init_secondary initializes stack canary.
+# Make sure we don't emit stack checks before it's
+# initialized.
+nostackp := $(call cc-option, -fno-stack-protector)
+CFLAGS_smpboot.o := $(nostackp)
+
 ifdef CONFIG_FUNCTION_TRACER
 # Do not profile debug and lowlevel utilities
 CFLAGS_REMOVE_tsc.o = -pg
```

Kernel stack protection itself is enabled by
`CONFIG_STACKPROTECTOR_STRONG=y` option.
The real fix is discussed in <https://lkml.org/lkml/2020/3/14/186> and
will involve marking only `start_secondary()` as exempted from
protection.

## Parting words

After this short exercise I think `gcc-10` is somewhat usable in
`gentoo`. Now to the real bugs like <https://gcc.gnu.org/PR94185> and
<https://gcc.gnu.org/PR93763>.

- A simple act of changing software version from `9` to `10` can
  break enough software if you do it once in 20 years. If you plan to do
  something similar consider putting actual breaking changes into next
  release if possible. Version change might be severe enough :)
- Due to `-fno-common` default change `gcc-10` will be more
  disruptive than a usual `gcc` upgrade.
- `"earlycon=efifb keep_bootcon"` is a great and cheap way to get
  early boot log from the kernel.

Have fun!
