---
title: ia64 machine emulation
date: March 19, 2017
---

## TL;DR

There is a working `ia64` machine simulator:
[`ski`](https://ski.sourceforge.net/). It can be used to test `linux`
kernel on any host architecture. Or at least on `x86_64` :)
`ski` also supports direct binary translation (in `qemu-user` style)
but the simulated kernel is too old. Modern `glibc` requires a few recent
system calls to make binary translation work. Adding missing syscalls
should not be too hard. But I didn't try yet :)

## The problem

Roughly a year ago I tweaked [`gcc` code
generation](/posts/189-glibc-on-ia64-or-how-relocations-bootstrap.html)
on `ia64` to handle local symbol references in a slightly more
efficient way. All was well: I was able to restore `ghc-7.10.3` on
`ia64`.
But yesterday I got [a report](https://bugs.gentoo.org/601014) that my
patch breaks `linux` kernel startup on `ia64`. Uh-oh.
The error did look very generic (not driver-specific):

``` 
:: Loading from mdadm: raid0: invalid slot number 1 for IMM64
 raid1: invalid slot number 1 for IMM64
 async_tx: invalid slot number 1 for IMM64
 async_tx: invalid slot number 1 for IMM64
 async_tx: invalid slot number 1 for IMM64
 raid10: invalid slot number 1 for IMM64
:: Loading from fs: jbd2: invalid slot number 1 for IMM64
 jbd2: invalid slot number 1 for IMM64
 jbd2: invalid slot number 1 for IMM64
 sunrpc: invalid slot number 1 for IMM64
 fuse: invalid slot number 1 for IMM64
```

Kernel fails to load some modules.
Last time I looked at [a similar
problem](https://lkml.org/lkml/2011/12/14/324) in `sparc` land where
kernel failed to boot early on `sparc32` if kernel was built with
`CFLAGS=-Os`. In that case the most complex part was to understand how
early relocations were fixed up at boot stage. The eventual fix was
[trivial](https://lkml.org/lkml/2011/12/14/348).

Back to `ia64`. I suspected it would be something similar as `gcc` patch
did clearly change type of commonly used relocations. In this case these
are not early relocations of kernel itself but `ELF` relocation
processing of kernel modules (`.ko`) when they are loaded.
Below is the snippet of `linux` kernel that generates the `invalid slot
number 1 for IMM64` error:

``` c
// from module.c:
static int
apply_imm64 (struct module *mod, struct insn *insn, uint64_t val)
{
    if (slot(insn) != 2) {
        printk(KERN_ERR "%s: invalid slot number %d for IMM64\n",
           mod->name, slot(insn));
        return 0;
    }
    ia64_patch_imm64((u64) insn, val);
    return 1;
}
...
// from patch.c:
void
ia64_patch_imm64 (u64 insn_addr, u64 val)
{
    /* The assembler may generate offset pointing to either slot 1
       or slot 2 for a long (2-slot) instruction, occupying slots 1
       and 2.  */
      insn_addr &= -16UL;
    ia64_patch(insn_addr + 2,
       0x01fffefe000UL, (  ((val & 0x8000000000000000UL) >> 27) /* bit 63 -> 36 */
         | ((val & 0x0000000000200000UL) <<  0) /* bit 21 -> 21 */
         | ((val & 0x00000000001f0000UL) <<  6) /* bit 16 -> 22 */
         | ((val & 0x000000000000ff80UL) << 20) /* bit  7 -> 27 */
         | ((val & 0x000000000000007fUL) << 13) /* bit  0 -> 13 */));
    ia64_patch(insn_addr + 1, 0x1ffffffffffUL, val >> 22);
}
```

`apply_imm64` does a straightforward thing: it applies new 64-bit
immediate value into existing instruction at `insns` address.
Both modules are worth looking at as a whole:

- [`module.c`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/ia64/kernel/module.c)
- [`patch.c`](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/arch/ia64/kernel/patch.c)

Comparing to `sparc32` case this looks like a normal relocation
handling. Stuffing `imm64` value into 128-bit instruction requires
quite a bit of bit shift acrobatics :)
`ia64_patch_imm64` comment suggests function can handle `slot=1` and
`slot=2` cases while `apply_imm64` only accepts `slot=2` case.
Looking at past history of `patch.c` I found [this
commit](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=9c184a073bfd650cc791956d6ca79725bb682716).
It extends support for `slot=1` relocation in a trivial way:

``` diff
--- a/arch/ia64/kernel/patch.c
+++ b/arch/ia64/kernel/patch.c
@@ -64,22 +64,30 @@ ia64_patch (u64 insn_addr, u64 mask, u64 val)
 void
 ia64_patch_imm64 (u64 insn_addr, u64 val)
 {
-       ia64_patch(insn_addr,
+       /* The assembler may generate offset pointing to either slot 1
+          or slot 2 for a long (2-slot) instruction, occupying slots 1
+          and 2.  */
+       insn_addr &= -16UL;
+       ia64_patch(insn_addr + 2,
                   0x01fffefe000UL, (  ((val & 0x8000000000000000UL) >> 27) /* bit 63 -> 36 */
                                     | ((val & 0x0000000000200000UL) <<  0) /* bit 21 -> 21 */
                                     | ((val & 0x00000000001f0000UL) <<  6) /* bit 16 -> 22 */
                                     | ((val & 0x000000000000ff80UL) << 20) /* bit  7 -> 27 */
                                     | ((val & 0x000000000000007fUL) << 13) /* bit  0 -> 13 */));
-       ia64_patch(insn_addr - 1, 0x1ffffffffffUL, val >> 22);
+       ia64_patch(insn_addr + 1, 0x1ffffffffffUL, val >> 22);
 }
```

The [bug](https://www.sourceware.org/PR1433) in
commit message explains why the patch was needed.
What happens there is:
`ia64` has 128-bit 3-tuple (or 3-slot) "instruction bundle" format:
`5 bundle tag bits + 3 instructions * 41-bit = 128 bits`. 41 bit is
not enough to hold 64-bit immediate. Some 2-slot instructions allow to
encode 64-bit immediate operand (for example `mov r1=imm64`).
Sometimes linker needs to place a relocation for that `imm64` (say,
when `imm64` value is an address). That relocation points to one of
the instruction slots in instruction bundle as
`<bundle_address>+slot_number`. In case of 2-slot instructions both
`<bundle_address>+1` and `<bundle_address>+2` are valid
references to `imm64` value described by relocation. It's up to
assembler which slot number to choose.
But kernel used to handle only `<bundle_address>+1` case.
Given that [the
patch](https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/commit/?id=9c184a073bfd650cc791956d6ca79725bb682716)
is in kernel tree since 2005 we can safely lift `slot=1` restriction
from `apply_imm64` (and `apply_imm60` while at it):

``` diff
--- a/arch/ia64/kernel/module.c
+++ b/arch/ia64/kernel/module.c
@@ -153,7 +153,7 @@ slot (const struct insn *insn)
 static int
 apply_imm64 (struct module *mod, struct insn *insn, uint64_t val)
 {
-       if (slot(insn) != 2) {
+       if (slot(insn) != 1 && slot(insn) != 2) {
                printk(KERN_ERR "%s: invalid slot number %d for IMM64\n",
                       mod->name, slot(insn));
                return 0;
@@ -165,7 +165,7 @@ apply_imm64 (struct module *mod, struct insn *insn, uint64_t val)
 static int
 apply_imm60 (struct module *mod, struct insn *insn, uint64_t val)
 {
-       if (slot(insn) != 2) {
+       if (slot(insn) != 1 && slot(insn) != 2) {
                printk(KERN_ERR "%s: invalid slot number %d for IMM60\n",
                       mod->name, slot(insn));
                return 0;
```

Sent the same patch [upstream](https://lkml.org/lkml/2017/3/19/173).

## Testing the patch

That was the theory. But I didn't have a spare machine to try a
potentially bad kernel on. Normally I use `qemu-system` emulation like
`qemu-system-sparc`. But `ia64` is one of those rare beasts that are
not supported by `qemu` yet. I've found a bunch of `qemu-ia64` `GSOC`
projects but none of them got to booting anything.
I did encounter `ski` before but did not manage to start anything in
it.
`ski` can do both full system emulation and userland emulation: [wiki
page](https://web.archive.org/web/20200324132913/https://www.gelato.unsw.edu.au/IA64wiki/SkiSimulator).
`ski` is an old piece of software: it requires [some
fixes](https://gitweb.gentoo.org/repo/gentoo.git/tree/app-emulation/ski/files)
for modern toolchain. But otherwise system emulation just works!
Userland emulation is not so lucky. It does not know of syscalls added
for the past 15 years into the `linux` kernel. Should not be too hard to
add them. I didn't try yet :)

## `ski` `linux` boot howto

So is it hard to boot the kernel?
Here is a step-by-step instruction to get `ia64` kernel booted:

1.  Build `ski`: `emerge -1 ski`
2.  Build `ia64` cross-compiler: `crossdev ia64-unknown-linux-gnu`
3.  Build `ia64` kernel:

    ``` 
    $ make defconfig sim_config
    $ ARCH=ia64 CROSS_COMPILE=ia64-unknown-linux-gnu- make
    ```

4.  Build (for example) `bash` for `ia64`: `ARCH=ia64
    emerge-ia64-unknown-linux-gnu -1 bash`
5.  Prepare disk image (I `rsync`ed my `./root` and
    `/usr/ia64-unknown-linux-gnu` as-is):

``` bash
$ cat make_img.sh
#!/bin/sh

# 1G
if [ ! -f sdc ]; then
    dd of=sdc if=/dev/zero bs=512 count=2097151
    ls -lh sdc
    /sbin/mke2fs sdc
fi
sudo mount -t ext2 -oloop sdc ./mnt
sudo rsync -av ./root/ ./mnt/
sudo rsync -av /usr/ia64-unknown-linux-gnu/ ./mnt/
sudo umount ./mnt

$ ./make_img.sh
```

6.  Boot the thing: `bski arch/ia64/hp/sim/boot/bootloader vmlinux
    root=/dev/sda simscsi=./sd simeth=eth0 init=/bin/bash PATH=/bin rw`

Done!

I did install a bunch of other tools there to load modules, format a
`btrfs` filesystem and got other toys.

``` 
loading vmlinux...
starting kernel...
Linux version 4.11.0-rc2-00360-gbb62600ffd6e (slyfox@sf) (gcc version 5.4.0 (Gentoo 5.4.0-r3 p1.3, pie-0.6.5) ) #9 SMP PREEMPT Sat Mar 18 20:51:03 GMT 2017
EFI v1.00 by Hewlett-Packard:
efi:  SALsystab=0x10b9d0 
warning: unable to switch EFI into virtual mode (status=9223372036854775811)
No I/O port range found in EFI memory map, falling back to AR.KR0 (0xffffc000000)
console [simcons0] enabled
warning: skipping physical page 0
...
bash: cannot set terminal process group (-1): Inappropriate ioctl for device
bash: no job control in this shell

random: crng init done
I have no name!@(none) / # uname -a
Linux (none) 4.11.0-rc2-00360-gbb62600ffd6e #9 SMP PREEMPT Sat Mar 18 20:51:03 GMT 2017 ia64 GNU/Linux
I have no name!@(none) / # cat /proc/cpuinfo 
processor  : 0
vendor     : HP Ski Simulator
arch       : IA-64
family     : 31
model      : 0
model name : McKinley
revision   : 0
archrev    : 0
features   : branchlong, 16-byte atomic ops
cpu number : 0
cpu regs   : 4
cpu MHz    : 2.000
itc MHz    : 2.000000
BogoMIPS   : 1.22
siblings   : 1
```

2 MHz is not the fastest box in the world but it runs user space just
fine. That's it!
So far `btrfs` seems to work on `ia64`. Looks like my fix is not that bad
after all :)

Random facts:

- `ski` needs minimal tweaking to boot up an `ia64` VM
- `ia64` `ISA` in `ski` is complete enough to boot kernel and
  user space
- `ia64` simulation is very slow. I didn't risk running `gcc` in a
  VM.
- `strace` did not work in a VM. Don't know why. I suspect incomplete
  breakpoint implementation.
- `ia64` page size is `16K`. It means `btrfs` filesystem formatted on
  `ia64` can't be mounted on `x86_64`.

Have fun!
