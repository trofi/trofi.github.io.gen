---
title: glibc on ia64 or how relocations bootstrap
date: Dec 27, 2015
---

It was a rainy evening on `#gentoo-ia64` and suddenly

    00:40 < undersys> trying out glibc 2.21 on my ia64 box
    00:41 < undersys> all compiles fine, gets to preinstall test and fails
    00:41 < undersys> "simple run test (/usr/bin/cal) failed" wat :C
    ... <some trials and errors, still fails>
    16:53 < undersys> /usr/portage/sys-libs/glibc/files/eblits/pkg_preinst.eblit:
              line 24: 17141 Segmentation fault
                  LC_ALL=C ./ld-*.so --library-path . ${x} > /dev/null

Brendan tried hard to get `glibc` building on `ia64` by picking
various `gcc` versions (`4.7`, `4.8`, `4.9`) but no luck. Late
stage of `libc` sanity check suggested resulting `glibc` is completely
busted. Bug is known for a while as a
[#503838](https://bugs.gentoo.org/show_bug.cgi?id=503838).

## the problem

I've tried to reproduce crash on one of `ia64` boxes:

```
$ emerge -v1 =glibc-2.22-r1
...
>>> Installing (1 of 1) sys-libs/glibc-2.22-r1
 * Defaulting /etc/host.conf:multi to on
/bound/portage/sys-libs/glibc/files/eblits/pkg_preinst.eblit: line 24:
  6623 Segmentation fault      (core dumped) LC_ALL=C ./ld-*.so --library-path . ${x} > /dev/null
```

Got the same thing!
I think I've seen the same crash before but never bothered finding the
actual cause of `glibc` failure. I always ignored it and kept using
old `glibc` as I usually was up to something else when visited
`ia64` (likely `GHC` binary rebuild).
But not this time :)

## reproducing

The crash happened in `pkg_preinst` phase after a successful attempt
to build the package:

    * ERROR: sys-libs/glibc-2.22-r1 failed (preinst phase):
    *   simple run test (/usr/bin/cal) failed
    *
    * Call stack:
    *           ebuild.sh, line   93:  Called pkg_preinst
    *         environment, line 2841:  Called eblit-run 'pkg_preinst'
    *         environment, line  930:  Called eblit-glibc-pkg_preinst
    *   pkg_preinst.eblit, line   57:  Called glibc_sanity_check
    *   pkg_preinst.eblit, line   36:  Called die
    * The specific snippet of code:
    *              LC_ALL=C \
    *              ./ld-*.so --library-path . ${x} > /dev/null \
    *                      || die "simple run test (${x}) failed"

`sys-libs/glibc/files/eblits/pkg_preinst.eblit` tries to run the
following code:

``` bash
pushd ../../image/lib # 'make install' places freshly built glibc here

for x in cal date env free ls true uname uptime ; do
     x=$(type -p ${x})
     LC_ALL=C \
       ./ld-*.so --library-path . ${x} > /dev/null \
         || die "simple run test (${x}) failed"
done
```

This code snippet picks commonly installed programs from user's system
and tries to run them under freshly built dynamic interpreter (as
opposed to default `/lib/ld-linux-ia64.so.2`). And new interpreter
mysteriously crashes.

We can try to reproduce crash right from a build directory:

``` bash
$ pwd
/var/tmp/portage/sys-libs/glibc-2.22-r1/work/build-ia64-ia64-unknown-linux-gnu-nptl
$ elf/ld.so --library-path ../../image/lib /usr/bin/cal
Segmentation fault (core dumped)

$ readelf -a /usr/bin/cal | grep interpreter
[Requesting program interpreter: /lib/ld-linux-ia64.so.2]
$ /lib/ld-linux-ia64.so.2 --library-path ../image/lib /usr/bin/cal
    December 2015
Su Mo Tu We Th Fr Sa
       1  2  3  4  5
 6  7  8  9 10 11 12
13 14 15 16 17 18 19
20 21 22 23 24 25 26
27 28 29 30 31
```

Old `ld.so` still works, the new one doesn't.

# nailing down the location

Next step is to drop down into `gdb` and look at the crash:

    # gdb -q --args elf/ld.so --library-path ../image/lib /usr/bin/cal
    (gdb) run
    Starting program: /var/tmp/portage/sys-libs/glibc-2.22-r1/work/build-ia64-ia64-unknown-linux-gnu-nptl/elf/ld.so --library-path ../image/lib /usr/bin/cal
    Failed to read a valid object file image from memory.

    Program received signal SIGSEGV, Segmentation fault.
    0x200000080000b1f1 in elf_get_dynamic_info (temp=0x0, l=0x2000000800052ef8 <_rtld_local+2456>) at get-dynamic-info.h:70
    70                   + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
    (gdb) list
    65            else if ((d_tag_utype) DT_VALTAGIDX (dyn->d_tag) < DT_VALNUM)
    66              info[DT_VALTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
    67                   + DT_VERSIONTAGNUM + DT_EXTRANUM] = dyn;
    68            else if ((d_tag_utype) DT_ADDRTAGIDX (dyn->d_tag) < DT_ADDRNUM)
    69              info[DT_ADDRTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
    70                   + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
    71            ++dyn;
    72          }
    73
    74      #define DL_RO_DYN_TEMP_CNT      8
    (gdb) bt
    #0  0x200000080000b1f1 in elf_get_dynamic_info (temp=0x0, l=0x2000000800052ef8 <_rtld_local+2456>) at get-dynamic-info.h:70
    #1  _dl_start (arg=0x60000fffffffb2a0) at rtld.c:382
    #2  0x2000000800001a50 in _start ()

`gdb` shows the exact line number where crash happens, that's good. I
tried to check disassembly to see if anything obvious stands up.

    (gdb) disassemble
    Dump of assembler code for function _dl_start:
        0x200000080000a800 <+0>:     [MMB]       alloc r51=ar.pfs,26,22,0
        0x200000080000a801 <+1>:                 mov r52=r12
        0x200000080000a802 <+2>:                 nop.b 0x0
        0x200000080000a810 <+16>:    [MII]       adds r12=-16,r12
        0x200000080000a811 <+17>:                mov r50=b0
        ... <some pages later>
        0x200000080000b1d0 <+2512>:  [MMI]       sub r16=r25,r14;;
        0x200000080000b1d1 <+2513>:              cmp.ltu p6,p7=10,r16
        0x200000080000b1d2 <+2514>:              nop.i 0x0;;
        0x200000080000b1e0 <+2528>:  [MMI]       nop.m 0x0
        0x200000080000b1e1 <+2529>:        (p07) shladd r14=r14,3,r0
        0x200000080000b1e2 <+2530>:              nop.i 0x0;;
        0x200000080000b1f0 <+2544>:  [MMI] (p07) sub r14=r26,r14;;
    =>  0x200000080000b1f1 <+2545>:        (p07) st8 [r14]=r15
        0x200000080000b1f2 <+2546>:              adds r15=16,r15;;
        0x200000080000b200 <+2560>:  [MMI]       nop.m 0x0
        0x200000080000b201 <+2561>:              ld8 r14=[r15]
        0x200000080000b202 <+2562>:              nop.i 0x0;;
        0x200000080000b210 <+2576>:  [MIB]       nop.m 0x0
        ... <some pages later>
        0x200000080000b711 <+3857>:              nop.i 0x0
        0x200000080000b712 <+3858>:              br.few 0x200000080000b620 <_dl_start+3616>
        0x200000080000b720 <+3872>:  [MMI]       nop.m 0x0
        0x200000080000b721 <+3873>:              ld8 r14=[r15]
        0x200000080000b722 <+3874>:              nop.i 0x0;;
        0x200000080000b730 <+3888>:  [MIB]       nop.m 0x0
        0x200000080000b731 <+3889>:              cmp.eq p7,p6=0,r14
        0x200000080000b732 <+3890>:              br.few 0x200000080000a940 <_dl_start+320>;;
    End of assembler dump.

All in all `_dl_start` disassembly was 720 lines long (15 pages of
code). I was not able to easily find where `r15` register assignment
happened. Basically I had no idea what I was looking at :)

First off it's worth understanding why disassembly shows us
`_dl_start()` and not `elf_get_dynamic_info()`.

Here is the annotated backtrace (click on the links! They are fun):

- #0
  [0x200000080000b1f1](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/get-dynamic-info.h;h=dc8359d36ac3d97fbb36821a9f8a2deb7ba541e5;hb=HEAD#l69)
  in
  [elf_get_dynamic_info](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/get-dynamic-info.h;h=dc8359d36ac3d97fbb36821a9f8a2deb7ba541e5;hb=HEAD#l28)
  (temp=0x0, l=0x2000000800052ef8 \<\_rtld_local+2456\>) at
  [get-dynamic-info.h:70](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/get-dynamic-info.h;h=dc8359d36ac3d97fbb36821a9f8a2deb7ba541e5;hb=HEAD#l69)
- #1
  [\_dl_start](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/rtld.c;h=52160dfde6be42eba000c4e8136de0d190617270;hb=HEAD#l336)
  (arg=0x60000fffffffb2a0) at
  [rtld.c:382](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/rtld.c;h=52160dfde6be42eba000c4e8136de0d190617270;hb=HEAD#l382)
- #2
  [0x2000000800001a50](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/ia64/dl-machine.h;h=57d761e48732cfa5de370e7022eef83ae74a9c65;hb=HEAD#l186)
  in [\_start
  ()](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/ia64/dl-machine.h;h=57d761e48732cfa5de370e7022eef83ae74a9c65;hb=HEAD#l159)

It is easy to trace the whole chain from the very `_start` (every
dynamically linked program starts like that on `ia64`):

- `_start` (`ld.so` entry point) where very little happens:
  - the module-base register `gp` (also known as `r1`) is being
    computed
  - control is passed to `_dl_start()`
- `_dl_start` is `C` code entry point where `ld.so` own `ELF`
  header is being parsed:
  - `bootstrap_map` (dynamic linker context) is being initialized as:

    ``` c
    static ElfW(Addr) _dl_start (void *arg) {
        ...
        #include "dynamic-link.h" /* includes defintion of elf_get_dynamic_info() */
        ...
        __builtin_memset (bootstrap_map.l_info, '\0', sizeof (bootstrap_map.l_info));
        bootstrap_map.l_addr = elf_machine_load_address ();
        bootstrap_map.l_ld = (void *) bootstrap_map.l_addr + elf_machine_dynamic ();

        elf_get_dynamic_info (&bootstrap_map, NULL); /* crash happens here */
        ...
    }
    ```

Thus the `elf_get_dynamic_info()` is a local inline function:

``` c
auto inline void __attribute__ ((unused, always_inline))
elf_get_dynamic_info (struct link_map *l, ElfW(Dyn) *temp) { ...
```

That's it. The reason of unreadable `_dl_start` is excessive
inlining.

I undid the inline damage to make disassembly slightly more readable.
Basically changed `inline` to `noiline` and moved out exact code bit
that crashed to yet another `noinline` function
`elf_get_dynamic_info_addr_tag`:

``` diff
--- ../glibc-2.22/elf/get-dynamic-info.h.orig   2015-12-27 12:29:22.468333779 +0000
+++ ../glibc-2.22/elf/get-dynamic-info.h        2015-12-27 12:33:43.124279949 +0000
@@ -1,100 +1,113 @@
 #include <assert.h>
 #include <libc-internal.h>

+#ifndef RESOLVE_MAP
+static
+#else
+auto
+#endif
+void __attribute__ ((unused, noinline))
+elf_get_dynamic_info_addr_tag (struct link_map *l, ElfW(Dyn) *dyn)
+{
+  ElfW(Dyn) **info = l->l_info;
+
+  info[DT_ADDRTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
+       + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
+}
+
 #ifndef RESOLVE_MAP
 static
 #else
 auto
 #endif
-inline void __attribute__ ((unused, always_inline))
+void __attribute__ ((unused, noinline))
 elf_get_dynamic_info (struct link_map *l, ElfW(Dyn) *temp)
 {
   ElfW(Dyn) *dyn = l->l_ld;
   ElfW(Dyn) **info;
 #if __ELF_NATIVE_CLASS == 32
   typedef Elf32_Word d_tag_utype;
 #elif __ELF_NATIVE_CLASS == 64
   typedef Elf64_Xword d_tag_utype;
 #endif

 #ifndef RTLD_BOOTSTRAP
   if (dyn == NULL)
     return;
 #endif

   info = l->l_info;

   while (dyn->d_tag != DT_NULL)
     {
       if ((d_tag_utype) dyn->d_tag < DT_NUM)
        info[dyn->d_tag] = dyn;
       else if (dyn->d_tag >= DT_LOPROC &&
               dyn->d_tag < DT_LOPROC + DT_THISPROCNUM)
        {
          /* This does not violate the array bounds of l->l_info, but
             gcc 4.6 on sparc somehow does not see this.  */
          DIAG_PUSH_NEEDS_COMMENT;
          DIAG_IGNORE_NEEDS_COMMENT (4.6,
                                     "-Warray-bounds");
          info[dyn->d_tag - DT_LOPROC + DT_NUM] = dyn;
          DIAG_POP_NEEDS_COMMENT;
        }
       else if ((d_tag_utype) DT_VERSIONTAGIDX (dyn->d_tag) < DT_VERSIONTAGNUM)
        info[VERSYMIDX (dyn->d_tag)] = dyn;
       else if ((d_tag_utype) DT_EXTRATAGIDX (dyn->d_tag) < DT_EXTRANUM)
        info[DT_EXTRATAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
             + DT_VERSIONTAGNUM] = dyn;
       else if ((d_tag_utype) DT_VALTAGIDX (dyn->d_tag) < DT_VALNUM)
        info[DT_VALTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
             + DT_VERSIONTAGNUM + DT_EXTRANUM] = dyn;
       else if ((d_tag_utype) DT_ADDRTAGIDX (dyn->d_tag) < DT_ADDRNUM)
-       info[DT_ADDRTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
-            + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
+       elf_get_dynamic_info_addr_tag (l, dyn);
       ++dyn;
     }
```

That way I've got the following crash dump:

    # gdb -q --args elf/ld.so --library-path ../image/lib /usr/bin/cal
    Reading symbols from elf/ld.so...done.
    (gdb) run
    Starting program: /var/tmp/portage/sys-libs/glibc-2.22-r1/work/build-ia64-ia64-unknown-linux-gnu-nptl/elf/ld.so --library-path ../image/lib /usr/bin/cal
    Failed to read a valid object file image from memory.

    Program received signal SIGSEGV, Segmentation fault.
    0x200000080000a8b0 in elf_get_dynamic_info_addr_tag (dyn=0x200000080004e4b0, l=0x2000000800053178 <_rtld_local+2456>)
        at get-dynamic-info.h:33
    33               + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
    (gdb) bt
    #0  0x200000080000a8b0 in elf_get_dynamic_info_addr_tag (dyn=0x200000080004e4b0, l=0x2000000800053178 <_rtld_local+2456>)
        at get-dynamic-info.h:33
    #1  0x200000080000ade0 in elf_get_dynamic_info (temp=0x0, l=0x2000000800053178 <_rtld_local+2456>) at get-dynamic-info.h:83
    #2  0x200000080000afe0 in _dl_start (arg=0x60000fffffffb2a0) at rtld.c:382
    #3  0x2000000800001a50 in _start ()
    (gdb) disassemble 
    Dump of assembler code for function elf_get_dynamic_info_addr_tag:
       0x200000080000a880 <+0>:     [MMI]       ld8 r14=[r32];;
       0x200000080000a881 <+1>:                 shladd r15=r14,3,r0
       0x200000080000a882 <+2>:                 addl r14=163120,r1;;
       0x200000080000a890 <+16>:    [MMI]       ld8 r14=[r14];;
       0x200000080000a891 <+17>:                adds r14=992,r14
       0x200000080000a892 <+18>:                nop.i 0x0;;
       0x200000080000a8a0 <+32>:    [MMI]       nop.m 0x0
       0x200000080000a8a1 <+33>:                sub r14=r14,r15
       0x200000080000a8a2 <+34>:                nop.i 0x0;;
    => 0x200000080000a8b0 <+48>:    [MIB]       st8 [r14]=r32
       0x200000080000a8b1 <+49>:                nop.i 0x0
       0x200000080000a8b2 <+50>:                br.ret.sptk.many b0;;
    End of assembler dump.

12 instructions (4 of which are `nop` of sorts) is more manageable.

More readable but still is completely unclear. `r32` is the only used
input register here (`r33` would be the second) while
`elf_get_dynamic_info_addr_tag()` clearly has two arguments:

``` c
void __attribute__ ((unused, noinline))
elf_get_dynamic_info_addr_tag (struct link_map *l, ElfW(Dyn) *dyn)
```

At this point I started looking at what exactly crashing code is
supposed to do.

## the first workaround

`_rtld_local` is a whole linker context ([defined
here](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/rtld.c;h=52160dfde6be42eba000c4e8136de0d190617270;hb=HEAD#l146))
of type [`struct
rtld_global`](https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/generic/ldsodefs.h;h=a8caf47bc378d3ec1ba2e4cd0219503faeeb5e76;hb=HEAD#l268)
(slightly simplified):

``` c
struct rtld_global
{
    struct link_namespaces
    {
    ....
    } _dl_ns[DL_NNS];
    ...
    struct link_map _dl_rtld_map;
    ...
};
...
struct link_map
{
  ElfW(Addr) l_addr;          /* Difference between the address in the ELF
                                 file and the addresses in memory.  */
  char *l_name;               /* Absolute file name object was found in.  */
  ElfW(Dyn) *l_ld;            /* Dynamic section of the shared object.  */
  struct link_map *l_next, *l_prev; /* Chain of loaded objects.  */
  ...
  ElfW(Dyn) *l_info[DT_NUM + DT_THISPROCNUM + DT_VERSIONTAGNUM
                    + DT_EXTRANUM + DT_VALNUM + DT_ADDRNUM];
  ...
}
```

The code crashed when tried to fill in
`_rtld_local._dl_rtld_map.l_info` global variable. `ld.so` even
succeeded at previous step (inspecting values right after crash):

    (gdb) print _rtld_local._dl_rtld_map.l_info 
    $4 = {0x0 <repeats 14 times>, 0x200000080004e4a0, 0x0 <repeats 62 times>}
    (gdb) print _rtld_local._dl_rtld_map.l_info[14]->d_tag
    $3 = 14 # DT_SONAME

but was not able to handle current section type:

    (gdb) (gdb) print dyn->d_tag 
    $5 = 1879047925 # 0x6ffffef5

Looking at `/usr/include/elf.h` it's a section of [`DT_GNU_HASH`
type](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/elf.h;h=fbadda4377aae9ede65ac2fcc795fbbd79d1a78c;hb=HEAD#l767).

What kinds of dynamic sections does `ld.so` have?

    # readelf -d elf/ld.so
    Dynamic section at offset 0x3e4a0 contains 21 entries:
      Tag        Type                         Name/Value
     0x000000000000000e (SONAME)             Library soname: [ld-linux-ia64.so.2]
     0x000000006ffffef5 (GNU_HASH)           0x190
     0x0000000000000005 (STRTAB)             0x8d0
     0x0000000000000006 (SYMTAB)             0x318
     0x000000000000000a (STRSZ)              952 (bytes)
     0x000000000000000b (SYMENT)             24 (bytes)
     0x0000000070000000 (IA_64_PLT_RESERVE)  0x52660 -- 0x52678
     0x0000000000000003 (PLTGOT)             0x2a980
     0x0000000000000002 (PLTRELSZ)           120 (bytes)
     0x0000000000000014 (PLTREL)             RELA
     0x0000000000000017 (JMPREL)             0x1278
     0x0000000000000007 (RELA)               0xdb0
     0x0000000000000008 (RELASZ)             1224 (bytes)
     0x0000000000000009 (RELAENT)            24 (bytes)
     0x000000006ffffffc (VERDEF)             0xd08
     0x000000006ffffffd (VERDEFNUM)          5
     0x000000000000001e (FLAGS)              BIND_NOW
     0x000000006ffffffb (FLAGS_1)            Flags: NOW
     0x000000006ffffff0 (VERSYM)             0xc88
     0x000000006ffffff9 (RELACOUNT)          17
     0x0000000000000000 (NULL)               0x0

`ld.so` managed to load `SONAME` (first) section and failed at
`GNU_HASH` (second). What if we drop `GHU_HASH` from `ld.so`
image? Tried to relink it with default `sysv` hash style.
The default command to link `ld.so` is:

    # ia64-unknown-linux-gnu-gcc \
        -Wl,-O1 -Wl,--as-needed \
        \
        -Wl,--hash-style=gnu \
        \
        -nostdlib -nostartfiles \
        -shared \
        \
        -o elf/ld.so.new \
        \
        -Wl,-z,combreloc -Wl,-z,relro -Wl,-z,defs -Wl,-z,now \
        elf/librtld.os \
        -Wl,--version-script=ld.map \
        -Wl,-soname=ld-linux-ia64.so.2 \
        -Wl,-defsym=_begin=0

I changed `-Wl,--hash-style=gnu` to `-Wl,--hash-style=sysv`:

    # ia64-unknown-linux-gnu-gcc \
        -Wl,-O1 -Wl,--as-needed \
        \
        -Wl,--hash-style=sysv \
        \
        -nostdlib -nostartfiles \
        -shared \
        \
        -o elf/ld.so.new \
        \
        -Wl,-z,combreloc -Wl,-z,relro -Wl,-z,defs -Wl,-z,now \
        elf/librtld.os \
        -Wl,--version-script=ld.map \
        -Wl,-soname=ld-linux-ia64.so.2 \
        -Wl,-defsym=_begin=0

And behold! Resulting `ld.so` can load simple binaries:

    # elf/ld.so.new --library-path ../image/lib /usr/bin/cal
        December 2015
    Su Mo Tu We Th Fr Sa
           1  2  3  4  5
     6  7  8  9 10 11 12
    13 14 15 16 17 18 19
    20 21 22 23 24 25 26
    27 28 29 30 31

    # readelf -d /usr/bin/cal | grep GNU_HASH
      0x000000006ffffef5 (GNU_HASH)           0x4000000000000270

Even if these binaries contain `GNU_HASH` themselves. That is
unexpected.

Thus the first workaround to get working `glibc` working on `ia64`
is by tweaking `LDFLAGS`:

    LDFLAGS="-Wl,--hash-style=sysv" emerge -av sys-libs/glibc

# down the rabbit hole

But why does `SIGSEGV` happen in the first place? Who is at fault
here? The primary suspects are `gcc`, `glibc` and `binutils`.

What is the difference between `SONAME` and `GNU_HASH` sections? All
the faulty code does is storing pointer to `ElfW(Dyn)`:

```c
void __attribute__ ((unused, noinline))
elf_get_dynamic_info_addr_tag (struct link_map *l, ElfW(Dyn) *dyn)
{
  ElfW(Dyn) **info = l->l_info;

  info[DT_ADDRTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
       + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
}
```

I spent some time trying to write simple example to reproduce the crash.
No matter how hard I tried all samples work just fine. The best
approximation is:

``` c
#include <stdio.h>

struct dyn_t {
    long d_tag;
    long foo;
};

struct g_t {
    long b[307];
    long a[8];
    struct dyn_t *p[77];
};

struct g_t _l __attribute__ ((visibility ("hidden"), section (".sdata")));

static void __attribute__ ((unused, noinline))
f(struct g_t * g, struct dyn_t * dyn)
{
  struct dyn_t **info = g->p;

  info[(0x6ffffeff - dyn->d_tag) + 66] = dyn;
}

void __attribute__ ((unused, noinline))
foo (struct dyn_t * dyn)
{
  f(&_l, dyn);
}

int __attribute__ ((noinline))
main(int argc) {
    struct dyn_t d = { argc + 66 };

    foo (&d);

    return _l.p[0] != 0;
}
```

When built as `-O2 -fPIC` it generates very similar `asm` for function
`f` (immediate around `addl r14` is the only deviation):

``` asm
$ objdump -r -d a
...
<f.constprop.0>:
       0b 70 00 40 18 10       [MMI]       ld8 r14=[r32];;     # read dyn->d_tag
       f0 70 00 24 40 c0                   shladd r15=r14,3,r0 # multiply dyn->d_tag by 8
       c1 ed d7 9f                         addl r14=-1316,r1;; # ?
       0b 70 00 1c 18 10       [MMI]       ld8 r14=[r14];;     # ??
       e0 00 3b 0e 42 00                   adds r14=992,r14    # ???
       00 00 04 00                         nop.i 0x0;;
       09 00 00 00 01 00       [MMI]       nop.m 0x0
       e0 70 3c 0a 40 00                   sub r14=r14,r15     # compute pointer in info[]
       00 00 04 00                         nop.i 0x0;;
       11 00 80 1c 98 11       [MIB]       st8 [r14]=r32       # perform store
       00 00 00 02 00 80                   nop.i 0x0
       08 00 84 00                         br.ret.sptk.many b0;;
...
```

`gcc` dump is slightly more readable:

``` asm
$ ia64-unknown-linux-gnu-gcc -O2 -S a.c -o a.S
...
f.constprop.0:
    .prologue
    .body
    .mmi
    ld8 r14 = [r32]
    ;;
    shladd r15 = r14, 3, r0
    addl r14 = @ltoffx(_l#+15032385536), r1
    ;;
    .mmi
    ld8.mov r14 = [r14], _l#+15032385536
    ;;
    adds r14 = 992, r14
    nop 0
    ;;
    .mmi
    nop 0
    sub r14 = r14, r15
    nop 0
    ;;
    .mib
    st8 [r14] = r32
    nop 0
    br.ret.sptk.many rp
.endp f.constprop.0#
```

At line `adds r14 = 992, r14` register `r14` should contain absolute
address of `_l#+15032385536+992`. But where that huge number comes
from, why we don't see it in `objdump`?

The magic is in `ld8.mov r14 = [r14], _l#+15032385536` line. It
instructs assembly to load this (absolute) address somewhere from
`ltoffx(_l#+15032385536) + r1` (aka `-1316 + r1`).

I returned to broken `ld.so` and checked how this same relocation
looks:

    # gdb -q elf/ld.so
    Reading symbols from elf/ld.so...done.
    (gdb) run
    Starting program: /var/tmp/portage/sys-libs/glibc-2.22-r1/work/build-ia64-ia64-unknown-linux-gnu-nptl/elf/ld.so 
    Failed to read a valid object file image from memory.

    Program received signal SIGSEGV, Segmentation fault.
        0x200000080000a8b0 in elf_get_dynamic_info_addr_tag (dyn=0x200000080004e4b0, l=0x2000000800053178 <_rtld_local+2456>)
        at get-dynamic-info.h:33
    33               + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
    (gdb) disassemble 
    Dump of assembler code for function elf_get_dynamic_info_addr_tag:
       0x200000080000a880 <+0>:     [MMI]       ld8 r14=[r32];;
       0x200000080000a881 <+1>:                 shladd r15=r14,3,r0
       0x200000080000a882 <+2>:                 addl r14=163120,r1;;
       0x200000080000a890 <+16>:    [MMI]       ld8 r14=[r14];;
       0x200000080000a891 <+17>:                adds r14=992,r14
       0x200000080000a892 <+18>:                nop.i 0x0;;
       0x200000080000a8a0 <+32>:    [MMI]       nop.m 0x0
       0x200000080000a8a1 <+33>:                sub r14=r14,r15
       0x200000080000a8a2 <+34>:                nop.i 0x0;;
    => 0x200000080000a8b0 <+48>:    [MIB]       st8 [r14]=r32
       0x200000080000a8b1 <+49>:                nop.i 0x0
       0x200000080000a8b2 <+50>:                br.ret.sptk.many b0;;
    (gdb) print *(void**)(163120+$r1)
    $1 = (void *) 0x3800527e0
    (gdb) quit

    # readelf -r elf/ld.so | fgrep 3800527e0
    0000000526b0  00000000006f R_IA64_REL64LSB                      3800527e0

    # objdump -R elf/ld.so | fgrep 3800527e0
    00000000000526b0 REL64LSB          *ABS*+0x00000003800527e0

This actually is an absolute relocation (contains absolute address). I
did not expect such things to be present in `PIC` mode. Such
relocations work for normal binaries but don't for `ld.so` for a
simple reason: `ld.so` did not adjust any relocations yet.

`ld.so` own section info is read at
[rtld.c:382](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/rtld.c;h=52160dfde6be42eba000c4e8136de0d190617270;hb=HEAD#l382)
and relocations are applied later at
[rtld.c:397](https://sourceware.org/git/?p=glibc.git;a=blob;f=elf/rtld.c;h=52160dfde6be42eba000c4e8136de0d190617270;hb=HEAD#l397).

`SONAME` section does not use `R_IA64_REL64LSB` relocation while
`GNU_HASH` does. It explains the crash but does not explain why
generated code is different.

## the cause

The answer is in the method how `gcc` optimises the following code:

``` c
elf_get_dynamic_info_addr_tag (&_rtld_local._dl_rtld_map, NULL);
```

Here is a lot of constants to expand:

``` c
void __attribute__ ((unused, noinline))
elf_get_dynamic_info_addr_tag (struct link_map *l, ElfW(Dyn) *dyn)
{
  ElfW(Dyn) **info = l->l_info;

  info[DT_ADDRTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
       + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM] = dyn;
  // or
  // info[(0x6ffffeff - dyn->d_tag) + 66] = dyn;
}
```

`gcc` infers that `elf_get_dynamic_info_addr_tag()` gets a constant
`_rtld_local._dl_rtld_map` (aka `_rtld_local+2456`) as it's
first argument `l` and specialises function into 1-argument variant:

``` c
void __attribute__ ((unused, noinline))
elf_get_dynamic_info_addr_tag_constprop (ElfW(Dyn) *dyn)
{
  // p is of type __attribute__ ((section (".sdata")))
  static const ElfW(Dyn) ** p = &_rtld_local._dl_rtld_map.l_info[0x6ffffeff + 66];

  p[-dyn->d_tag] = dyn;
}
```

To compile that code `gcc` infers the following facts about `p`:

- offset from `_rtld_local` to `p` exceeds 22-bit value (limit of
  `addl <imm22>, gp` instruction): `(0x6ffffeff + 66) * 8 + 2520 =
  0x3800003e0 = 0x380000000 + 992`
- `gcc` decides to push `p` address to `.got` and load it from
  there

The workaround to avoid `.got` reload is simple (but fragile): force
`gcc` to compute final offset first:

``` c
void __attribute__ ((unused, noinline))
elf_get_dynamic_info_addr_tag (struct link_map *l, ElfW(Dyn) *dyn)
{
  ElfW(Dyn) **info = l->l_info;

  long o = DT_ADDRTAGIDX (dyn->d_tag) + DT_NUM + DT_THISPROCNUM
         + DT_VERSIONTAGNUM + DT_EXTRANUM + DT_VALNUM;

  info[o] = dyn;
}
```

I've tried it on a toy example first. Hack made `gcc` avoid
`ltoffx` and use small `gprel` offset:

``` asm
f.constprop.0:
    .prologue
    .body
    .mlx
    ld8 r15 = [r32]
    movl r14 = 1879048001
    ;;
    .mii
    sub r14 = r14, r15
    addl r15 = @gprel(_l#+2520), gp
    ;;
    shladd r14 = r14, 3, r15
    ;;
    .mib
    st8 [r14] = r32
    nop 0
    br.ret.sptk.many rp
```

or the same in final binary:

``` asm
<f.constprop.0>:
   05 78 00 40 18 d0       [MLX]       ld8 r15=[r32]
   6f 00 00 00 00 c0                   movl r14=0x6fffff41;;
   11 f4 fb 67 
   03 70 38 1e 05 20       [MII]       sub r14=r14,r15
   f0 60 07 12 48 c0                   addl r15=1260,r1;;
   e1 78 48 80                         shladd r14=r14,3,r15;;
   11 00 80 1c 98 11       [MIB]       st8 [r14]=r32
   00 00 00 02 00 80                   nop.i 0x0
   08 00 84 00                         br.ret.sptk.many b0;;
   08 00 00 00 01 00       [MMI]       nop.m 0x0
   00 00 00 02 00 00                   nop.m 0x0
   00 00 04 00                         nop.i 0x0
```

No memory loads, only a single store \\o/.

A workaround is sent to `libc`-alpha ML [for
review](https://sourceware.org/ml/libc-alpha/2015-12/msg00556.html).

## wrapping up

Random observations:

- `LDFLAGS=-Wl,--hash-style=sysv` is a simple way to get modern
  `glibc` work on `ia64`
- uninlining things did not make bug disappear
- dynamic linkers are simple yet delicate at bootstrap phase when no
  relocations are adjusted
- bug does not happen on `-O1` optimization level (triggered by
  `-fipa-cp` knob)
- the workaround is weak and can break at any time in future
- perhaps `gcc` could be smarter to use `MLX` instruction to embed
  large offset:

  ``` asm
  movl r32 = @gprel(_l#+15032385536)
  add  r32 = r32, gp
  ```

  But separate section for `_l` makes things more complicated.
- it took me a day to get `-Wl,--hash-style=sysv` workaround and 6
  days to figure out why it works
- `ld.so` uses `ELF` relocations extensively and sets them up in C
  code
- `gdb` output is misleading for specialised functions (the argument
  order is flipped)

Have fun!
