---
title: "tracking down mysterious memory corruption"
date: July 14, 2018
---

:PostID: 209
:Title: "tracking down mysterious memory corruption"
:Keywords: memory, RAM, dual channel, sse, avx, non-temporal instructions
:Categories: notes

I've bought my current desktop machine around 2011 (7 years ago)
and mostly had no problems with it save one exception: occasionally
(once 2-3 months) firefox, liferea or gcc would mysteriously crash.

Bad PTE
-------

**dmesg** reports would claim that page table entries refer to already
freed physical memory:

.. code-block::

    Apr 24 03:59:17 sf kernel: BUG: Bad page map in process cc1  pte:200000000 pmd:2f9d0d067
    Apr 24 03:59:17 sf kernel: addr:00000000711a7136 vm_flags:00000875 anon_vma:          (null) mapping:000000003882992c index:101a
    Apr 24 03:59:17 sf kernel: file:cc1 fault:filemap_fault mmap:btrfs_file_mmap readpage:btrfs_readpage
    Apr 24 03:59:18 sf kernel: CPU: 1 PID: 14834 Comm: cc1 Tainted: G         C        4.17.0-rc1-00215-g5e7c7806111a #65
    Apr 24 03:59:18 sf kernel: Hardware name: Gigabyte Technology Co., Ltd. To be filled by O.E.M./H77M-D3H, BIOS F4 02/16/2012
    Apr 24 03:59:18 sf kernel: Call Trace:
    Apr 24 03:59:18 sf kernel:  dump_stack+0x46/0x5b
    Apr 24 03:59:18 sf kernel:  print_bad_pte+0x193/0x230
    Apr 24 03:59:18 sf kernel:  ? page_remove_rmap+0x216/0x330
    Apr 24 03:59:18 sf kernel:  unmap_page_range+0x3f7/0x920
    Apr 24 03:59:18 sf kernel:  unmap_vmas+0x47/0xa0
    Apr 24 03:59:18 sf kernel:  exit_mmap+0x86/0x170
    Apr 24 03:59:18 sf kernel:  mmput+0x64/0x120
    Apr 24 03:59:18 sf kernel:  do_exit+0x2a9/0xb90
    Apr 24 03:59:18 sf kernel:  ? syscall_trace_enter+0x16d/0x2c0
    Apr 24 03:59:18 sf kernel:  do_group_exit+0x2e/0xa0
    Apr 24 03:59:18 sf kernel:  __x64_sys_exit_group+0xf/0x10
    Apr 24 03:59:18 sf kernel:  do_syscall_64+0x4a/0xe0
    Apr 24 03:59:18 sf kernel:  entry_SYSCALL_64_after_hwframe+0x44/0xa9
    Apr 24 03:59:18 sf kernel: RIP: 0033:0x7f7a039dcb96
    Apr 24 03:59:18 sf kernel: RSP: 002b:00007fffdfa09d08 EFLAGS: 00000246 ORIG_RAX: 00000000000000e7
    Apr 24 03:59:18 sf kernel: RAX: ffffffffffffffda RBX: 00007f7a03ccc740 RCX: 00007f7a039dcb96
    Apr 24 03:59:18 sf kernel: RDX: 0000000000000000 RSI: 000000000000003c RDI: 0000000000000000
    Apr 24 03:59:18 sf kernel: RBP: 0000000000000000 R08: 00000000000000e7 R09: fffffffffffffe70
    Apr 24 03:59:18 sf kernel: R10: 0000000000000008 R11: 0000000000000246 R12: 00007f7a03ccc740
    Apr 24 03:59:18 sf kernel: R13: 0000000000000038 R14: 00007f7a03cd5608 R15: 0000000000000000
    Apr 24 03:59:18 sf kernel: Disabling lock debugging due to kernel taint
    Apr 24 03:59:18 sf kernel: BUG: Bad rss-counter state mm:000000004fac8a77 idx:2 val:-1

It's not something that is easy to debug or reproduce.

`**Transparent Hugepages** <https://lwn.net/Articles/359158/>`_
were a new thing at that time and I was using it systemwide via
**CONFIG_TRANSPARENT_HUGEPAGE_ALWAYS=y** kernel option.

After those crashes I decided to switch it back to
**CONFIG_TRANSPARENT_HUGEPAGE_MADVISE=y** only. Crashes became more
rare: once in a 5-6 months.

Enabling more debugging facilities in the kernel did not change anything
and I moved on.

A few years later I set up nightly builds on this machine to build and
test packages in an automatic way. Things were running smoothly except
for a few memory-hungry tests that crashed once in a while: **firefox**,
**rust** and **webkit** builds every other night hit internal compiler
errors in **gcc**.

Crashes were very hard to isolate or reproduce: every time **SIGSEGV** 
happened on a new source file being compiled. I tried to run the same failed gcc
command in a loop for hours to try to reproduce the crash but never succeeded.
It is usually a strong sign of flaky hardware. At that point I tried **memtest86+-5.01**
and **memtester** tools to validate **RAM** chips. Tools claimed **RAM**
to be fine.
My conclusion was that crashes are the result of an obscure software problem causing
memory corruption (probably in the kernel). I had no idea how to debug that and
kept on using this system. For day-to-day use it was perfectly stable.

A new clue
----------

[years later]

Last year I joined Gentoo's toolchain@ project and started caring a bit more
about **glibc** and **gcc**. dilfridge@ did a fantastic job on making
**glibc** testsuite  work on amd64 (and also many other things not directly
related to this post).

One day I made a `major change <https://bugs.gentoo.org/657760>`_ in how
**CFLAGS** are handled in **glibc** ebuild and broke a few users with
**CFLAGS=-mno-sse4.2**.
That day I ran **glibc** testsuite to check if I made things worse.
There was only one test failing: **string/test-memmove**.

Of all the obscure things that **glibc** checks for only one simple
**memmove()** test refused to work!

The failure occured only on 32-bit version of **glibc** and looked like
this:

.. code-block::

    $ elf/ld.so --inhibit-cache --library-path . string/test-memmove
    simple_memmove  __memmove_ssse3_rep     __memmove_ssse3 __memmove_sse2_unaligned        __memmove_ia32
    string/test-memmove: Wrong result in function __memmove_sse2_unaligned dst "0x70000084" src "0x70000000" offset "43297733"

This command runs **string/test-memmove** binary using **./libc.so.6** and
**elf/ld.so** as a loader.

The good thing is that I was somewhat able to reproduce the failure:
every few
runs the error popped up. Test was not failing deterministically. Every
time test failed it was always **__memmove_sse2_unaligned** but
**offset** was different.

Here is the `test source code <https://sourceware.org/git/?p=glibc.git;a=blob;f=string/test-memmove.c;h=64e3651ba40604e47ddf6d633f4d0aea4644f60a;hb=HEAD>`_.
The test basically runs **memmove()** and checks if all memory was moved
as expected. Originally test was written to check how **memmove()**
handles memory ranges that span signed/unsigned address boundary around
address **0x80000000**. Hence the unusual **mmap(addr=0x70000000, size=0x20000000)**
as a way to allocate memory.

Now the fun thing: the error disappeared as soon as I rebooted the
machine.
And came back one day later (after the usual nightly tests run). To
explore
the breakage and make a fix I had to find a faster way to
reproduce the failure.

At that point the fastest way to make the test fail again was to run
firefox build process first. It took "only" 40 minutes to get the
machine in a state when I could reproduce the failure.

Once in that state I started shrinking down `**__memmove_sse2_unaligned** implementation <https://sourceware.org/git/?p=glibc.git;a=blob;f=sysdeps/i386/i686/multiarch/memcpy-sse2-unaligned.S;h=9aa17de99c9c3415a9b5ac28fd9f1eb4457f916d;hb=HEAD#l1>`_
to check where exactly data gets transferred incorrectly. 600 lines of
straightforward code is not that much.

.. code-block:: asm

    ; check if the copied block is smaller than cache size
    167         cmp     __x86_shared_cache_size_half, %edi 
    ...
    170         jae     L(mm_large_page_loop_backward)
    ...
    173 L(mm_main_loop_backward): ; small block, normal instruction
    175         prefetcht0 -128(%eax)
    ...
    ; load 128 bits from source buffer
    177         movdqu  -64(%eax), %xmm0
    ...
    ; store 128 bits to destination buffer
    181         movaps  %xmm0, -64(%ecx)
    ...
    244 L(mm_large_page_loop_backward):
    ...
    ; load 128 bits from source buffer
    245         movdqu  -64(%eax), %xmm0
    ...
    ; store 128 bits to destination avoiding cache
    249         movntdq %xmm0, -64(%ecx)

Note: **memcpy()**'s behaviour depends on CPU cache size. When the block
of copied memory is small (less than CPU cache size, **8MB** in my case)
**memcpy()** does not do anything special. Otherwise **memcpy()** tries
to avoid cache pollution and uses **non-temporal** variant of store
instruction: **movntdq** instead of usual **movaps**.

While I was poking at this code I found a reliable workaround to make
**memcpy()** never fail on my machine: change **movntdq** to **movdqa**:

.. code-block:: diff

    --- a/sysdeps/i386/i686/multiarch/memcpy-sse2-unaligned.S
    +++ b/sysdeps/i386/i686/multiarch/memcpy-sse2-unaligned.S
    @@ -26,0 +27 @@
    +#define movntdq movdqa /* broken CPU? */

I was pondering if I should patch **binutils** locally to avoid
**movntdq** instruction entirely but eventually discarded it and
focused on finding the broken component instead. Who knows what
else can be there.

I was so close!

A minimal reproducer
--------------------

I attempted to craft a testcase that does not depend on **glibc**'s
**memcpy()** and got this:

.. code-block:: c

    #include <emmintrin.h> /* movdqu, sfence, movntdq */
    
    static void memmove_si128u (__m128i_u * dest, __m128i_u const *src, size_t items)
    {
        dest += items - 1;
        src  += items - 1;
        _mm_sfence();
        for (; items != 0; items-=1, dest-=1, src-=1)
        {
            __m128i xmm0 = _mm_loadu_si128(src); // movdqu
            if (0)
            {
              // this would work:
              _mm_storeu_si128(dest, xmm0);// movdqu
            }
            else
            {
              // this causes single bit memory corruption
              _mm_stream_si128(dest, xmm0); // movntdq
            }
        }
        _mm_sfence();
    }

This code assumes quite a few things from the caller:

- **dest > src** as copying happens right-to-left
- **dest** has to be 16-byte aligned
- block size must be a multiple of 16-bytes.

Here is what C code compiles to with **-O2 -m32 -msse2**:

.. code-block:: asm

    (gdb) disassemble memmove_si128u
    Dump of assembler code for function memmove_si128u(__m128i_u*, __m128i_u const*, size_t):
       0x000008f0 <+0>:     push   %ebx
       0x000008f1 <+1>:     lea    0xfffffff(%ecx),%ebx
       0x000008f7 <+7>:     shl    $0x4,%ebx
       0x000008fa <+10>:    add    %ebx,%eax
       0x000008fc <+12>:    add    %ebx,%edx
       0x000008fe <+14>:    sfence 
       0x00000901 <+17>:    test   %ecx,%ecx
       0x00000903 <+19>:    je     0x923 <memmove_si128u(__m128i_u*, __m128i_u const*, size_t)+51>
       0x00000905 <+21>:    shl    $0x4,%ecx
       0x00000908 <+24>:    mov    %eax,%ebx
       0x0000090a <+26>:    sub    %ecx,%ebx
       0x0000090c <+28>:    mov    %ebx,%ecx
       0x0000090e <+30>:    xchg   %ax,%ax
       0x00000910 <+32>:    movdqu (%edx),%xmm0
       0x00000914 <+36>:    sub    $0x10,%eax
       0x00000917 <+39>:    sub    $0x10,%edx
       0x0000091a <+42>:    movntdq %xmm0,0x10(%eax)
       0x0000091f <+47>:    cmp    %eax,%ecx
       0x00000921 <+49>:    jne    0x910 <memmove_si128u(__m128i_u*, __m128i_u const*, size_t)+32>
       0x00000923 <+51>:    sfence 
       0x00000926 <+54>:    pop    %ebx
       0x00000927 <+55>:    ret

And with **-O2 -m64 -mavx2**:

.. code-block:: asm

    (gdb) disassemble memmove_si128u
    Dump of assembler code for function memmove_si128u(__m128i_u*, __m128i_u const*, size_t):
       0x0000000000000ae0 <+0>:     sfence 
       0x0000000000000ae3 <+3>:     mov    %rdx,%rax
       0x0000000000000ae6 <+6>:     shl    $0x4,%rax
       0x0000000000000aea <+10>:    sub    $0x10,%rax
       0x0000000000000aee <+14>:    add    %rax,%rdi
       0x0000000000000af1 <+17>:    add    %rax,%rsi
       0x0000000000000af4 <+20>:    test   %rdx,%rdx
       0x0000000000000af7 <+23>:    je     0xb1e <memmove_si128u(__m128i_u*, __m128i_u const*, size_t)+62>
       0x0000000000000af9 <+25>:    shl    $0x4,%rdx
       0x0000000000000afd <+29>:    mov    %rdi,%rax
       0x0000000000000b00 <+32>:    sub    %rdx,%rax
       0x0000000000000b03 <+35>:    nopl   0x0(%rax,%rax,1)
       0x0000000000000b08 <+40>:    vmovdqu (%rsi),%xmm0
       0x0000000000000b0c <+44>:    sub    $0x10,%rdi
       0x0000000000000b10 <+48>:    sub    $0x10,%rsi
       0x0000000000000b14 <+52>:    vmovntdq %xmm0,0x10(%rdi)
       0x0000000000000b19 <+57>:    cmp    %rdi,%rax
       0x0000000000000b1c <+60>:    jne    0xb08 <memmove_si128u(__m128i_u*, __m128i_u const*, size_t)+40>
       0x0000000000000b1e <+62>:    sfence 
       0x0000000000000b21 <+65>:    retq

Surprisingly (or not so surprisingly) both **-m32**/**-m64** tests started
failing on my machine.

It was always second bit of a 128-bit value that was corrupted.

On 128MB blocks this test usually caused one incorrect bit to be copied
once in a few runs. I tried to run exactly the same test on other
hardware I have access to. None of it failed.

I started to suspect the kernel to corrupt **SSE** cpu context on
context switch. But why only **non-temporal** instruction is affected?
And why only a single bit and not a full 128-bit chunk? Could it be that
the kernel forgot to issue **mfence** on context switch and all in-flight
**non-temporal** instructions stored garbage? That would be a sad race
condition. But the single bit flip did not line up with it.

Sounds more like kernel would arbitrarily flip one bit in userspace. But
why only when **movntdq** is involved?

I suspected CPU bug and upgraded CPU firmware, switched machine from
**BIOS**-compatible mode to native **UEFI** hoping to fix it. Nope.
Nothing changed. Same failure persisted: single bit corruption after
a heavy load on the machine.

I started thinking on how to speed my test up to avoid firefox
compilation as a trigger.

Back to square one
------------------

My suspect was bad **RAM** again. I modified my test all **RAM** by
allocating 128MB chunks at a time and run **memmove()** on newly
allocated **RAM** to cover all available pages. Test would either
find bad memory or **OOM**-fail.

Full program source is at https://github.com/trofi/xmm-ram-test.

And bingo! It took only 30 seconds to reproduce the failure. The test
usually started reporting the first problem when it got to 17GB of
**RAM** usage.

I have **4x8GB** **DDR3-DIMM**s. I started brute-forcing
various configurations of **DIMM** order on motherboard slots:

.. code-block::

    A      B      A      B
    DIMM-1 -      -      -      : works
    DIMM-2 -      -      -      : works
    DIMM-3 -      -      -      : works
    DIMM-4 -      -      -      : works
    DIMM-1 -      DIMM-3 -      : fails (dual channel mode)
    DIMM-1 DIMM-3 -      -      : works (single channel mode)
    -      DIMM-2 -      DIMM-4 : works (dual channel mode)
    DIMM-3 -      DIMM-1 -      : fails (dual channel mode)
    -      DIMM-3 -      DIMM-1 : fails (dual channel mode)
    -      DIMM-1 -      DIMM-3 : fails (dual channel mode)
    -      DIMM-2 -      DIMM-3 : fails (dual channel mode)

And many other combinations of **DIMM-3** with others.

It was obvious **DIMM-3** did not like team work. I booted from livecd
to double-check it's not my kernel causing all of this. The error was
still there.

I bought and plugged in a new pair of **RAM** modules in place of
**DIMM-1** and **DIMM-3**. And had no mysterious failures since!

Time to flip **CONFIG_TRANSPARENT_HUGEPAGE_ALWAYS=y** back on :)

Speculations and open questions
-------------------------------

It seems that dual-channel mode and cache coherency has something to do
with it. A few thoughs:

1. Single **DDR3-DIMM** can perform only 64-bit wide loads and stores.
2. In dual-channel mode two 64-bit wide stores can happen at a time
   and require presence of two **DIMM**s.
3. **movntdq** stores directly into **RAM**
   possibly evicting existing value from cache. That can cause further
   writeback to **RAM** to free dirty cache line.
4. **movdqa** stores to cache. But eventually
   cache pressure will also trigger store back to RAM in chunks of cache
   line size of Last Line Cache (64-bytes=512-bits for me). Why do we
   not see corruption happening in this case?

It feels like there should be not much difference between
**non-temporal** and normal instructions in terms of size of data being
written at a time over memory bus. What likely changes is access sequence
of physical addresses under two workloads. But I don't know how to look
into it in detail.

Mystery!

Parting words
-------------


- This crash took me 7 years to figure out :)
- Fix didn't require a single line of code :)
- Bad **RAM** happens. Even if **memtest86+-5.01** disagrees.
- As I was running **memtest86+** in **qemu** I found a bunch of
  unrelated bugs in **tianocore** implementation of **UEFI** and
  **memtest86+** gentoo ebuild: hybrid ISO is not recognized as an ISO
  at all. **memtest86+** crashes at statrup for yet unknown reason
  (likely needs to be fixed against newer toolchain).
- **non-temporal** instructions are a thing and have their own
  memory I/O engine.
- C-level wrappers around **SSE** and **AVX** instructions are easy to
  use!

Have fun!
