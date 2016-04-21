---
title: perf on haskell programs
date: April 21, 2016
---

:PostID: 192
:Title: perf on haskell programs
:Keywords: ghc, haskell, perf
:Categories: notes

**GHC** 7.10.1 got a new **-g** option to generate
debugging information in **DWARF** format for compiled
haskell code.

While the **-g** option was being developed I asked Peter a few
times on **#ghc** how **perf top** would look
like for a random haskell program. But his focus
was on **gdb** support.

It's time to actually try it :)

.. raw:: html

   <!--more-->

Let's go!

.. code-block:: asm

    $ perf record -- ghc-7.10.3 --make ... # building highlighting-kate
    $ perf report
    
      10,61%  ghc              libHSrts_thr-ghc7.10.3.so                                        [.] evacuate1
       3,67%  ghc              libHSrts_thr-ghc7.10.3.so                                        [.] c24_info
       2,63%  ghc              libHSrts_thr-ghc7.10.3.so                                        [.] clS_info
       1,83%  ghc              libHSrts_thr-ghc7.10.3.so                                        [.] 0x0000000000045438
       1,22%  ghc              libHSrts_thr-ghc7.10.3.so                                        [.] c7F_info
       1,19%  as               as                                                               [.] hash_lookup.isra.0
       1,12%  ghc              libHSrts_thr-ghc7.10.3.so                                        [.] scavenge_block1
       0,84%  ghc              libHSghc-7.10.3-JQ2WG6oexwDDVprkkcjveL-ghc7.10.3.so              [.] r113_info
       0,72%  as               [kernel.vmlinux]                                                 [k] copy_page
       0,72%  ghc              libHScontainers-0.5.6.2-2C3ZI8RgPO2LBMidXKTvIU-ghc7.10.3.so      [.] cISR_info
       0,60%  as               ld-2.23.so                                                       [.] _dl_relocate_object
       0,58%  as               as                                                               [.] md_assemble
       0,54%  ghc              libHSbase-4.8.2.0-HQfYBxpPvuw8OunzQu6JGM-ghc7.10.3.so            [.] c7Vt_info
       0,50%  ghc              libHSghc-7.10.3-JQ2WG6oexwDDVprkkcjveL-ghc7.10.3.so              [.] cOKK_info
    #
    # Drilling down to r113_info (ENTER):
            │     0000000002fce688 <ghc_Pretty_bufLeftRenderzuzdsreduceDoc1_info+0x5b0>:
       0,91 │       H.E.L9
       2,65 │       A.e.L.
       7,36 │       ....H.
       2,29 │       H.....
            │       x.... .
       5,03 │       ......
       1,92 │       H..0H.
       1,01 │       @....D
            │       ...... .......
            │       H.E.H.
            │       ......
            │       ...... .......
      19,33 │       H.E.H.
       8,91 │       .5r?..
       0,09 │       ..H...
            │       ...H.U
       4,16 │       .H....
       6,26 │       .H.E.H
       3,43 │       .H.M.H
            │       ..r...
            │       .U.H..
       0,05 │       .H.E.H
            │       .hM;.X
            │       .S.H.[
            │       t$.I.D
            │       $.H...
            │       D$.I.L
            │       4...I.
            │       D$.I.D

Quite unreadable. At least it contains a part of hot function: **ghc_Pretty_bufLeftRenderzuzdsreduceDoc**.

**perf** does not show assembly code as **NCG** (native code generator)
marks internal assembly entry points
as **\@object**s (normal **C** code does **\@function** annotations).

So let's try to undo this hack and built ghc with debugging symbols:

.. code-block:: diff

    diff --git a/compiler/nativeGen/X86/CodeGen.hs b/compiler/nativeGen/X86/CodeGen.hs
    index cd45d92..e03c98f 100644
    --- a/compiler/nativeGen/X86/CodeGen.hs
    +++ b/compiler/nativeGen/X86/CodeGen.hs
    @@ -130,7 +130,8 @@ basicBlockCodeGen block = do
           -> do fileId <- getFileId (srcSpanFile span)
                 let line = srcSpanStartLine span; col = srcSpanStartCol span
                 return $ unitOL $ LOCATION fileId line col name
    -    _ -> return nilOL
    +    _ -> do _fileId <- getFileId (fsLit "dummy.hs")
    +            return nilOL
       mid_instrs <- stmtsToInstrs stmts
       tail_instrs <- stmtToInstrs tail
       let instrs = loc_instrs `appOL` mid_instrs `appOL` tail_instrs
    diff --git a/compiler/nativeGen/X86/Ppr.hs b/compiler/nativeGen/X86/Ppr.hs
    index 7809ae1..89e23fb 100644
    --- a/compiler/nativeGen/X86/Ppr.hs
    +++ b/compiler/nativeGen/X86/Ppr.hs
    @@ -142,8 +142,8 @@ pprGloblDecl lbl
     pprTypeAndSizeDecl :: CLabel -> SDoc
     pprTypeAndSizeDecl lbl
         = sdocWithPlatform $ \platform ->
    -      if osElfTarget (platformOS platform) && externallyVisibleCLabel lbl
    -      then text ".type " <> ppr lbl <> ptext (sLit ", @object")
    +      if osElfTarget (platformOS platform)
    +      then text ".type " <> ppr lbl <> ptext (sLit ", @function")
           else empty
    
     pprLabel :: CLabel -> SDoc

.. code-block:: bash

    ghc $ echo 'SRC_HC_OPTS += -g' >> mk/build.mk
    ghc $ ./configure && make

**\@object** annotations are used by **GHC** to avoid reading out local symbols via **PLT**
and resolve them directly. The **.hidden** assembly annotation should have the same
effect (**__attribute__((visibility("hidden")))** in **GNU** **C** speak).

That's it. Checking again our build benchmark:


.. code-block:: asm

    $ perf record -- inplace/bin/ghc-stage2 --make ... # building highlighting-kate
    $ perf report
    
      12,32%  ghc-stage2  ghc-stage2               [.] evacuate1
       4,21%  ghc-stage2  ghc-stage2               [.] stg_upd_frame_info
       1,80%  ghc-stage2  ghc-stage2               [.] scavenge_block1
       1,79%  ghc-stage2  ghc-stage2               [.] stg_BLACKHOLE_info
       1,28%  ghc-stage2  ghc-stage2               [.] eval_thunk_selector
       1,17%  ghc-stage2  ghc-stage2               [.] stg_gc_noregs
       1,16%  ghc-stage2  ghc-stage2               [.] sH9p_info
       0,97%  as          as                       [.] hash_lookup.isra.0
       0,84%  ghc-stage2  ghc-stage2               [.] sF9P_info
       0,75%  ghc-stage2  ghc-stage2               [.] stg_ap_0_fast
       0,57%  ghc-stage2  ghc-stage2               [.] stg_ap_p_info
       0,57%  ghc-stage2  ghc-stage2               [.] stg_ap_p_fast
       0,55%  ghc-stage2  ghc-stage2               [.] crE9_info
    #
    # Drilling down to sH9p_info (ENTER):
          │      00000000015a9d58 <sH9p_info>:
          │      sH9p_info():
          │      *                                                                      *
          │      ************************************************************************
          │      -}
          │
          │      traceTc :: String -> SDoc -> TcRn ()
          │      traceTc herald doc = traceTcN 1 (hang (text herald) 2 doc)
     1,41 │        lea    -0x478(%rbp),%rax
     0,36 │        cmp    %r15,%rax
          │      ↓ jb     ed3
     0,14 │        movq   $0x15ab478,-0x10(%rbp)
     0,36 │        mov    0x6(%rbx),%rax
     0,14 │        mov    %r14,%rbx
          │        mov    %rax,-0x8(%rbp)
     0,07 │        add    $0xfffffffffffffff0,%rbp
     0,04 │        test   $0x7,%bl
          │      ↓ jne    1720
     0,07 │      ↓ jmpq   fffffffffea562a8
          │              ...
          │
          │      00000000015a9da8 <cLB2_info>:
     0,14 │  50:   movq   $0x15aac98,-0x438(%rbp)
     0,33 │        mov    0x7(%rbx),%rax
     0,22 │        mov    0xf(%rbx),%rcx
     0,11 │        mov    0x17(%rbx),%rdx
     0,11 │        mov    0x1f(%rbx),%rsi
     0,14 │        mov    0x27(%rbx),%rdi
     0,11 │        mov    0x2f(%rbx),%r8
          │        mov    0x37(%rbx),%r9
     0,07 │        mov    0x3f(%rbx),%r10
     0,04 │        mov    0x47(%rbx),%r11
     0,11 │        mov    0x4f(%rbx),%r14
          │        mov    %rax,0x40(%rsp)
     0,22 │        mov    0x57(%rbx),%rax
     0,22 │        mov    %rcx,0x48(%rsp)
     0,22 │        mov    0x5f(%rbx),%rcx
          │        mov    %rdx,0x50(%rsp)
     0,04 │        mov    0x67(%rbx),%rdx
          │        mov    %rsi,0x58(%rsp)
     0,25 │        mov    0x6f(%rbx),%rsi
          │        mov    %rdi,0x60(%rsp)
     0,36 │        mov    0x77(%rbx),%rdi
     0,11 │        mov    %r8,0x68(%rsp)
     0,25 │        mov    0x7f(%rbx),%r8
     0,07 │        mov    %r9,0x70(%rsp)
     0,22 │        mov    0x87(%rbx),%r9
     0,18 │        mov    %r10,0x78(%rsp)
     0,22 │        mov    0x8f(%rbx),%r10
          │        mov    %r11,0x80(%rsp)
     0,36 │        mov    0x97(%rbx),%r11
     0,07 │        mov    %r14,0x88(%rsp)
     0,04 │        mov    0x9f(%rbx),%r14
     0,07 │        mov    %rax,0x90(%rsp)
     0,18 │        mov    0xa7(%rbx),%rax
     0,18 │        mov    %rcx,0x98(%rsp)
     0,40 │        mov    0xaf(%rbx),%rcx
     0,07 │        mov    %rdx,0xa0(%rsp)
     0,25 │        mov    0xb7(%rbx),%rdx
     ...

Now we can see not only instructions but also nicer function names and bits of haskell code!

As you can see there is a lot to be tweaked in **GHC** (and **perf**, and ...):

- **NCG** needs to be tweaked upstream to lie less about **.hidden** symbols
- **perf** could be smarter about executable "data" and disassemble non-functions
- **GC** takes a lot (most) of program runtime, it might mean some **+RTS** options need to be tweaked for this workload.
- **GHC** could generate **memcpy** C call instead of generating endless sequence of **mov** instructions (in last snippet)

Have fun!
