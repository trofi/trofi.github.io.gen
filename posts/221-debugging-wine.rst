---
title: "Debugging wine"
date: December 06, 2020
---

:PostID: 221
:Title: "Debugging wine"
:Keywords: gentoo, notes, gcc-11, wine
:Categories: notes

Sometimes I test gcc toolchains against less popular targets
like **i686-w64-mingw32** (32-bit windows). As a cheap run time
test I use `Wine <https://www.winehq.org/>`_. Today I tried to
run a simple executable:

.. code-block::

    $ echo 'int main() {}' | i686-w64-mingw32-gcc -x c - -o a.exe -ggdb3 && wine a.exe
    Segmentation fault

It crashed! I attempted to run standard **wine** programs.
That also did not work:

.. code-block::

    $ winecfg
    Segmentation fault

Maybe **wine** itself is broken? (and not just runtime
linked into my **a.exe**).

The new quest has started \\o/.

What is Wine?
-------------

**wine** is a rare sort of software: it executes
windows binaries on native system directly as much as reasonably
possible. No instruction level emulation is involved.

**wine** loads
`PE/COFF <https://en.wikipedia.org/wiki/Portable_Executable>`_
(windows native) executable file formats on systems that don't
support it (Linux native format is
`ELF <https://en.wikipedia.org/wiki/Executable_and_Linkable_Format>`_
for example).

Simpler windows-specific library calls are re-implemented in terms
of linux library calls. More complicated calls are implemented
by communicating with **wineserver** standalone daemon.

Debugging
---------

I never investigated program crashes under wine before. Internetz say
**wine** comes with
`winedbg <https://wiki.winehq.org/Wine_Developer%27s_Guide/Debugging_Wine>`_
tool that might help with that. I tried it:

.. code-block::

    $ winedbg a.exe
    Segmentation fault
    $ winedbg
    Segmentation fault

No luck. Why did **winedbg** fail? I tried to find something
I could attach **gdb** to:

.. code-block::

    $ file $(which winedbg)
    /usr/bin/winedbg: symbolic link to /usr/bin/winedbg-vanilla-5.22
    $ file $(which /usr/bin/winedbg-vanilla-5.22)
    /usr/bin/winedbg-vanilla-5.22: POSIX shell script, ASCII text executable
    $ file $(which winedbg)
    /usr/bin/winedbg: symbolic link to /usr/bin/winedbg-vanilla-5.22
    $ bash -x /usr/bin/winedbg-vanilla-5.22
    + exec /usr/lib/wine-vanilla-5.22/bin/winedbg
    Segmentation fault
    $ file /usr/lib/wine-vanilla-5.22/bin/winedbg
    /usr/lib/wine-vanilla-5.22/bin/winedbg: POSIX shell script, ASCII text executable
    $ bash -x /usr/lib/wine-vanilla-5.22/bin/winedbg
    ...
    + exec /usr/lib/wine-vanilla-5.22/bin/wine winedbg.exe
    Segmentation fault

    $ file /usr/lib/wine-vanilla-5.22/wine/fakedlls/winedbg.exe
    /usr/lib/wine-vanilla-5.22/wine/fakedlls/winedbg.exe: PE32 executable (console) Intel 80386, for MS Windows

The above says **/usr/bin/winedbg** is a shell wrapper around
**winedbg.exe** **PE/COFF** executable (ran through **wine**).
It makes **windbg** even more complex program than **a.exe**.

This is yet another hint at problems in **wine** itself. I settled
on debugging simpler **a.exe** program.

Getting clues
-------------

To get some idea what binary actually crashes I used **strace**:

.. code-block::

    $ strace -s 10000 -f -etrace=execve wine a.exe
    execve("/usr/bin/wine", ["wine", "a.exe"], 0x7ffd82e380d8 /* 83 vars */) = 0
    execve("/usr/lib/wine-vanilla-5.22/bin/wine", ["/usr/lib/wine-vanilla-5.22/bin/wine", "a.exe"], 0x55a3215c07c0 /* 81 vars */) = 0
    [ Process PID=2324610 runs in 32 bit mode. ]
    execve("/usr/lib/wine-vanilla-5.22/bin/wine-preloader", ["/usr/lib/wine-vanilla-5.22/bin/wine-preloader", "/usr/lib/wine-vanilla-5.22/bin/wine", "a.exe"], 0x7e99a2a0 /* 82 vars */) = 0
    strace: Process 2324611 attached
    strace: Process 2324612 attached
    [pid 2324611] +++ exited with 0 +++
    [pid 2324610] --- SIGCHLD {si_signo=SIGCHLD, si_code=CLD_EXITED, si_pid=2324611, si_uid=1000, si_status=0, si_utime=0, si_stime=0} ---
    [pid 2324612] execve("/usr/lib/wine-vanilla-5.22/bin/wine64-preloader", ["/usr/lib/wine-vanilla-5.22/bin/wine64-preloader", "/usr/lib/wine-vanilla-5.22/bin/wine64", "C:\\windows\\system32\\conhost.exe", "--unix", "--width", "192", "--height", "55", "--server", "0x10"], 0x7e890b10 /* 84 vars */) = 0
    [pid 2324612] [ Process PID=2324612 runs in 64 bit mode. ]
    [pid 2324610] --- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_MAPERR, si_addr=0xfffffffc} ---
    [pid 2324610] --- SIGSEGV {si_signo=SIGSEGV, si_code=SEGV_MAPERR, si_addr=0xfffffe28} ---
    [pid 2324610] +++ killed by SIGSEGV +++
    +++ exited with 0 +++
    Segmentation fault

    $ file /usr/lib/wine-vanilla-5.22/bin/wine-preloader
    /usr/lib/wine-vanilla-5.22/bin/wine-preloader: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), statically linked, stripped
    $ file /usr/lib/wine-vanilla-5.22/bin/wine64-preloader
    /usr/lib/wine-vanilla-5.22/bin/wine64-preloader: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), statically linked, stripped
    $ file /usr/lib64/wine-vanilla-5.22/wine/fakedlls/conhost.exe
    /usr/lib64/wine-vanilla-5.22/wine/fakedlls/conhost.exe: PE32+ executable (GUI) x86-64, for MS Windows

Here we see a few interesting bits:

- before catching **SIGSEGV** our program **execve()**s twice:
  - **wine** shell script execs **wine** binary
  - **wine** binary execs **wine-preloader** binary
- it also **fork()**s and execs **wine64-preloader** to run **conhost.exe**

  .. code-block::

     - wine a.exe -> exec "wine-preloader wine a.exe"
       ` -> fork -> exec "wine64-preloader conhost.exe"

- **wine-preloader** is a 32-bit static **ELF** executable. It's address space
  will host our future **a.exe**.

- **wine64-preloader** is a 64-bit static **ELF** executable. It's address space
  will host some 64-bit **conhost.exe** process. **conhost.exe** seems to be an
  equivalent of virtual terminal for windows.

We see a **SIGSEGV** only for 32-bit **wine-preloader**. 64-bit
processes seem to work fine.

I rebuilt **wine** with **CFLAGS+=-ggdb3** to get plenty of
debugging symbols and ran **wine** under **gdb**:

.. code-block::

    $ gdb --args /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    Reading symbols from /usr/lib/wine-vanilla-5.22/bin/wine...
    Reading symbols from /usr/lib/debug//usr/lib/wine-vanilla-5.22/bin/wine.debug...

    (gdb) run
    Starting program: /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    [Thread debugging using libthread_db enabled]
    Using host libthread_db library "/lib64/libthread_db.so.1".
    process 2326226 is executing new program: /usr/lib/wine-vanilla-5.22/bin/wine-preloader
    [Detaching after fork from child process 2326230]


    (gdb) info proc
    process 2326226
    cmdline = 'a.exe                                                                                  '
    cwd = '/'
    exe = '/usr/lib/wine-vanilla-5.22/bin/wine-preloader'

    Program received signal SIGSEGV, Segmentation fault.
    0x7bc714dd in ?? ()
    (gdb) bt
    #0  0x7bc714dd in ?? ()
    #1  0x7b03c0de in ?? ()
    #2  0x7edde0ab in ?? ()
    #3  0x7edff106 in ?? ()
    #4  0x7bc52d86 in ?? ()
    #5  0x7bc55c2f in ?? ()
    #6  0x7bc55f7c in ?? ()
    #7  0x7bc58e38 in ?? ()
    #8  0x00000000 in ?? ()

That is not a very useful backtrace. Why none of the addresses are
resolved?

Looks like **gdb** does not know anything about dynamically loaded
modules (handled by **wine-preloader** ?). As a workaround we can
manually add them.

The **2325141** debugged process is not yet finished and we can poke at
it's address map:

.. code-block::

    $ cat /proc/2325141/maps
    00010000-00110000 rw-p 00000000 00:00 0
    00110000-00120000 rwxp 00000000 00:00 0
    ...
    00400000-00401000 r-xp 00000000 08:03 53849265                           /home/slyfox/.wine/drive_c/a.exe
    00401000-00403000 r-xp 00000000 00:00 0
    ...
    0040a000-0040c000 r-xp 00003000 08:03 53849265                           /home/slyfox/.wine/drive_c/a.exe
    ...
    7b000000-7b016000 r--p 00000000 08:03 86205568                           /usr/lib/wine-vanilla-5.22/wine/kernelbase.dll.so
    ...
    7bc00000-7bc19000 r--p 00000000 08:03 86206617                           /usr/lib/wine-vanilla-5.22/wine/ntdll.dll.so
    ...
    7d000000-7d001000 r--p 00000000 08:03 86270026                           /usr/lib/wine-vanilla-5.22/bin/wine
    ...
    7d001000-7d002000 r-xp 00001000 08:03 86270026                           /usr/lib/wine-vanilla-5.22/bin/wine
    ...
    7d3ff000-7d400000 r--p 00000000 08:03 86270404                           /usr/lib/wine-vanilla-5.22/bin/wine-preloader
    ...
    7d405000-7d427000 rw-p 00000000 00:00 0                                  [heap]
    ...
    7ed8c000-7ed8e000 r--p 00000000 08:03 86205774                           /usr/lib/wine-vanilla-5.22/wine/msvcrt.so
    ...
    7ee78000-7ee7d000 r--p 00000000 08:03 85966906                           /usr/lib/wine-vanilla-5.22/libwine.so.1.0
    ...
    f7c12000-f7c1c000 r--p 00000000 08:03 85213329                           /lib/libm-2.32.so
    ...
    f7cdf000-f7cf3000 r--p 00000000 08:03 86206591                           /usr/lib/wine-vanilla-5.22/wine/ntdll.so
    ...
    f7d81000-f7d9a000 r--p 00000000 08:03 85213351                           /lib/libc-2.32.so
    ...
    f7d9a000-f7eee000 r-xp 00019000 08:03 85213351                           /lib/libc-2.32.so
    ...
    f7fcc000-f7fcd000 r--p 00000000 08:03 85213350                           /lib/ld-2.32.so
    ...
    f7ff8000-f7ffc000 r--p 00000000 00:00 0                                  [vvar]
    f7ffc000-f7ffe000 r-xp 00000000 00:00 0                                  [vdso]
    f8000000-ffcd0000 ---p 00000000 00:00 0
    ffcd0000-fffd0000 rw-p 00000000 00:00 0
    fffdc000-ffffe000 rw-p 00000000 00:00 0                                  [stack]

This is a huge list of objects present in our address space.

Now looking at **gdb**'s view:

.. code-block::

    (gdb) info files
    Symbols from "/usr/lib/wine-vanilla-5.22/bin/wine-preloader".
    Native process:
            Using the running image of child process 2326226.
            While running this, GDB does not access memory from...
    Local exec file:
            `/usr/lib/wine-vanilla-5.22/bin/wine-preloader', file type elf32-i386.
            Entry point: 0x7d400000
            0x7d400000 - 0x7d401880 is .text
            0x7d402000 - 0x7d4022e0 is .rodata
            0x7d4022e0 - 0x7d4024a4 is .eh_frame
            0x7d404000 - 0x7d40400c is .got.plt
            0x7d404020 - 0x7d404060 is .data
            0x7d404060 - 0x7d404490 is .bss


**gdb** onlysees  **wine-preloader** binary,
but our backtrace contains addresses like **0x7bc714dd** which looks
suspiciously close to **ntdll.dll.so** range from **/proc/$pid/maps**.

Let's add **ntdll.dll.so** symbols into our session:

.. code-block::

    (gdb) add-symbol-file /usr/lib/wine-vanilla-5.22/wine/ntdll.dll.so
    add symbol table from file "/usr/lib/wine-vanilla-5.22/wine/ntdll.dll.so"
    (y or n) y

    Reading symbols from /usr/lib/wine-vanilla-5.22/wine/ntdll.dll.so...
    Reading symbols from /usr/lib/debug//usr/lib/wine-vanilla-5.22/wine/ntdll.dll.so.debug...

    (gdb) info registers
    eax            0x0                 0
    ecx            0x0                 0
    edx            0x7ffc2c00          2147232768
    ebx            0x0                 0
    esp            0xfffffffc          0xfffffffc
    ebp            0x61ed88            0x61ed88
    esi            0x7ffc2bf8          2147232760
    edi            0x61ed60            6417760
    eip            0x7bc714dd          0x7bc714dd <RtlAnsiStringToUnicodeString+141>
    eflags         0x10246             [ PF ZF IF RF ]
    cs             0x23                35
    ss             0x2b                43
    ds             0x2b                43
    es             0x2b                43
    fs             0x63                99
    gs             0x6b                107

    (gdb) bt
    #0  0x7bc714dd in RtlAnsiStringToUnicodeString (uni=<error reading variable: Cannot access memory at address 0x0>, ansi=<error reading variable: Cannot access memory at address 0x4>,
        doalloc=<error reading variable: Cannot access memory at address 0x8>) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:581
    Backtrace stopped: Cannot access memory at address 0xfffffffc

We got a function name! But our backtrace became even worse:
only one frame is visible instead of 8 frames before. What is wrong?

The stack pointer (**esp 0xfffffffc**) is incorrect and looks more like
a **NULL** pointer dereference with small negative offset than a real
stack pointer. No chance to get 8 frames here.

Perhaps **gdb** used frame pointer (**ebp 0x61ed88**) before we loaded symbols?

Let's try to fake stack pointer to be closer to frame pointer with a hack:

.. code-block::

    (gdb) set $esp=$ebp

    (gdb) bt
    #0  0x7bc714dd in RtlAnsiStringToUnicodeString (uni=0x7b03c0de, ansi=0x7ee405be, doalloc=0 '\000') at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:581
    #1  0x0061edc8 in ?? ()
    #2  0x7b03c0de in ?? ()
    #3  0x7edde0ab in ?? ()
    #4  0x7edff106 in ?? ()
    #5  0x7bc52d86 in call_dll_entry_point ()
    #6  0x7ffc2c00 in ?? ()
    Backtrace stopped: previous frame inner to this frame (corrupt stack?)

This looks more plausible. It's still probably wrong as stack pointer within function
is likely off by a few values. But it's better that nothing.

Let's add symbols for address **0x7b03c0de** from **/proc/2325141/maps**.
**kernelbase.dll.so** looks closest.

.. code-block::

    (gdb) add-symbol-file /usr/lib/wine-vanilla-5.22/wine/kernelbase.dll.so
    add symbol table from file "/usr/lib/wine-vanilla-5.22/wine/kernelbase.dll.so"
    (y or n) y
    Reading symbols from /usr/lib/wine-vanilla-5.22/wine/kernelbase.dll.so...
    Reading symbols from /usr/lib/debug//usr/lib/wine-vanilla-5.22/wine/kernelbase.dll.so.debug...

    (gdb) bt
    #0  0x7bc714dd in RtlAnsiStringToUnicodeString (uni=0x7b03c0de <CreateFileA+73>, ansi=0x7ee405be, doalloc=0 '\000')
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:581
    #1  0x0061edc8 in ?? ()
    #2  0x7b03c0de in CreateFileA (name=0x7ee405be "CONIN$", access=3221225472, sharing=3, sa=0x0, creation=3, attributes=0, template=0x0)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/kernelbase/file.c:704
    #3  0x7edde0ab in ?? ()
    #4  0x7edff106 in ?? ()
    #5  0x7bc52d86 in call_dll_entry_point ()
    #6  0x00000003 in ?? ()
    #7  0x00000004 in ?? ()
    #8  0x0000ffff in ?? ()
    #9  0x000000b8 in ?? ()
    #10 0x00000000 in ?? ()

Presence of **CreateFileA("CONIN$") / RtlAnsiStringToUnicodeString()**
looks very close!

But parameters to **RtlAnsiStringToUnicodeString()** seem incorrect:
**uni=0x7b03c0de <CreateFileA+73>** is a code address (should be some
writable address). It's probably a stack corruption and our stack
pointer is wrong.

Ideally I would like to get to the point where corruption happens.

The definition of **RtlAnsiStringToUnicodeString()** has nothing unusual or suspicious:

.. code-block:: c

    //(gdb) list RtlAnsiStringToUnicodeString
         NTSTATUS WINAPI RtlAnsiStringToUnicodeString(
             PUNICODE_STRING uni, /* [I/O] Destination for the unicode string */
             PCANSI_STRING ansi,  /* [I]   Ansi string to be converted */
             BOOLEAN doalloc)     /* [I]   TRUE=Allocate new buffer for uni, FALSE=Use existing buffer */
         {
             DWORD total = RtlAnsiStringToUnicodeSize( ansi );
    
             if (total > 0xffff) return STATUS_INVALID_PARAMETER_2;
             uni->Length = total - sizeof(WCHAR);
             if (doalloc)
             {
                 uni->MaximumLength = total;
                 if (!(uni->Buffer = RtlAllocateHeap( GetProcessHeap(), 0, total )))
                     return STATUS_NO_MEMORY;
             }
             else if (total > uni->MaximumLength) return STATUS_BUFFER_OVERFLOW;
    
             RtlMultiByteToUnicodeN( uni->Buffer, uni->Length, NULL, ansi->Buffer, ansi->Length );
             uni->Buffer[uni->Length / sizeof(WCHAR)] = 0;
             return STATUS_SUCCESS;
         }

Let's see if we can find out something else about the way our program crashes
by looking at exact instructions and register values around:

.. code-block::

    (gdb) disassemble RtlAnsiStringToUnicodeString
    Dump of assembler code for function RtlAnsiStringToUnicodeString:
       0x7bc71450 <+0>:     lea    0x4(%esp),%ecx
       0x7bc71454 <+4>:     and    $0xfffffff0,%esp
       0x7bc71457 <+7>:     push   -0x4(%ecx)
       0x7bc7145a <+10>:    push   %ebp
       0x7bc7145b <+11>:    mov    %esp,%ebp
       0x7bc7145d <+13>:    push   %edi
       0x7bc7145e <+14>:    push   %esi
       0x7bc7145f <+15>:    push   %ebx
       0x7bc71460 <+16>:    push   %ecx
       0x7bc71461 <+17>:    sub    $0x1c,%esp
       0x7bc71464 <+20>:    mov    0x4(%ecx),%esi
       0x7bc71467 <+23>:    mov    (%ecx),%ebx
       0x7bc71469 <+25>:    mov    0x8(%ecx),%edi
       0x7bc7146c <+28>:    movzwl (%esi),%eax
       0x7bc7146f <+31>:    push   %eax
       0x7bc71470 <+32>:    lea    -0x1c(%ebp),%eax
       0x7bc71473 <+35>:    push   0x4(%esi)
       0x7bc71476 <+38>:    push   %eax
       0x7bc71477 <+39>:    call   0x7bc5d3e0 <RtlMultiByteToUnicodeSize>
       ....
       0x7bc714d5 <+133>:   pop    %ecx
       0x7bc714d6 <+134>:   pop    %ebx
       0x7bc714d7 <+135>:   pop    %esi
       0x7bc714d8 <+136>:   pop    %edi
       0x7bc714d9 <+137>:   pop    %ebp
       0x7bc714da <+138>:   lea    -0x4(%ecx),%esp
    => 0x7bc714dd <+141>:   ret    $0xc

    (gdb) print (void*)$ecx
    $3 = (void *) 0x0
    (gdb) print (void*)$ecx-4
    $4 = (void *) 0xfffffffc

Hah, it's completely obvious! You can see the bug, right?
No, it's not obvious at all. I'm just kidding.

In the dump above I saw nothing that would stands.

But somehow **ecx** value on stack was overwritten by **0x0**
value and we were not able to return back from the function
with **ret $0xc**. That is an explicit sign of stack corruption.

Let's find out what overwrites **ecx** on stack
while **RtlAnsiStringToUnicodeString()** is executing.

I tried a naive attempt by running **wine** under **gdb** as is:

.. code-block::

    $ gdb --args /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    Reading symbols from /usr/lib/wine-vanilla-5.22/bin/wine...
    Reading symbols from /usr/lib/debug//usr/lib/wine-vanilla-5.22/bin/wine.debug...

    (gdb) break RtlAnsiStringToUnicodeString
    Function "RtlAnsiStringToUnicodeString" not defined.
    Make breakpoint pending on future shared library load? (y or [n]) y
    Breakpoint 1 (RtlAnsiStringToUnicodeString) pending.
    (gdb) run

    Starting program: /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    [Thread debugging using libthread_db enabled]
    Using host libthread_db library "/lib64/libthread_db.so.1".
    process 2326439 is executing new program: /usr/lib/wine-vanilla-5.22/bin/wine-preloader
    [Detaching after fork from child process 2326443]

    Program received signal SIGSEGV, Segmentation fault.
    0x7bc714dd in ?? ()

Execution did not stop at **RtlAnsiStringToUnicodeString()**. My understanding on why
it happened is because **ntdll.dll.so** gets loaded to address space in a way not
visible to **gdb**. It has something to do with re-execution of **wine-preloader**
binary in place of **wine**.

I happened to notice that the way re-execution happens
is by checking `WINELOADERNOEXEC=1 <https://source.winehq.org/git/wine.git/blob/HEAD:/dlls/ntdll/unix/loader.c#l1758>`_.

My understanding of re-execution is to adjust **PATH**, **WINEPREFIX**
and similar variables (and switch 32/64-bit address space if needed)

We should be able to set environment up as needed without the need for re-execution.

I tried the simple way:

.. code-block::

    $ WINELOADERNOEXEC=1 gdb --args /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    Reading symbols from /usr/lib/wine-vanilla-5.22/bin/wine...
    Reading symbols from /usr/lib/debug//usr/lib/wine-vanilla-5.22/bin/wine.debug...

    (gdb) run
    Starting program: /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    [Thread debugging using libthread_db enabled]
    Using host libthread_db library "/lib64/libthread_db.so.1".
    [Detaching after fork from child process 2326739]
    
    Program received signal SIGSEGV, Segmentation fault.
    0x7bc714dd in RtlAnsiStringToUnicodeString (uni=<error reading variable: Cannot access memory at address 0x0>, ansi=<error reading variable: Cannot access memory at address 0x4>,
        doalloc=<error reading variable: Cannot access memory at address 0x8>) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:581
    581     }

    (gdb) bt
    #0  0x7bc714dd in RtlAnsiStringToUnicodeString (uni=<error reading variable: Cannot access memory at address 0x0>, ansi=<error reading variable: Cannot access memory at address 0x4>,
        doalloc=<error reading variable: Cannot access memory at address 0x8>) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:581
    Backtrace stopped: Cannot access memory at address 0xfffffffc
    (gdb) set $esp=$ebp
    (gdb) bt
    #0  0x7bc714dd in RtlAnsiStringToUnicodeString (uni=0x7b03c0de <CreateFileA+73>, ansi=0x7fe205be, doalloc=0 '\000')
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:581
    #1  0x0061edc8 in ?? ()
    #2  0x7b03c0de in CreateFileA (name=0x7fe205be "CONIN$", access=3221225472, sharing=3, sa=0x0, creation=3, attributes=0, template=0x0)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/kernelbase/file.c:704
    #3  0x7fdbe0ab in msvcrt_init_console () at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/msvcrt/console.c:48
    #4  0x7fddf106 in DllMain (hinstDLL=0x7fdb0000 <__wine_spec_pe_header+49120>, fdwReason=1, lpvReserved=0x61fd24)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/msvcrt/main.c:115
    #5  0x7bc52d86 in call_dll_entry_point () from /usr/lib/wine-vanilla-5.22/wine/ntdll.dll.so
    #6  0x7bc55c2f in MODULE_InitDLL (wm=wm@entry=0x1110d8, reason=reason@entry=1, lpReserved=lpReserved@entry=0x61fd24)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/loader.c:1329
    #7  0x7bc55f7c in process_attach (wm=0x1110d8, lpReserved=lpReserved@entry=0x61fd24) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/loader.c:1423
    #8  0x7bc58e38 in process_attach (lpReserved=0x61fd24, wm=<optimized out>) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/loader.c:1394
    #9  LdrInitializeThunk (context=0x61fd24, unknown2=1073733632, unknown3=0, unknown4=0) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/loader.c:3539
    #10 0x00000000 in ?? ()

It worked as is! We get a nice backtrace and we can even set lazy breakpoints.

Let's track down the exact place where stack corruption happens. The plan is simple:

1. Stop at the call of **RtlAnsiStringToUnicodeString()**
2. Check where **ecx** is saved on stack there
3. Set a watchpoint on memory location in saved stack space
4. Observe where change happens.

Full session that executes it:

.. code-block::

    $ WINELOADERNOEXEC=1 gdb --args /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    Reading symbols from /usr/lib/wine-vanilla-5.22/bin/wine...
    Reading symbols from /usr/lib/debug//usr/lib/wine-vanilla-5.22/bin/wine.debug...

    (gdb) break RtlAnsiStringToUnicodeString
    Function "RtlAnsiStringToUnicodeString" not defined.
    Make breakpoint pending on future shared library load? (y or [n]) y
    Breakpoint 1 (RtlAnsiStringToUnicodeString) pending.

    (gdb) run
    Starting program: /usr/lib/wine-vanilla-5.22/bin/wine a.exe
    [Thread debugging using libthread_db enabled]
    Using host libthread_db library "/lib64/libthread_db.so.1".
    [Detaching after fork from child process 2326777]

    Breakpoint 1, RtlAnsiStringToUnicodeString (uni=0x3ffe2bf8, ansi=0x61ed60, doalloc=0 '\000') at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:566
    566         DWORD total = RtlAnsiStringToUnicodeSize( ansi );
    (gdb) disassemble
    Dump of assembler code for function RtlAnsiStringToUnicodeString:
    => 0x7bc71450 <+0>:     lea    0x4(%esp),%ecx
       0x7bc71454 <+4>:     and    $0xfffffff0,%esp
       0x7bc71457 <+7>:     push   -0x4(%ecx)
       0x7bc7145a <+10>:    push   %ebp
       0x7bc7145b <+11>:    mov    %esp,%ebp
       0x7bc7145d <+13>:    push   %edi
       0x7bc7145e <+14>:    push   %esi
       0x7bc7145f <+15>:    push   %ebx
       0x7bc71460 <+16>:    push   %ecx
       0x7bc71461 <+17>:    sub    $0x1c,%esp
    ...

    (gdb) break *0x7bc71461
    Breakpoint 2 at 0x7bc71461: file /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c, line 565.

    (gdb) continue
    Continuing.
    
    Breakpoint 2, 0x7bc71461 in RtlAnsiStringToUnicodeString (uni=0x3ffe2bf8, ansi=0x61ed60, doalloc=0 '\000')
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:565
    565     {

    (gdb) disassemble
    Dump of assembler code for function RtlAnsiStringToUnicodeString:
       0x7bc71450 <+0>:     lea    0x4(%esp),%ecx
       0x7bc71454 <+4>:     and    $0xfffffff0,%esp
       0x7bc71457 <+7>:     push   -0x4(%ecx)
       0x7bc7145a <+10>:    push   %ebp
       0x7bc7145b <+11>:    mov    %esp,%ebp
       0x7bc7145d <+13>:    push   %edi
       0x7bc7145e <+14>:    push   %esi
       0x7bc7145f <+15>:    push   %ebx
       0x7bc71460 <+16>:    push   %ecx
    => 0x7bc71461 <+17>:    sub    $0x1c,%esp

    (gdb) print (void*)$ecx
    $2 = (void *) 0x61ed50
    (gdb) print *(void**)$esp
    $3 = (void *) 0x61ed50
    (gdb) print (void*)$esp
    $4 = (void *) 0x61ed28

    (gdb) watch -l *0x61ed28
    Hardware watchpoint 3: -location *0x61ed28
    (gdb) continue
    Continuing.

    Hardware watchpoint 3: -location *0x61ed28

    Old value = 6417744
    New value = 0
    RtlCustomCPToUnicodeN (info=0x7bcc14ec <nls_info+44>, dst=0x0, dstlen=0, reslen=0x61ed28, src=<optimized out>, srclen=<optimized out>)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/locale.c:972
    972         return STATUS_SUCCESS

    (gdb) bt
    #0  RtlCustomCPToUnicodeN (info=0x7bcc14ec <nls_info+44>, dst=0x0, dstlen=0, reslen=0x61ed28, src=<optimized out>, srclen=<optimized out>)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/locale.c:972
    #1  0x7bc5d3c5 in RtlMultiByteToUnicodeN (dst=0x3ffe2c00, dstlen=12, reslen=0x0, src=0x7fe205be "CONIN$", srclen=6)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/locale.c:1019
    #2  0x7bc714bb in RtlAnsiStringToUnicodeString (uni=<error reading variable: Cannot access memory at address 0x0>, ansi=<error reading variable: Cannot access memory at address 0x4>,
        doalloc=<error reading variable: Cannot access memory at address 0x8>) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:578
    Backtrace stopped: previous frame inner to this frame (corrupt stack?)

Aha! From the above we see that **RtlCustomCPToUnicodeN()**
managed to write into stack of it's grand-caller:

.. code-block::

    -> RtlAnsiStringToUnicodeString() [puts $ecx on stack ]
      ->RtlMultiByteToUnicodeN()
        ->RtlCustomCPToUnicodeN() [ corrupts $ecx on stack ]

The call chain is straightforward but unexpected. Let's see the definitions
of **RtlCustomCPToUnicodeN()** and **RtlMultiByteToUnicodeN()** for clues
where stack corruption could happen due to a code bug:

.. code-block:: c

    //(gdb) list RtlCustomCPToUnicodeN
    NTSTATUS WINAPI RtlCustomCPToUnicodeN( CPTABLEINFO *info, WCHAR *dst, DWORD dstlen, DWORD *reslen,
                                           const char *src, DWORD srclen )
    {
        DWORD i, ret;

        dstlen /= sizeof(WCHAR);
        if (info->DBCSOffsets)
        {
            for (i = dstlen; srclen && i; i--, srclen--, src++, dst++)
            {

                USHORT off = info->DBCSOffsets[(unsigned char)*src];
                if (off && srclen > 1)
                {
                    src++;
                    srclen--;
                    *dst = info->DBCSOffsets[off + (unsigned char)*src];
                }
                else *dst = info->MultiByteTable[(unsigned char)*src];
            }
            ret = dstlen - i;

        }
        else
        {
            ret = min( srclen, dstlen );
            for (i = 0; i < ret; i++) dst[i] = info->MultiByteTable[(unsigned char)src[i]];
        }
        if (reslen) *reslen = ret * sizeof(WCHAR);
        return STATUS_SUCCESS;
    }

    NTSTATUS WINAPI RtlMultiByteToUnicodeN( WCHAR *dst, DWORD dstlen, DWORD *reslen,
                                            const char *src, DWORD srclen )
    {
        if (nls_info.AnsiTableInfo.WideCharTable)
            return RtlCustomCPToUnicodeN( &nls_info.AnsiTableInfo, dst, dstlen, reslen, src, srclen );

        /* locale not setup yet */
        dstlen = min( srclen, dstlen / sizeof(WCHAR) );

        if (reslen) *reslen = dstlen * sizeof(WCHAR);
        while (dstlen--) *dst++ = *src++ & 0x7f;
        return STATUS_SUCCESS;
    }

**gdb** says corruption happens at a line
**if (reslen) *reslen = ret * sizeof(WCHAR);**. But what is
wrong with it?

The hint is seen in backtrace itself:

.. code-block::

    (gdb) bt
    #0  RtlCustomCPToUnicodeN (info=0x7bcc14ec <nls_info+44>, dst=0x0, dstlen=0, reslen=0x61ed28, src=<optimized out>, srclen=<optimized out>)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/locale.c:972
    #1  0x7bc5d3c5 in RtlMultiByteToUnicodeN (dst=0x3ffe2c00, dstlen=12, reslen=0x0, src=0x7fe205be "CONIN$", srclen=6)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/locale.c:1019
    #2  0x7bc714bb in RtlAnsiStringToUnicodeString (uni=<error reading variable: Cannot access memory at address 0x0>, ansi=<error reading variable: Cannot access memory at address 0x4>,
        doalloc=<error reading variable: Cannot access memory at address 0x8>) at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/rtlstr.c:578
    Backtrace stopped: previous frame inner to this frame (corrupt stack?)

Somehow **reslen** value in **gdb** backtrace suddenly changed
from **reslen=0x0** (frame 2) to **reslen=0x61ed28** (frame 3).
That smells like an ABI mismatch between caller and callee.

Let's have a peek at how **RtlMultiByteToUnicodeN()** calls **RtlCustomCPToUnicodeN()**
in the assembly:

.. code-block::

    (gdb) fr 1
    #1  0x7bc5d3c5 in RtlMultiByteToUnicodeN (dst=0x3ffe2c00, dstlen=12, reslen=0x0, src=0x7fe205be "CONIN$", srclen=6)
        at /usr/src/debug/app-emulation/wine-vanilla-5.22-r2/wine-5.22/dlls/ntdll/locale.c:1019
    1019            return RtlCustomCPToUnicodeN( &nls_info.AnsiTableInfo, dst, dstlen, reslen, src, srclen );

    Dump of assembler code for function RtlMultiByteToUnicodeN:
       0x7bc5d330 <+0>:     lea    0x4(%esp),%ecx
       0x7bc5d334 <+4>:     and    $0xfffffff0,%esp
       0x7bc5d337 <+7>:     push   -0x4(%ecx)
       0x7bc5d33a <+10>:    push   %ebp
       0x7bc5d33b <+11>:    mov    %esp,%ebp
       0x7bc5d33d <+13>:    push   %ebx
       0x7bc5d33e <+14>:    mov    0x8(%ebp),%eax
       0x7bc5d341 <+17>:    vmovd  0xc(%ebp),%xmm0
       0x7bc5d346 <+22>:    vmovd  0x18(%ebp),%xmm1
       0x7bc5d34b <+27>:    mov    0x7bcc150c,%ebx
       0x7bc5d351 <+33>:    push   %ecx
       0x7bc5d352 <+34>:    mov    0x10(%ebp),%edx
       0x7bc5d355 <+37>:    mov    0x14(%ebp),%ecx
       0x7bc5d358 <+40>:    test   %ebx,%ebx
       0x7bc5d35a <+42>:    jne    0x7bc5d3a8 <RtlMultiByteToUnicodeN+120>
       ...
       0x7bc5d3a8 <+120>:   sub    $0x4,%esp
       0x7bc5d3ab <+123>:   vmovd  %xmm1,(%esp)
       0x7bc5d3b0 <+128>:   push   %ecx
       0x7bc5d3b1 <+129>:   push   %edx
       0x7bc5d3b2 <+130>:   sub    $0x4,%esp
       0x7bc5d3b5 <+133>:   vmovd  %xmm0,(%esp)
       0x7bc5d3ba <+138>:   push   %eax
       0x7bc5d3bb <+139>:   push   $0x7bcc14ec
       0x7bc5d3c0 <+144>:   call   0x7bc5d150 <RtlCustomCPToUnicodeN>
    => 0x7bc5d3c5 <+149>:   lea    -0x8(%ebp),%esp
       0x7bc5d3c8 <+152>:   pop    %ecx
       0x7bc5d3c9 <+153>:   pop    %ebx
       0x7bc5d3ca <+154>:   pop    %ebp
       0x7bc5d3cb <+155>:   lea    -0x4(%ecx),%esp
       0x7bc5d3ce <+158>:   ret    $0x14

Here we already have suspicious code. Can you spot it?

We know what we are looking for:
we need to track life of parameter 3 (**reslen=0x0**) being passed as parameter 4 into
**RtlCustomCPToUnicodeN()**.

All the 3 functions in the backtrace are **WINAPI**
(aka **__attribute__((stdcall))**). This means all argumens
should be passed on stack. Specifically at **RtlMultiByteToUnicodeN()**
entry first argument should be at **esp + 4**:

.. code-block::

    ; at RtlMultiByteToUnicodeN entry:
                 | ...
     $esp     -> | <return address to RtlAnsiStringToUnicodeString>
     $esp + 4 -> | arg1: dst
                 | arg2: dstlen
                 | arg3: reslen
                 | arg4: src
                 | arg5: srclen
                 | ...

Let's trace **reslen** through to **RtlCustomCPToUnicodeN()** call
site (**RtlMultiByteToUnicodeN+144**).

.. code-block::

    Dump of assembler code for function RtlMultiByteToUnicodeN:
       0x7bc5d330 <+0>:     lea    0x4(%esp),%ecx   ; $ecx=$esp+4 (save pointer to parameter 1 on stack)
       0x7bc5d334 <+4>:     and    $0xfffffff0,%esp ; align $esp to 16 bytes
       0x7bc5d337 <+7>:     push   -0x4(%ecx)       ; re-save return address on current top of stack
       0x7bc5d33a <+10>:    push   %ebp             ; safe frame pointer.
       0x7bc5d33b <+11>:    mov    %esp,%ebp
       0x7bc5d33d <+13>:    push   %ebx
       0x7bc5d33e <+14>:    mov    0x8(%ebp),%eax   ; load arg1(dst)
       0x7bc5d341 <+17>:    vmovd  0xc(%ebp),%xmm0  ; load arg2(dstlen)
       0x7bc5d346 <+22>:    vmovd  0x18(%ebp),%xmm1 ; load arg5(srclen)
       0x7bc5d34b <+27>:    mov    0x7bcc150c,%ebx  ; load 'nls_info.AnsiTableInfo.WideCharTable' address
       0x7bc5d351 <+33>:    push   %ecx
       0x7bc5d352 <+34>:    mov    0x10(%ebp),%edx  ; ->>> load arg3(reslen)
       0x7bc5d355 <+37>:    mov    0x14(%ebp),%ecx  ; load arg4(src)
       0x7bc5d358 <+40>:    test   %ebx,%ebx
       0x7bc5d35a <+42>:    jne    0x7bc5d3a8 <RtlMultiByteToUnicodeN+120>
       ...
       0x7bc5d3ab <+123>:   vmovd  %xmm1,(%esp)     ; push srclen (forward arg5 unmodified)
       0x7bc5d3b0 <+128>:   push   %ecx             ; push src (forward arg4 unmodified)
       0x7bc5d3b1 <+129>:   push   %edx             ; ->>> push reslen (forward arg3 unmodified)
       0x7bc5d3b2 <+130>:   sub    $0x4,%esp
       0x7bc5d3b5 <+133>:   vmovd  %xmm0,(%esp)     ; push dstlen (forward arg2 unmodified)
       0x7bc5d3ba <+138>:   push   %eax             ; push dst (forward arg1 unmodified)
       0x7bc5d3bb <+139>:   push   $0x7bcc14ec      ; push info
       0x7bc5d3c0 <+144>:   call   0x7bc5d150 <RtlCustomCPToUnicodeN>
       0x7bc5d3c5 <+149>:   lea    -0x8(%ebp),%esp
       0x7bc5d3c8 <+152>:   pop    %ecx
       0x7bc5d3c9 <+153>:   pop    %ebx
       0x7bc5d3ca <+154>:   pop    %ebp
       0x7bc5d3cb <+155>:   lea    -0x4(%ecx),%esp
       0x7bc5d3ce <+158>:   ret    $0x14

The only problem with this code is that arguments are loaded from
**ebp** as if **and $0xfffffff0,%esp** instruction never existed.
And as a result we load arguments at a wrong stack location!

That is a compiler bug. Filed as https://gcc.gnu.org/PR98161.

Here is the single-file example:

.. code-block:: c

    typedef unsigned short u16;
    typedef unsigned int   u32;
    typedef unsigned char  u8;
    
    u32
        __attribute__((__force_align_arg_pointer__))
    unreach(
        const u16 * pu16,
        u16 *dst, u32 dstlen,
        const u8 *src, u32 srclen
      )
    {
        for (u32 i = dstlen; srclen && i; i--, srclen--, src++, dst++)
        {
            u16 off = pu16[*src];
            if (off)
            {
                src++; srclen--;
                *dst = pu16[off + *src];
            }
        }
        return 56;
    }
    
    u32
        __attribute__((__force_align_arg_pointer__))
        __attribute__((noipa))
    bug(
        const u16 * pu16,
        u16 *dst, u32 dstlen,
        const u8 *src, u32 srclen
      )
    {
        if (pu16)
           /* Branch should not execute, but stack realignment
            * reads wrong 'pu16' value from stack. */
            return unreach(pu16, dst, dstlen, src, srclen);
    
        return (srclen < dstlen) ? srclen : dstlen;
    }
    
    int main() {
        /* Should return 12 */
        return bug(0, 0, 12, 0, 34);
    }

The mismatch is seen on **-O1** / **-O2**:

.. code-block::

    $ x86_64-pc-linux-gnu-gcc -m32 -fno-PIC -fno-builtin -pipe -fcf-protection=none -fno-stack-protector -fno-omit-frame-pointer \
        -O1 -mavx -o bug bug.c.c
    ./bug; echo $?
    12

    $ x86_64-pc-linux-gnu-gcc -m32 -fno-PIC -fno-builtin -pipe -fcf-protection=none -fno-stack-protector -fno-omit-frame-pointer \
        -O2 -mavx -o bug bug.c.c
    ./bug; echo $?
    56

There are a few unusual things about the example:

- **wine** uses **__attribute__((__force_align_arg_pointer__))** to realign stack
  as windows and linux have slightly different alignment assumptions.
- **-msse4** or above was used to compile **wine**
- unreleased **gcc-11** was used to compile **wine** (**gcc-10** works fine)

Parting words
-------------

- **wine** can normally run **mingw**-compiled binaries :)

- **wine**'s loader process is very similar to **glibc**'s ld process
  with a few minor differences like re-execution on first run.

- **WINELOADERNOEXEC=1** is a good hack to make **wine** more
  debuggable under **gdb**. I wonder if **gdb** (or **wine**?)
  could be tweaked to avoid the need for **WINELOADERNOEXEC=1**
  as a workaround to get symbols.

- **gdb** is quite usable to debug binaries under **wine**.

- **gcc-11** is still an unreleased version of compiler. If you
  enjoy tracking these kinds of bugs as I do you might want to
  give it a try.

I reported the bug this Sunday morning and was lazily writing
this blog post. **gcc** upstream already fixed bug in master
before I have finished the post. What a speed!

Have fun!
