---
title: "perf and DWARF and fork()"
date: April 10, 2020
---

:PostID: 215
:Title: "perf and DWARF and fork()"
:Keywords: perf, DWARF, fork, bash
:Categories: notes

Tl;DR
-----

If you wonder why **perf record -g** does not collect nice callstacks for your prorgams
try **perf record -g \-\-call-graph=dwarf** instead.

Case study
----------

Simple example:

.. code-block::

    $ perf record -F 99 -g -- \
        /usr/lib/llvm/10/bin/clang -w -x c++ -std=c++1z -O0 -fstrict-aliasing -c A.cpp.c -o A.o
    $ perf report
       Children      Self  Command  Shared Object        Symbol
    +   35,83%     0,00%  clang    [unknown]            [.] 0000000000000000
    +   17,71%     0,00%  clang    [unknown]            [.] 0x0000441f0f000000
    +    4,86%     0,00%  clang    [unknown]            [.] 0x0000000100000007
    +    4,86%     0,00%  clang    [unknown]            [.] 0x000055ca7ceacc30
    +    4,17%     0,00%  clang    [unknown]            [.] 0x48487f8d48fd8948
    +    3,59%     0,00%  clang    [unknown]            [.] 0x3de907894810c083
    +    3,23%     0,00%  clang    libclang-cpp.so.10   [.] clang::VarDecl::~VarDecl
    +    3,03%     0,21%  clang    libclang-cpp.so.10   [.] clang::TreeTransform<(anonymous namespace)::TemplateInstantiator>::TransformNestedNameSpecifierLoc
    +    2,88%     0,28%  clang    libclang-cpp.so.10   [.] clang::TreeTransform<(anonymous namespace)::TemplateInstantiator>::TransformTemplateSpecializationType

This does not look very useful. Top 6 are cryptic addresses.

It's because perf uses frame pointer unwinding by default. gcc's **-O2** optimization level
avoids them (**-fomit-frame-pointer**) to generate shorter entry/exit code.

Luckily basic debugging information has a way to encode equivalent information
using **DWARF** format. **perf** knows how to unwind **DWARF** using call stack
snapshots with **--call-graph=dwarf**:

.. code-block::

    $ perf record -F 99 -g --call-graph=dwarf -- \
        /usr/lib/llvm/10/bin/clang -w -x c++ -std=c++1z -O0 -fstrict-aliasing -c A.cpp.c -o A.o
    $ perf report
      Children      Self  Command  Shared Object        Symbol
    +   11,12%     0,00%  clang    libclang-cpp.so.10   [.] (anonymous namespace)::EmitAssemblyHelper::EmitAssembly
    +   11,03%     0,00%  clang    libLLVM-10.so        [.] llvm::legacy::PassManagerImpl::run
    +    9,70%     0,09%  clang    libLLVM-10.so        [.] llvm::FPPassManager::runOnFunction
    +    9,61%     0,00%  clang    libLLVM-10.so        [.] llvm::FPPassManager::runOnModule
    +    7,00%     0,00%  clang    libLLVM-10.so        [.] 0x00007fb266cc6d56
    +    5,25%     0,00%  clang    [unknown]            [.] 0xffffffffffffffff
    +    4,00%     1,87%  clang    libLLVM-10.so        [.] llvm::FoldingSetBase::FindNodeOrInsertPos
    +    3,86%     0,12%  clang    libLLVM-10.so        [.] llvm::SelectionDAGISel::runOnMachineFunction
    +    3,86%     0,00%  clang    libLLVM-10.so        [.] 0x00007fb268996715

Most symbols are resolved now and seem to match reality.

The trick is to build profiled binaries (and their shared libraries) at
least with **-g1** compiler flags. (Something like
**CFLAGS="-O2 -g1" / CXXFLAGS="-O2 -g1"**. Note: **-g** is equivalent to **-g2**
and contains all that **-g1** has. **-g1** is the minimum level to get
stack unwinding working.

In Gentoo I'm using the following setup to build a few packages with
extra debugging:

.. code-block:: bash

    # cat /etc/portage/env/debug1.conf
    CFLAGS="${CFLAGS} -g1"
    CXXFLAGS="${CXXFLAGS} -g1"
    
    FEATURES="${FEATURES} splitdebug"
    HCFLAGS="${HCFLAGS} -g1"
    
    # needs debugedit
    FEATURES="${FEATURES} installsources"

.. code-block:: bash

    # cat /etc/portage/package.env/debug
    sys-libs/glibc     debug1.conf
    ...
    sys-devel/clang    debug1.conf

Real example
------------

Now let's try to profile something more heavyweight than **clang**,
namely **bash**. Example is taken from https://bugs.gentoo.org/688922 bug.
There **texlive-module-collection-fontsextra** package unpacks quickly
(within a minute) and then hangs up for 20 minutes doing something.

To try to figure out what that something is I ran unpack
process in one terminal, waited when unpacking finishes, and then ran
**perf** sampling for a few seconds:

.. code-block::

    # in one terminal
    $ ebuild texlive-fontsextra-2019.ebuild unpack
    ...
    >>> Unpacking texlive-module-collection-fontsextra-2019.tar.xz to /tmp/portage/dev-texlive/texlive-fontsextra-2019/work
    <seems to have hung>

.. code-block::
    
    # in another, after unpacking is seemingly done
    root # perf record -a -g --call-graph=dwarf
    # wait ~3 seconds, Ctrl-C
    ^C[ perf record: Woken up 1000 times to write data ]
    [ perf record: Captured and wrote 257,906 MB perf.data (35102 samples) ]
    
    # perf report
       Children      Self  Command          Shared Object       Symbol
    +   18,86%     0,12%  ebuild.sh        [kernel.kallsyms]   [k] entry_SYSCALL_64_after_hwframe
    +   18,56%     1,21%  ebuild.sh        [kernel.kallsyms]   [k] do_syscall_64
    +   13,13%     0,00%  swapper          [kernel.kallsyms]   [k] secondary_startup_64
    +   13,13%     0,00%  swapper          [kernel.kallsyms]   [k] cpu_startup_entry
    +   13,13%     0,02%  swapper          [kernel.kallsyms]   [k] do_idle
    +   12,48%     0,01%  swapper          [kernel.kallsyms]   [k] cpuidle_enter
    +   12,47%     0,03%  swapper          [kernel.kallsyms]   [k] cpuidle_enter_state
    +   11,92%     0,01%  swapper          [kernel.kallsyms]   [k] intel_idle
    +   11,89%    11,89%  swapper          [kernel.kallsyms]   [k] mwait_idle_with_hints.constprop.0
    +    7,71%     0,00%  ebuild.sh        libc-2.31.so        [.] __GI_munmap (inlined)
    +    6,76%     0,04%  ebuild.sh        [kernel.kallsyms]   [k] __x64_sys_munmap
    +    6,72%     0,05%  ebuild.sh        [kernel.kallsyms]   [k] __vm_munmap
    +    6,68%     0,04%  dirname          [kernel.kallsyms]   [k] entry_SYSCALL_64_after_hwframe
    +    6,55%     0,17%  ebuild.sh        [kernel.kallsyms]   [k] __do_munmap
    +    6,54%     0,55%  dirname          [kernel.kallsyms]   [k] do_syscall_64
    +    5,31%     0,00%  ebuild.sh        libc-2.31.so        [.] __GI___mmap64 (inlined)
    +    5,19%     0,04%  ebuild.sh        [kernel.kallsyms]   [k] page_fault
    +    4,74%     0,68%  ebuild.sh        libsandbox.so       [.] malloc

To make sense out of this data I used @brendangregg's FlameGraph tools (https://github.com/brendangregg/FlameGraph).
I generated interactive **.svg** files as:

.. code-block::

    $ perf script > out.perf
    $ ~/dev/git/FlameGraph/stackcollapse-perf.pl out.perf > out.folded
    $ ~/dev/git/FlameGraph/flamegraph.pl out.folded > out.svg

And got `this result (clickable and interactive!) </posts.data/215-perf/sandboxed-out.svg>`_:

.. image:: /posts.data/215-perf/sandboxed-out.svg
    :target: /posts.data/215-perf/sandboxed-out.svg

Most of profile is unrelated to our **ebuild** run and CPU time is spent on
unrelated tasks. I could have used **perf record -p $pid -g --call-graph=dwarf**
but I'd like to make sure there is no background kernel (or userspace IPC)
activity that is not a part of **ebuild** process.

Here is what we see right above the **all** bar:

- ~5% of CPU is taken by firefox playing video (Audio, Compositor, Media, Web_Content bars)
- ~9% dirname program
- ~27% ebuild.sh program
- ~29% emerge program (unrelated to ebuild.sh)
- ~3% perf itself
- ~22% swapper (idle)

Now clicking at **ebuild.sh** we see that all the time is spent preparing for
**fork()/exec()** (**malloc()**, **mmap()**, **munmap()**).

Surprisingly (or not that surprising if you are familiar with memory allocators)
**munmap()** is the heaviest operation here. The actual external program being
executed is **dirname**!

Looking at **texlive-fontsextra-2019.ebuild** definition **dirname** calls happen at
https://gitweb.gentoo.org/repo/gentoo.git/tree/eclass/texlive-module.eclass#n140.
Here is the relevant code snippet:

.. code-block:: bash

    texlive-module_src_unpack() {
        unpack ${A}
    
        grep RELOC tlpkg/tlpobj/* | awk '{print $2}' | sed 's#^RELOC/##' > "${T}/reloclist" || die
        { for i in $(<"${T}/reloclist"); do  dirname ${i}; done; } | uniq > "${T}/dirlist"
        for i in $(<"${T}/dirlist"); do
            if [[ ! -d ${RELOC_TARGET}/${i} ]]; then
                mkdir -p "${RELOC_TARGET}/${i}" || die
            fi
        done
        for i in $(<"${T}/reloclist"); do
            mv "${i}" "${RELOC_TARGET}"/$(dirname "${i}") || die "failed to relocate ${i} to ${RELOC_TARGET}/$(dirname ${i})"
        done
    }

**reloclist** contains 71000 lines. Here **dirname** happens to be an external tool
from **coreutils**. It's implementation removes everything after last trailing slash:

- `dirname:main() <https://github.com/coreutils/coreutils/blob/master/src/dirname.c>`_
- `dir_len() <https://github.com/coreutils/gnulib/blob/master/lib/dirname-lgpl.c>`_

The first workaround is to implement simplified version of it in **bash**
assuming that paths are already normalized in **reloclist**:

.. code-block:: bash

    # faster than external 'dirname' binary
    dirname() {
        echo "${1%/*}"
    }

Here is the result of
`system-wide profile after the change </posts.data/215-perf/sandboxed-out.svg>`_:

.. image:: /posts.data/215-perf/sandboxed-out-2.svg
    :target: /posts.data/215-perf/sandboxed-out-2.svg

**dirname** binary disappeared completely and **mv** popped up (don't mind unrelated
**cc1plus** binary). That means we are in the second **for** loop of **texlive-module_src_unpack()**
**bash** function.

The **dirname()** shell builtin we just added allows cutting down unpack
time from 17 minutes to 11 minutes (1.5x speedup).

We can also shrink **mv** process creations overhead down to one per target directory:

.. code-block:: diff

    --- a/eclass/texlive-module.eclass
    +++ b/eclass/texlive-module.eclass
    @@ -137,18 +137,22 @@ S="${WORKDIR}"
    
     RELOC_TARGET=texmf-dist
    
    +dirname() {
    +       echo "${1%/*}"
    +}
    +
     texlive-module_src_unpack() {
            unpack ${A}
    
            grep RELOC tlpkg/tlpobj/* | awk '{print $2}' | sed 's#^RELOC/##' > "${T}/reloclist" || die
    -       { for i in $(<"${T}/reloclist"); do  dirname ${i}; done; } | uniq > "${T}/dirlist"
    +       { for i in $(<"${T}/reloclist"); do  dirname ${i}; done; } | sort | uniq > "${T}/dirlist"
            for i in $(<"${T}/dirlist"); do
                    if [[ ! -d ${RELOC_TARGET}/${i} ]]; then
                            mkdir -p "${RELOC_TARGET}/${i}" || die
                    fi
            done
    -       for i in $(<"${T}/reloclist"); do
    -               mv "${i}" "${RELOC_TARGET}"/$(dirname "${i}") || die "failed to relocate ${i} to ${RELOC_TARGET}/$(dirname ${i})"
    +       for i in $(<"${T}/dirlist"); do
    +               mv $(egrep "^${i}/[^/]+$" "${T}/reloclist") "${RELOC_TARGET}/${i}/" || die "failed to relocate to ${RELOC_TARGET}/${i}"
            done
     }

That cuts it further down from 11 minutes to 30 seconds. That is **22x** speedup
from previous state, **34x** from initial state.

While it's not the best solution I think it's a good enough proof of concept
to get the idea what gains we can potentially have here.

Better solution would probably be a **perl** or **python** one-liner to
perform similar mass **mkdir**/**mv**. It would also eliminate rest of
per-directory **fork()**s we still have. Should be doable in 20 minutes!

Fork speed
----------

I always wondered what is the actual overhead of **fork()**/**exec()** sequence.
I would love it to be a function of target process size (or even better be
a small constant). But what does happen in reality?

Kernel has to copy much of process' metadata anyway. At the very least
all the page tables have to be copied. These are visible in above perf graphs
if we click through: **sbuild.sh** > **__libc__fork** > ... **_do_fork** > **copy_process** > **copy_page_range**.

We can double-check `copy_page_range() <https://github.com/torvalds/linux/blob/63bef48fd6c9d3f1ba4f0e23b4da1e007db6a3c0/mm/memory.c#L967>`_
definition.

But maybe it's a negligible part of normal system operation? Let' get some
intuition by looking at a simple benchmark.

Benchmark will gradually increase
host **bash** process size with environment variables and check
**fork()**/**exec()** performance on a tiny **/bin/true** binary.

We measure 1000 runs of a small **/bin/true** binary out of a big process.

.. code-block:: bash

    $ for env_mb in 0 1 10 100 250 500 1000; do
        env_var=$(python -c "print('A'*1024*1024*$env_mb);")
        echo "Benchmarking $env_mb MB"
        time { for i in `seq 1 1000`; do /bin/true; done; }
    done
    
    Benchmarking 0 MB
    real 0m0,571s user 0m0,302s sys 0m0,326s
    
    Benchmarking 1 MB
    real 0m0,648s user 0m 0,327s sys 0m0,379s
    
    Benchmarking 10 MB
    real 0m1,099s user 0m0,285s sys 0m0,871s
    
    Benchmarking 100 MB
    real 0m3,223s user 0m0,162s sys 0m3,112s
    
    Benchmarking 250 MB
    real 0m12,777s user 0m0,192s sys 0m12,635s
    
    Benchmarking 500 MB
    real 0m23,782s user 0m0,202s sys 0m23,632s
    
    Benchmarking 1000 MB
    real 0m45,248s user 0m0,203s sys 0m45,097s

**fork()/exec()** performance degrades quickly (linear) with host process size.
It starts from 500 microseconds on a default interactive **bash** process
and degrades down to 45 milliseconds on a 1GB process (100x slowdown).

**bash** process size also relates to our original example: in case of
**texlive-module.eclass** eclass the expression

.. code-block:: bash

    $(<"${T}/reloclist")

pulls in 4MB file into **bash** process. That alone slows process creation down
at least by half.

Something like:

.. code-block:: bash

    while read ...; do
        ...
    done < "${T}/reloclist"

would probably make it 50% faster.

Parting words
-------------

- **perf** is not that complicated to use. Give it a try!
- **perf record -g \-\-call-graph=dwarf** can extract call stacks from optimized binaries.
- Flame graphs are nice :)
- Process **fork()**/**exec()** is not cheap and spends most of time creating and destroying page tables.
  Eliminating heavyweight process creation can easily be a 30x performance speedup.
- **bash** should consider using **vfork()** and/or **posix_spawn()** (I assume it does not yet do it).

Have fun!
