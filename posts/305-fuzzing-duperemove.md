---
title: "fuzzing duperemove"
date: November 21, 2023
root: "http://trofi.github.io"
---

[`duperemove-0.14`](https://github.com/markfasheh/duperemove/releases/tag/v0.14)
was released yesterday and included a few small fixes I wrote about
[before](/posts/304-duperemove-speedups.html).

On top of that the new release contains an overhauled parallel file
scanner and database handler that scale a lot better on large files.

## new crashes

Unfortunately recent changes also increased complexity of handling
deduplication queue in a way that caused occasional crashes like 
asserts in `dedupe_extent_list()` at
[`run_dedupe.c:448`](https://github.com/markfasheh/duperemove/issues/329).

In that case my typical dedupe run started crashing as:

```
# duperemove -q --batchsize=0 --dedupe-options=partial,same -rd --hashfile=/run/duperemove/root-dupes.db /
...
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
ERROR: run_dedupe.c:287
[stack trace follows]
/nix/store/528cmbj5wnz37llwkfwxjxj3j41ng0gi-duperemove-0.14/bin/duperemove(print_stack_trace+0x2e) [0x409aee]
/nix/store/528cmbj5wnz37llwkfwxjxj3j41ng0gi-duperemove-0.14/bin/duperemove() [0x40d2dc]
/nix/store/7wkspba8d5i28bw0jfxyi3c70wrw2512-glib-2.76.4/lib/libglib-2.0.so.0(+0x8b06a) [0x7f906e45006a]
/nix/store/7wkspba8d5i28bw0jfxyi3c70wrw2512-glib-2.76.4/lib/libglib-2.0.so.0(+0x8a71d) [0x7f906e44f71d]
/nix/store/qn3ggz5sf3hkjs2c797xf7nan3amdxmp-glibc-2.38-27/lib/libc.so.6(+0x8b084) [0x7f906e038084]
/nix/store/qn3ggz5sf3hkjs2c797xf7nan3amdxmp-glibc-2.38-27/lib/libc.so.6(+0x10d60c) [0x7f906e0ba60c]

/nix/store/b86jv7hh4656xf60mby91w7a93wi4h03-remove-dupes.bash: line 12: 405268 Aborted                 (core dumped) SQLITE_TMPDIR=/run/duperemove /nix/store/528cmbj5wnz37llwkfwxjxj3j41ng0gi-duperemove-0.14/bin/duperemove -q --batchsize=0 --dedupe-options=partial,same -rd --hashfile=/run/duperemove/root-dupes.db /

duperemove-root.service: Main process exited, code=exited, status=134/n/a
duperemove-root.service: Failed with result 'exit-code'.
Failed to start duperemove-root.service.
duperemove-root.service: Consumed 34min 12.755s CPU time, no IP traffic.
```

It took 30 minutes of CPU time (and about 10 minutes of real time) to
only crash later.

## crash location

I poked a bit around the crash in `gdb` to find that failure happens
somewhere in the middle of [`dedupe_extent_list()`](https://github.com/markfasheh/duperemove/blob/v0.14/run_dedupe.c#L274).
It's a seemingly simple but big function:

```c
static int dedupe_extent_list(struct dupe_extents *dext, uint64_t *fiemap_bytes,
                              uint64_t *kern_bytes, unsigned long long passno)
{
    int last = 0;
    struct dedupe_ctxt *ctxt = NULL;
    // ...
    list_for_each_entry(extent, &dext->de_extents, e_list) {
        if (list_is_last(&extent->e_list, &dext->de_extents))
            last = 1;
        // ...
        if (...) {
            // ...
            if (ctxt && last)
                goto run_dedupe;
            continue;
        }
        if (ctxt == NULL) {
            ctxt = new_dedupe_ctxt(dext->de_num_dupes,
                                   tgt_extent->e_loff, len,
                                   tgt_extent->e_file);
            // ...
            if (tgt_extent == extent)
                continue;
            // ...
        }
        // ...
        if (...) {
            // ...
            if (!last)
                continue;
        // ...
run_dedupe:
        // ...
close_files:
        filerec_close_open_list(&open_files);
        free_dedupe_ctxt(ctxt);
        ctxt = NULL;
        // ...
    }
    // ...
    abort_on(ctxt != NULL); // we fail here
    // ...
}
```

`duperemove` fails at `abort_on(ctxt != NULL);` assertion. The intent
of the assert seems straightforward: `ctxt` is expected to be created
within `list_for_each_entry()` loop and is destroyed before we exit the
loop.

There is a bit of logic that tries to track if we are in the last
element of the loop to make sure we clean up properly.

From a quick glance I was not able to figure out why `duperemove` crashes
on my input. What is worse: running `duperemove` with
`--io-threads=1 --cpu-threads=1` options to decrease parallelism and to
simplify deduplication sequence started triggering an unrelated
`abort_on()` in the same function.

That means there not just one but a few different cases that manage to
break `duperemove`.

## building a reproducer

As the scan takes at least 10 minutes on my input data I wondered if I
could extract a smaller example to present for upstream.

At first I though of crafting the files on the file system in a
particular way to match the way `duperemove` breaks for me. But I also
felt it would be a tedious task.

Before actually trying to extract the first crash example I got an idea
of fuzzing `duperemove`. In theory a simple sequence of random
actions against a file system to create interesting enough file state
would be able to create a lot more interesting scenarios than I have.
Maybe I'll get something that crashes `duperemove` faster?

I though if the following operations for the fuzzer:

1. create a brand new file with a few (`4`) unique non-dedupable blocks
2. copy full existing file contents with or without reflinking into a new file
3. copy one random block from one random file to another random file
4. run `duperemove` on the current state

**Quick quiz**: If we execute these actions at random, how long would it
take to crash `duperemove`? A second, a minute, a day or never?

Here is the direct `bash` implementation of the fuzzer described above:

```bash
#!/usr/bin/env bash

duperemove_binary=$1
target_dir=$2

shift; shift

if [[ -z $duperemove_binary ]] || [[ -z $target_dir ]]; then
    echo "Usage: $0 </abs/path/to/duperemove> <directory> [duperemove opts]"
    exit 1
fi

# fail on any error
set -e

mkdir "$target_dir"
cd "$target_dir"

shopt -s nullglob

while :; do
    sync
    files=(*)
    f_count=${#files[@]}
    dst=$f_count

    case $((RANDOM % 4)) in
        0)  # copy existing file
            [[ $f_count -eq 0 ]] && continue

            cp_arg=""
            case $((RANDOM % 2)) in
                0) cp_arg=--reflink=always;;
                1) cp_arg=--reflink=never;;
            esac
            src=$((RANDOM % f_count))
            cp -v "$cp_arg" "$src" "$dst"
            ;;
        1) # create new file of 4x4KB distinct blocks
            printf "0%*d" 4095 "$dst"  > "$dst"
            printf "1%*d" 4095 "$dst" >> "$dst"
            printf "2%*d" 4095 "$dst" >> "$dst"
            printf "3%*d" 4095 "$dst" >> "$dst"
            ;;
        2) # run duperemove
            "$duperemove_binary" "$@" -rd -b 4096 "$target_dir"
            ;;
        3) # dd 4KB of one file into another
            [[ $f_count -eq 0 ]] && continue

            src=$((RANDOM % f_count))
            dst=$((RANDOM % f_count))
            [[ $src = $dst ]] && continue

            src_block=$((RANDOM % 3))
            dst_block=$((RANDOM % 3))
            dd "if=$src" "iseek=$src_block" "of=$dst" "oseek=$dst_block" bs=4096 count=1
            ;;
    esac
done
```

Specifically we always create `16KB` files and move `4KB` blocks around
to make sure `duperemove` considers them as a whole. We also pass
`-b 4096` block size as default block size is `128KB`.

And now goes the quiz answer:

```
$ time { rm -rfv ~/tmp/dr/ && bash ./duperemove-fuzz.bash $PWD/duperemove/duperemove ~/tmp/dr -q; }
...
Simple read and compare of file data found 12 instances of files that might benefit from deduplication.
ERROR: run_dedupe.c:287
[stack trace follows]
/home/slyfox/dev/git/duperemove/duperemove(print_stack_trace+0x29) [0x409b39]
/home/slyfox/dev/git/duperemove/duperemove() [0x40d844]
/nix/store/6bpc4sc5apc2ryxhjyij43n3wi169hfn-glib-2.76.4/lib/libglib-2.0.so.0(+0x8ad72) [0x7f8da7b47d72]
/nix/store/6bpc4sc5apc2ryxhjyij43n3wi169hfn-glib-2.76.4/lib/libglib-2.0.so.0(+0x8a41d) [0x7f8da7b4741d]
/nix/store/znf2bj54q3qj4pyk0xbp7pk12xbxq07b-glibc-2.38-27/lib/libc.so.6(+0x908b1) [0x7f8da77278b1]
/nix/store/znf2bj54q3qj4pyk0xbp7pk12xbxq07b-glibc-2.38-27/lib/libc.so.6(+0x114e6c) [0x7f8da77abe6c]
./duperemove-fuzz.bash: line 27: 264572 Aborted                 (core dumped) "$duperemove_binary" "$@" -rd -b 4096 "$target_dir"

real    0m3.354s
user    0m0.530s
sys     0m1.500s
```

3 seconds!

I'm not always as lucky: sometimes it takes 2 seconds, sometimes as much
as 8 seconds. It feels like a very good result for such a dumb script.

## parting words

`duperemove` has a non-trivial state machine to track extent state to
avoid multiple deduplication attempts. It clearly has a few bugs like
[issue #329](https://github.com/markfasheh/duperemove/issues/329).

Fuzzing works great for a small set of well defined operations. I picked
a minimal subset of operations to trigger the failures.

The fuzzer does not exercise other interesting operations like hardlinks
creation, file removal or larger files with more interesting extent
sharing structure. There is still more room for improvement to get into
darker corners of state tracking in `duperemove`.

I will not have much time debugging specifics of these `duperemove`
crashes in the following days. Try to fix these crashes yourself!

Have fun!
