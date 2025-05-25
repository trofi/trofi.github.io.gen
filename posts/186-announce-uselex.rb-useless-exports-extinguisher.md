---
title: "ANNOUNCE: uselex.rb - useless exports extinguisher"
date: July 7, 2013
---

Ladies and gentlemen! Welcome new (tiny) tool to find another kind of
`C`/`C++` code deficiency: needlessly externally visible symbols. Usage:

```
$ uselex.rb `find /path/to/project/build/dir/ -name '*.o'`
```

It lives [here](https://github.com/trofi/uselex).

Let's consider simple example:

```c
// main.c
#include <stdio.h>
int add_a_to_b (int a, int b)
{
    return a + b;
}
int main (int argc, char * argv[])
{
    return add_a_to_b (argc, 4);
}
```

and build it:

``` asm
$ gcc -O2 -c main.c
$ objdump -d a.o
0000000000000000 <add_a_to_b>:
0:   8d 04 37                lea    (%rdi,%rsi,1),%eax
3:   c3                      retq
Disassembly of section .text.startup:
0000000000000000 <main>:
0:   8d 47 04                lea    0x4(%rdi),%eax
3:   c3                      retq
```

See that useless piece of generated code? It's `add_a_to_b`. To
make sure we don't need it we should add `static` keyword to it.
(provided nobody from another module uses that function as well
obviously).

This

```c
// main.c
#include <stdio.h>
static int add_a_to_b (int a, int b)
{
    return a + b;
}
int main (int argc, char * argv[])
{
    return add_a_to_b (argc, 4);
}
```

will give

```asm
0000000000000000 <main>:
0:   8d 47 04                lea    0x4(%rdi),%eax
3:   c3                      retq
```

As you see a chunk of `.text` section gone away. In many cases it
helps in more aggressive inlining. Large programs have a ton of
uselessly exported stuff: global variables, global functions, global
constants. They are usually used only in one place (even if they were
used in many places long ago).

I've decided to try to write small tool to find such unused exports
`uselex.rb`:

```
$ uselex.rb main.o
add_a_to_b: [R]: exported from: a.o
```

Caught the guy. Let's try for more advanced projects:

```
~/dev/git/btrfs-progs $ uselex.rb `find -name '*.o'`

write_all_supers: [R]: exported from: ./disk-io.o
btrfs_lookup_dir_index_item: [R]: exported from: ./dir-item.o
update_seeding_flag: [R]: exported from: ./btrfstune.o
enable_extrefs_flag: [R]: exported from: ./btrfstune.o
write_tree_block: [R]: exported from: ./disk-io.o
receive_cmd_group: [R]: exported from: ./cmds-receive.o
radix_tree_delete: [R]: exported from: ./radix-tree.o
...
```

Not all of them are problems. `btrfs-progs` exports a library, thus
you need to ignore functions exported from installable headers.

And the `linux` kernel for stress test:

```
~/linux-2.6-mytree $ time uselex.rb `find -name '*.o'` | wc -l

3808
real    0m15.971s
user    0m7.526s
sys     0m7.070s
```

You can read some implementation details in help output for `uselex`.

Thanks for your patience!
