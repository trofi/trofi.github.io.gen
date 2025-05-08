---
title: "sort by key in coreutils"
date: May 8, 2025
---

This post is about `sort` tool from `GNU coreutils`. Until today I
foolishly thought that to sort a file by a second (and just second
column) you just need to use `sort -k2` option.

Indeed that does seem to work for a simple case:

```
$ printf "1 2\n2 1\n"
1 2
2 1
```

```
$ printf "1 2\n2 1\n" | sort -k2
2 1
1 2
```

But today I attempted a slightly more complicated sort by sorting commit
history:

```
abcd foo: commit z
bcde bar: commit a
cdef foo: commit y
defg bar: commit b
```

I wanted to sort these by the `foo:` / `bar:` component while preserving
the order of commit within the component. I want this outcome:

```
bcde bar: commit a
defg bar: commit b
abcd foo: commit z
cdef foo: commit y
```

How do you achieve that? My naive attempt was to use `sort -k2 --stable`:

```
$ sort -k2 --stable <l
bcde bar: commit a
defg bar: commit b
cdef foo: commit y
abcd foo: commit z
```

Note how `foo:` commits were unexpectedly reordered.

Quick quiz: why did excessive reorder happen? How to fix it?

## The answer

`sort --help` has an answer by describing what `-k2` key selection
actually does. But `--debug` option is even better at illustrating what
is being compared. Let's use that:

```
$ LANG=C sort -k2 --stable --debug <l
sort: text ordering performed using simple byte comparison
sort: leading blanks are significant in key 1; consider also specifying 'b'
bcde bar: commit a
    ______________
defg bar: commit b
    ______________
cdef foo: commit y
    ______________
abcd foo: commit z
    ______________
```

The `______________` underscore shows the actual compared key: it's not
just `foo:` or `bar:`. It's the whole line that starts at the
whitespace right before `foo:` and `bar:`. The fix is to tweak the selector:

```
$ LANG=C sort -k2,2 --stable --debug <l
sort: text ordering performed using simple byte comparison
sort: leading blanks are significant in key 1; consider also specifying 'b'
bcde bar: commit a
    _____
defg bar: commit b
    _____
abcd foo: commit z
    _____
cdef foo: commit y
    _____
```

or with `-b` if leading spaces look confusing:

```
$ LANG=C sort -k2,2 --stable --debug -b <l
sort: text ordering performed using simple byte comparison
bcde bar: commit a
     ____
defg bar: commit b
     ____
abcd foo: commit z
     ____
cdef foo: commit y
     ____
```

This way the sorting works as expected:

```
$ LANG=C sort -k2,2 --stable -b <l
bcde bar: commit a
defg bar: commit b
abcd foo: commit z
cdef foo: commit y
```

## parting words

`sort -k` is tricky: it's not a field number but a field range. `--debug`
option is great at showing used sorting key (or keys of `--stable` is not
used).

Have fun!
