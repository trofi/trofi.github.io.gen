---
title: doxygen for lazies
date: February 2, 2016
---

## `TL;DR`

To get `doxygen` docs really fast you can run the
[`doxify`](https://github.com/trofi/home/blob/master/bin/doxify) script
as:

```
$ doxify path-to-project path-to-docs
# Done!
$ firefox path-to-docs/index.html
```

## more words

Let's do the same for a small yet complex real-world C++ project: `re2c`.
The session looks like:

```
$ git clone https://github.com/skvadrik/re2c
$ doxify re2c/re2c/src re2c_docs
warning: Output language Russian not supported! Using English instead.
warning: failed to open layout file 'DoxygenLayout.xml' for reading!
warning: Included by graph for 'c99_stdint.h' not generated, too many nodes. Consider increasing DOT_GRAPH_MAX_NODES.
$ firefox re2c_docs/index.html
```

The docs generation process takes 9 seconds on my machine. I've
uploaded result [here](/posts.data/190-doxy/re2c_docs/index.html). I
won't update that documentation thus it's frost in time.

By default `doxygen` generates documentation only for explicitly
documented files and functions thus we override that behavior and force
it to tell us everything it knows about the project. The following
`doxify` line is responsible for it:

``` bash
doxygen_set_value "EXTRACT_ALL" "YES"
```

Most of the rest is a nicety to get a code browser inline with
documentation. Let's look at actually generated stuff. There is:

- [Namespaces](/posts.data/190-doxy/re2c_docs/namespaces.html)
- [Classes](/posts.data/190-doxy/re2c_docs/classes.html)
- [Files](/posts.data/190-doxy/re2c_docs/files.html)

Under **Classes** there is **Class Hierarchy** drop down. Some complex
projects have huge class hierarchy. One click and you know `re2c` is
[not one of them](/posts.data/190-doxy/re2c_docs/inherits.html).

Under **Files** we can find a lot of useful info as well. Picking
[`main.cc`](/posts.data/190-doxy/re2c_docs/main_8cc.html) as an example:

- Transitive header inclusion graph. If you click on a header you'll
  get both direct and reverse inclusion graphs:
  [`input_api.h`](/posts.data/190-doxy/re2c_docs/input__api_8h.html)
- Highlighted function definition with clickable cross-references:
  [`main()`
  function](/posts.data/190-doxy/re2c_docs/main_8cc.html#a97b0fa62b7b0972875f5f589322c4c24)
- Original source code with highlight fancy and cross-reference links:
  [`main()`
  again](/posts.data/190-doxy/re2c_docs/main_8cc_source.html#l00026)

More random examples:

- [`re2c::Output`](/posts.data/190-doxy/re2c_docs/structre2c_1_1Output.html)
- clicking at the
  [legend](/posts.data/190-doxy/re2c_docs/graph_legend.html) under any
  graph will decipher arrow colors
- `\~Output()` destructor has a [nice
  call graph](/posts.data/190-doxy/re2c_docs/structre2c_1_1Output.html#a1cc81b46a98f3ada41135bb395df0c55)
- `re2c::matches` does not call anyone but is called occasionally:
  [its reverse
  call graph](/posts.data/190-doxy/re2c_docs/namespacere2c.html#a79ad7b02c4996a9bab41faabd451d624)

Have fun!
