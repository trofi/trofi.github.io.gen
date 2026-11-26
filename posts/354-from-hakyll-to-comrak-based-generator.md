+++
title = "From hakyll to comrak based generator"
date = "November 15, 2026"
tags = ["blog", "rust", "comrak"]
+++

Tl;DR: I decided to gradually switch from
[`hakyll-`based framework](https://jaspervdj.be/hakyll/) for this blog
to [`comrak`-based](https://comrak.ee/) custom generator.

I'll migrate both `HTML` render of posts and `RSS` feeds in the following
few days. You should not see any changes: all the URLs and UUIDs are the
same before and after. Until I missed something crucial. I hope I did not.

The one exception will be the style of syntax highlighting in the `HTML`
render of posts (and it will probably keep changing for a while more
when I experiment with various backends).

## Background

I used `hakyll` for about 13 years at least since
[first commit](https://github.com/trofi/trofi.github.io.gen/commit/7ed816cf5515a47703f8cb2c804244a569bba30f)
from 25 November 2013. It allowed me to bring up the trivial static blog
site on [GitHub Pages](https://docs.github.com/en/pages) after I migrated
WordPress-style platform where I used [`BlogLiterately`](https://byorgey.wordpress.com/blogliterately/).

`haskell` was my primary language to hack on hobby projects and `hakyll`
was natural fit: it used `pandoc` to read `rst`, `markdown` and many
other formats, it was able to render `RSS` feed.

Over time I added small features to `hakyll`-based generator like
[`graphviz`](/posts/300-inline-graphviz-dot-in-hakyll.html)
and [`gnuplot`](/posts/318-inline-gnuplot.html) renders.

I had a few minor issues with `hakyll`:

- [`issue #610`](https://github.com/jaspervdj/hakyll/issues/610): no
  support for multiple output files from a single rule
- no simple recipes of adding per-tag RSS feeds
- my lack of understanding of `hakyll` internals to add more advanced
  custom pages
- my dwindled interest in `haskell` ecosystem
- relatively slow site generation: about 10 seconds for my small input

## My Attempt

I wanted to give `rust` a try. But I was not able to find a close
equivalent to `hakyll` in `rust` land. I thought of building something
similar: have arbitrary build rules for mostly `markdown` inputs.

I explored a few libraries that could parse `markdown` into `AST`, allow
its mutation and render into `HTML`. I stopped on [`comrak`](https://comrak.ee/)
and was pleasantly surprised by its [API](https://docs.rs/comrak/latest/comrak/):
It gave me everything I wanted. `comrak` has a ton of optional `markdown`
extension on top of `CommonMark`.

### `comrak` As `markdown` Render

`comrak` did not support tables understood by `pandoc` and I had to
[convert](https://github.com/trofi/trofi.github.io.gen/commit/e0d8ea815e353bef8702e1a2f3ed4b50a84e1c2f)
a few posts to more standard table extension. It also did not support
`~~~ { .haskell }` style of highlighting, I
[converted](https://github.com/trofi/trofi.github.io.gen/commit/a7cbeca76b2fa07039891fc37d79bb0aa5a6cc2d)
such posts as well.

After that I got almost identical `HTML` render out of `markdown` files
I had. A few exceptions were `"` quotes, `'` and `...` where `hakyll` was
more eager to use modern Unicode characters. `comrak` does not add as
much of newlines between `HTML` tags.

### `maud` As `HTML` Template Library

On templating side I was considering using [`tera`](https://docs.rs/tera/latest/tera/)
to get [`jinja`-style](https://en.wikipedia.org/wiki/Jinja_(template_engine))
templates. But eventually leaned towards in-code templates using
[`maud`](https://maud.lambda.xyz/). `maud` has more natural way of
embedding arbitrary `rust` code into templates.

My biggest template looks [this way](https://github.com/trofi/trofi.github.io.gen/blob/113363111eddd1236560cec2045371157d12cf22/src/template.rs).

### `lol_html` As an `HTML` Rewriter

`hakyll` was handling internal URL format conversion into relative
(`HTML`) and into absolute (`RSS`) URLs. It took me some time to find a
library that both parses `HTML` and allows its mutation via simple API.
`lol_html` does not look too terrible.

My initial attempt at rewriting `<img src=` and `<a href=` links looks
[this way](https://github.com/trofi/trofi.github.io.gen/blob/113363111eddd1236560cec2045371157d12cf22/src/rewrite.rs).

### Other Libraries

A few other libraries I used are:

- `math-core`: as `comrak` was not able to handle `latex-style` formulas
  I used `math-core` to do build-time conversion.
- `serde` / `serde_yaml`: parse post metadata
- `thiserror`: ergonomic application-wide errors
- `url`: parse and normalize URIs
- `glob`: expand paths like `"pages/*.md` as input to the generator
- `rss` and `atom_syndication`: build `RSS 2.0` / `Atom 1.0` feeds

### The Result

After about 2 weeks of evening I came up with something that generates
very close `HTML` to the original (except the syntax highlighting).
And this is my attempt as a [single commit](https://github.com/trofi/trofi.github.io.gen/commit/113363111eddd1236560cec2045371157d12cf22).

The whole site is built in 2 seconds (compared to `hakyll`-based 10
seconds).

There is no interactive reload /rebuild support for it. For me
the `inotifywait` / `cargo run --release` is enough to implement the
equivalent.

Have fun!
