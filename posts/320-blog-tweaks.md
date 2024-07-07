---
title: "blog tweaks"
date: July 06, 2024
---

Tl;DR:

A few changes happened to this blog in the past few weeks:

- `RSS` feed and web pages no longer embed `svg` images into `<html>`
  and include them via `<img src="...">`.

  This fixes `RSS` readers like `miniflux` but might break others. At
  least now there should be an icon in place of a missing picture
  instead of just stripped tags.

  As a small bonus `RSS` feed should not be as large to download.

- `RSS` feed now includes source code snippets without syntax
  highlighting.

  I never included `css` style into `rss` feed. `highlighting-kate` uses
  various tags and decorates them with links heavily. This change fixes
  source code rendering in `liferea`.

- `RSS` feed now embeds `https://` self-links instead of `http://`
  (except for a few recent entries to avoid breaking reading history).

More words:

I started this blog in 2010. In 2013 I moved it to
[`hakyll`](https://jaspervdj.be/hakyll/) static site generator. The
initial version was just
[88 lines of `haskell` code](https://github.com/trofi/trofi.github.io.gen/blob/7ed816cf5515a47703f8cb2c804244a569bba30f/src/site.hs).

I did not know much about `hakyll` back then and I kept it that way for
about 10 years: it just worked for me. The only thing I missed were
tag-based `RSS` feeds and article breakdown per tag. It prevented the
blog from being added to thematic `RSS` aggregators like
[`Planet Gentoo`](https://planet.gentoo.org/). But it was not a big deal.
I though I would add it "soon" and never did.

The only "non-trivial" tweaks I did were
[`dot` support](/posts/300-inline-graphviz-dot-in-hakyll.html)
and [`gunplot` support](/posts/318-inline-gnuplot.html).

Fast forward to 2024 few weeks ago I boasted to my friend how cool my
new `gnuplot` embeddings are. To what the response was "What pictures?".
Apparently `miniflux` does not like `<svg>` tags embedded into `<html>`
and strips them away leaving only bits of `<title>` tags that almost
looks like original `graphviz` input :)

That meant my cool hack with `svg` embedding did not quite work for
`RSS` feed. I moved all the embeddings into separate `.svg` files with
[this change](https://github.com/trofi/trofi.github.io.gen/commit/12812bab87ce4bdff91227527d543ee3ac2161a9).

It's not a big change, but it does violate some `hakyll` assumptions.
Apparently `hakyll` can output only one destination file for a source
file. For example `foo.md` can only produce `foo.html` and not `foo.html`
plus indefinite amount of pictures. There is a
[version support](https://jaspervdj.be/hakyll/tutorials/06-versions.html)
in `hakyll`, but it assumes that we know number of outputs upfront. It's
not really usable for cases like `N` unknown outputs from an input. To
work it around I'm writing all the auxiliary files without the `hakyll`
dependency tracker knowledge. I do it by defining `Writable` instance:

```haskell
data PWI = PWI {
    pandoc :: H.Item String
  , inlines :: [(String, H.Item DBL.ByteString)]
} deriving (GG.Generic)

deriving instance DB.Binary PWI

instance H.Writable PWI where
    write path item = do
        -- emit page itself:
        let PWI pand inls = H.itemBody item
        H.write path pand
        -- emit inlines nearby:
        CM.forM_ inls $ \(fp, contents) -> do
            H.makeDirectories fp
            H.write fp contents
```

Here `inlines` is the list of pairs of filenames and their contents to
write on disk and `pandoc` is the primary content one would normally
write as `H.Item String`.

While at it I disabled syntax highlighting in `RSS` feed as `liferea`
rendered highlighted source as an unreadable mess. And `miniflux` just
stripped out all the links and styles. [The change](https://github.com/trofi/trofi.github.io.gen/commit/1dc9d5a9d6b54db928f3fdaef1c0dcb4b6d567df)
is somewhat long, but it's gist is a single extra `writerHighlightStyle`
option passed to `pandoc` render:

```haskell
pandocRSSWriterOptions :: TPO.WriterOptions
pandocRSSWriterOptions = pandocWriterOptions{
    -- disable highlighting
    TPO.writerHighlightStyle = Nothing
}
```

The last thing I changed was to switch from `http://` links to
`https://` links by default. In theory it's a
[one-character change](https://github.com/trofi/trofi.github.io.gen/commit/cfc80bb575c1b131225c43c1fed47ff639540bd9).
In practice that would break unread history for all `RSS` users. I worked
it around by restoring `http://` root link for current `RSS` entries
with [metadata change](https://github.com/trofi/trofi.github.io.gen/commit/6b1883a1b23f6965314bfd2b55cb3e9e6a42ec16).

That way all new posts should contain `https://` root links and all
site-local links should automatically become `https://` links.

Still no tag support. Maybe later.

Have fun!
