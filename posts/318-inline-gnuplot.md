---
title: "inline gnuplot"
date: June 22, 2024
root: "http://trofi.github.io"
---

Time to time I find myself needing to plot histograms and approximations
in occasional posts.
Similar to [inline `graphviz`](/posts/300-inline-graphviz-dot-in-hakyll.html)
support today I added `gnuplot` `svg` inlining support into this blog.
The trivial example looks this way:

```{render=gnuplot}
plot [-pi:pi] sin(x)
```

The above is generated using the following `.md` snippet:

````
```{render=gnuplot}
plot [-pi:pi] sin(x)
```
````

`hakyll` [integration](https://github.com/trofi/trofi.github.io.gen/commit/4fb830628c6923873c0b21b2ac444a73d4d47cee)
is also straightforward:

```haskell
inlineGnuplot :: TP.Block -> Compiler TP.Block
inlineGnuplot cb@(TP.CodeBlock (id, classes, namevals) contents)
  | ("render", "gnuplot") `elem` namevals
  = TP.RawBlock (TP.Format "html") . DT.pack <$> (
      unixFilter "gnuplot"
          [ "--default-settings"
          , "-e", "set terminal svg"
          , "-"]
          (DT.unpack contents))
inlineGnuplot x = return x
```

Here we call `gnuplot --default-settings -e "set terminal svg" -` and
pass our script over `stdin`. Easy!
For those who wonder what `gnuplot` is capable of have a look at
[`gnuplot.info` demo page](http://www.gnuplot.info/demo_svg_4.6/).
As a bonus here is the time chart of my commits into `nixpkgs`:

```{render=gnuplot}
set term svg
set xdata time
set timefmt "%Y-%m"
set format x "%Y-%m"

set grid

plot 'posts.data/318-inline-gnuplot/commits.dat' \
  using 1:2:(60*60*24*28) \
  with boxes \
  fillstyle solid 0.8 fillcolor "#000080" \
  title "monthly commits"
```

Have fun!
