---
title: "visualising python profiles with flameprof"
date: April 11, 2020
---

## Tl;DR

<https://github.com/baverman/flameprof/> is a great single-file
converter from `python`'s profile file format to **SVG** flame graphs.

## profiling emerge

`emerge` is known to be slow occasionally. I always wondered why
exactly but could never quite get to it.
A few years go I tried
<http://www.vrplumber.com/programming/runsnakerun/> once with some
success. It seems it was not ported to `python-3` and I found
<https://github.com/baverman/flameprof/>.
Example ebuild for Gentoo is
[here](https://github.com/trofi/slyfox-gentoo/blob/master/dev-util/flameprof/flameprof-0.4.ebuild).
Let's grab the profile:

``` 
$ cd ~/dev/git/portage
$ python -m cProfile -o emerge.prof \
    \
    ./bin/emerge -pvuDN @world @preserved-rebuild --with-bdeps=y --complete-graph \
    --changed-deps --verbose-conflicts --backtrack=10000

# wait 5 minutes
^Ctrl-C
Exiting on signal Signals.SIGINT
```

``` 
$ flameprof emerge.prof > emerge.svg
```

Generated profile graph is [here](/posts.data/216-flameprof/emerge.svg).
[![image](/posts.data/216-flameprof/emerge.svg)](/posts.data/216-flameprof/emerge.svg)

If you spot something odd you can try to tweak it! I'll try as well.

Have fun!
