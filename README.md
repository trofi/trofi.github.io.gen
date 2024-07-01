Hello!

This is the source of <https://trofi.github.io/> blog.

# Navigating

- [`posts/`](./posts): posts source code in `.rst` or `.markdown` format.
- [`posts.data/`](./posts.data): post-specific images, example binaries and similar.
- [`src/`](./src): source code for the site generator itself.
- [`run_watcher.sh`](./run_watcher.sh): main script to generate site and run a preview web server locally.
- [`trofi.github.io`](https://github.com/trofi/trofi.github.io/): repository in
  [GitHub pages](https://pages.github.com/) format with auto-generated content.

# Trying it out

Just clone this repo and run a [`run_watcher.sh`](./run_watcher.sh) and see what happens:

```
# optional, start a `nix` developmen shell to pull in all dependencies:
$ nix develop --no-write-lock-file

$ git clone https://github.com/trofi/trofi.github.io.gen
$ cd trofi.github.io.gen/
$ nix develop --no-write-lock-file
$ ./run_watcher.sh
...
[1 of 1] Compiling Main             ( Setup.hs, Setup.o )
Linking ./Setup ...
Configuring mksite-0.0.0.0...
Preprocessing executable 'trofi.github.io.gen' for mksite-0.0.0.0..
Building executable 'trofi.github.io.gen' for mksite-0.0.0.0..
[1 of 1] Compiling Main             ( src/site.hs, dist/build/trofi.github.io.gen/trofi.github.io.gen-tmp/Main.dyn_o )
Linking dist/build/trofi.github.io.gen/trofi.github.io.gen ...
Removing _site...
Removing _cache...
Removing _cache/tmp...

Listening on http://127.0.0.1:8000

  updated feed/rss.xml
  updated index.html
  updated archive.html
Success
```

Now you can visit <http://127.0.0.1:8000> and explore the result.

Caveat: [`run_watcher.sh`](./run_watcher.sh) relies on a few dependencies:

- `ghc` and a few `haskell` libraries:
  * `hakyll`
  * `mtl`
  * `pandoc-types`
  * `pandoc`
  * `text`
- `graphviz`
- `gnuplot`

# Credits

The site is built with help of [`hakyll`](https://jaspervdj.be/hakyll/)
static site generator and [`pandoc`](https://pandoc.org/) document
converter.
