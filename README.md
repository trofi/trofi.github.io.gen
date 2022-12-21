Hello!

This is the source of <https://trofi.github.io/> blog.

# Navigating

- [posts/](./posts): posts source code in `.rst` or `.markdown` format.
- [posts.data/](./posts.data): post-specific images, example binaries and similar.
- [src/](./src): source code for the site generator itself.
- [run_watcher.sh](./run_watcher.sh): main script to generate site and run a preview web server locally.
- [trofi.github.io](https://github.com/trofi/trofi.github.io/): repository in
  [GtHub pages](https://pages.github.com/) format with auto-generated content.

# Trying it out

Just clone this repo and run a [run_watcher.sh](./run_watcher.sh) and see what happens:

```
$ git clone https://github.com/trofi/trofi.github.io.gen
$ cd trofi.github.io.gen/
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

Caveat: [run_watcher.sh](./run_watcher.sh) relies on
[nix](https://nixos.org/manual/nix/stable/) package manager and would
not run as is in systems without it. It's not a required dependency:
To run it on `nix`-less system you will need to install `ghc`,
`pandoc` and `hakyll`. After you tweak the script header you should
be able to run it.

# Credits

The site is built with help of [hakyll](https://jaspervdj.be/hakyll/) static
site generator and [pandoc](https://pandoc.org/) document converter.
