---
title: "Waypipe fixes"
date: November 07, 2022
---

`waypipe` is a great hack! It allows you to run `wayland` applications
on a remote host and show the picture on local machine (somewhat similar
to `X11` network protocol).

Architecture diagram is both
[simple and fascinating](https://mstoeckl.com/notes/gsoc/blog.html).

`waypipe` allows for quite a bit of flexibility in remote rendering
setup controlled by a few commandline options:

- `--no-gpu`: use software render instead of `DMABUF`-backed GPU. Useful
  to work around remote rendering bugs. Should not be required in
  perfect world.
- `--compress`: compress tunneled traffic for image diffs: `none`, `zstd`, `lzo`
- `--video`: stream video compression instead of pixel diffs (`vp9`, `h264`)
- `--video=hw`: use hardware video compression acceleration.

By default `waypipe` enables GPU acceleration on a remote side, extracts
the result as a texture via `DMABUF` and sends deltas (or video) over the
netowork to reconstruct it locally and pass to the compositor.

Does not sound too complicated. Or does it? Chosen common pixel buffer
format matters here: both sides need to agree on supported configuration.
And there are so many to choose from!

`wayland-info` can show supported modes:

```
$ wayland-info
interface: 'wl_shm',                                     version:  1, name:  1
        formats (fourcc):
        0x48344241 = 'AB4H'
        0x48344258 = 'XB4H'
        0x30334241 = 'AB30'
        0x30334258 = 'XB30'
        0x36314752 = 'RG16'
        0x35314152 = 'RA15'
        0x35315852 = 'RX15'
        0x32314152 = 'RA12'
        0x32315852 = 'RX12'
        0x34324742 = 'BG24'
        0x34324241 = 'AB24'
        0x34324258 = 'XB24'
                 1 = 'XR24'
                 0 = 'AR24'
interface: 'wl_drm',                                     version:  2, name:  2
interface: 'zwp_linux_dmabuf_v1',                        version:  4, name:  3
        main device: 0xE280
        tranche
                target device: 0xE280
                flags: none
                formats (fourcc) and modifiers (names):
                0x48344241 = 'AB4H'; 0x00ffffffffffffff = INVALID
                0x48344241 = 'AB4H'; 0x00ffffffffffffff = INVALID
                0x48344241 = 'AB4H'; 0x0000000000000000 = LINEAR
                0x48344258 = 'XB4H'; 0x00ffffffffffffff = INVALID
                0x48344258 = 'XB4H'; 0x00ffffffffffffff = INVALID
                0x48344258 = 'XB4H'; 0x0000000000000000 = LINEAR
                0x30335241 = 'AR30'; 0x00ffffffffffffff = INVALID
                0x30335241 = 'AR30'; 0x00ffffffffffffff = INVALID
                0x30335241 = 'AR30'; 0x0000000000000000 = LINEAR
                0x30335258 = 'XR30'; 0x00ffffffffffffff = INVALID
                0x30335258 = 'XR30'; 0x00ffffffffffffff = INVALID
                0x30335258 = 'XR30'; 0x0000000000000000 = LINEAR
                0x30334241 = 'AB30'; 0x00ffffffffffffff = INVALID
                0x30334241 = 'AB30'; 0x00ffffffffffffff = INVALID
                0x30334241 = 'AB30'; 0x0000000000000000 = LINEAR
                0x30334258 = 'XB30'; 0x00ffffffffffffff = INVALID
                0x30334258 = 'XB30'; 0x00ffffffffffffff = INVALID
                0x30334258 = 'XB30'; 0x0000000000000000 = LINEAR
                0x34325241 = 'AR24'; 0x00ffffffffffffff = INVALID
                0x34325241 = 'AR24'; 0x00ffffffffffffff = INVALID
                ...
```

Here my local machine supports a bunch of `LINEAR` formats of `DMABUF`s.
And possibly a few tiled ones (`INVALID` entries with non-zero
modifiers).

What happens when `waypipe` somehow gets format match wrong? Say, uses
tiled format on one side while unknowingly using `LINEAR` on another side?
With `waypipe-0.8.3` I got this beauty:

![broken-gears](/posts.data/265-a-waypipe-bug/gears.png)

It's hard to see on a picture but it's a `es2gears_wayland` application
output (port of `glxgears`). Supposed to look like that:

![working gears](https://upload.wikimedia.org/wikipedia/commons/6/62/Xf86_glxgears.png)

In my case colors are fine, but the gears are inclined
and shredded. I had to spend a bit of time resizing the
window to get something that resembles gears at all. Otherwise
it a line soup.

I have machines with 2 card types: `amdgpu` (`nz`) and `i965` (`i7`).
Before any `waypipe` fixes none of `amdgpu->i965`, `amdgpu->amdgpu`,
`i965->amdgpu` GPU-accelerated piping modes worked:

- `amdgpu->i965` generated garbled image ([bug report](https://gitlab.freedesktop.org/mstoeckl/waypipe/-/issues/67))
- `amdgpu->amdgpu` and `i965->amdgpu` just `SIGSEGV`ed ([bug report](https://gitlab.freedesktop.org/mstoeckl/waypipe/-/issues/69))

I was not sure if `waypipe` is maintained or abandoned given that
nothing seemingly worked. I filed the issues anyway. To my surprise
Manuel fixed both bugs before I had a chance to look into the details!

Running `waypipe` from master I got a good picture out of everything I
tried! I tested a few games and applications and got the following results:

- `waypipe --no-gpu ssh i7` is usable only for terminals and
  static-mostly browsing
- `waypipe ssh i7` gives me about 7 FPS on fast-paced games. Not quite
  usable for gaming, but good enough to get a picture back for minimal
  runtime testing.
- `waypipe --video ssh i7` gives me about 45 FPS on fast-paced games
  in 1920x1080 resolution. Not perfect for gaming, but it's almost there!
  You can certainly interact with the game without too much inconvenience.
  Lower resolution easily gives 95 FPS.

45 FPS (or even 95!) is a lot more than I expected from this setup. I
probably failed to enable hardware video codec compression. Tweaking that
should get both better picture quality and performance when in `--video=hw`
mode.

## Parting words

`waypipe ssh <host>` while slow should just work on `waypipe` from `git`.
If you still have artifacts try safest `waypipe --no-gpu ssh <host>`.
Best interactive I got was from `waypipe --video=hw ssh <host>`.
Upstream is alive and responsive.

And finally [a screnshot](/posts.data/265-a-waypipe-bug/xonotic.png)
extracted from `waypipe --video=hw ssh i7 nix run nixpkgs#xonotic`.

You may notice quite a bit of video codec damage on the screen. But
that's just because `i7` is such an old slow box. I think it's a good
result overall.

Have fun!
