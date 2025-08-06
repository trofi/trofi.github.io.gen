---
title: "Trying out wayland"
date: September 28, 2022
---

## TL;DR

I switched to `wayland` from `X11` as my daily driver and don't
regret it so far. It feels like it fixed more problems for me than
introduced to the point I'd prefer not to switch back.

## `X11` and default `DPI`

I got my desktop monitor a few years ago. It is capable of
`3840x2160` output (`172` DPI). My integrated video card could do only
`1920x1080` at most (`78` DPI). I was a happy user of `Xorg` with
default `96` DPI settings. I was using `alacritty` as a terminal with
bitmap `Terminus` fonts carefully picked to avoid the need for glyph
hinting.

## `X11` and `HiDPI`

About a year ago I updated my desktop machine. I chose cheap video card
that could output in `3840x2160` mode for my existing monitor. I wanted
to look at the use of vector fonts in the terminal.

I switched to `HiDPI` mode on `X11`. Without the special configuration
everything shrunk and became half the initial size! Arch Linux has a
detailed overview of the knobs you can tweak to get most of `Xorg`
environment upscaled: <https://wiki.archlinux.org/title/HiDPI>.

I tried hard not to configure too many things manually. I did font
size `Xft.dpi: 192` tweak via `.Xresources`. Pictograms in many
applications (like `claws-mail`) didn't pick up larger sizes. Mouse
cursor became almost invisible. `gtk-2` applications and older `X11`
applications that specify window sizes in pixels became unusable. I
fixed cursor `Xcursor.size: 64` via `.Xresources`. In applications
(toolkits) without good support for `DPI` scaling I had to artificially
increase font size and disabled pictograms in favor of text labels on
buttons.

Surprisingly I had to abandon a few GUI apps in favor of their terminal
equivalents as font scaling works so much better than UI element
scaling!

The result was looking ok-ish. But after an active use I noticed sever
tearing effect when I switch between workspaces. It was most pronounced
when I switched from `Firefox` workspace (usually white background) to
terminal workspace (black background). Tearing effect was a diagonal
blocky zip line seen for a very short while. I mitigated it with
`Option "TearFree" "true"` via `xorg.conf`. The mitigation only mostly
removes tearing effect, but not fully. Diagonal line gets turned to
horizontal line. The tearing frequency decreases to usable levels. But
it was still noticeable.

Looking at the final `X11` `HiDPI` setup:

- the `TTF` fonts now looked great in terminals
- the UI sizes were not quite the same, usually smaller, sometimes
  unreadable
- some programs did not survive `2x` downscaling and were too small to be
  useful at all
- a bit of tearing effect was present

## `wayland`

I've been meaning to try `wayland` for a while but never had an excuse.
I was afraid that it will break too many applications I use frequently (or
occasionally). I vaguely remembered horror stories from past years of
things like `wine` not being able to track mouse cursor properly,
possible video card incompatibility problems (like `nvidia`). I also
was afraid I was a too long-term `Xorg` user to easily accept minor
changes I'll face.

Having looked at the amount of tweaks I had to do for `HiDPI` and seeing
recent [An `X11` Apologist Tries Wayland](https://artemis.sh/2022/09/18/wayland-from-an-x-apologist.html)
post I decided to give it a try.

I used `i3` window manager in `Xorg`. Closest `wayland` sibling is
`sway`. `sway` is mostly config-compatible with `i3`: it reads
`~/.config/i3/config` if `~/.config/sway/config` does not exist.
Thus, the initial switch is trivial: just run `sway`.

`wayland` has a bit different way to handle `DPI`: it just assumes `96` DPI
for programs that don't handle scaling. User needs to specify the scale
once for `wayland`. For me the magic command was `wlr-randr --output DP-3 --scale 2`
(or `output DP-3 scale 2.0` via `~/.config/sway/config`).

After that I got most old programs up to a reasonable size! It is
something that `X11` `HiDPI` could not do easily. That was a pleasant
surprise. I even undid those rare changes I had to do for `HiDPI` on
`X11`! `Xwayland` does a reasonable job of running many `X11` programs
as is. There are warts still.

Surprisingly under `wayland` tearing effect disappeared completely.

I switched to direct `sway` start from `linux` terminal. Before I used
`lightdm` graphical login manager (modern `X11` needed it for rootless
mode for various reasons).

Not everything was ideal in `wayland` world. Some unexpected things I
found:

- `xeyes` background is garbled at start, issue known as
  <https://github.com/swaywm/sway/issues/3395>
- clicking the links in terminal does not switch the focus to the
  browser, issues known as <https://github.com/swaywm/sway/issues/4830>
- `Ctrl+Enter` does not work in `mc` (because it relies on `X11` events).
  I'm slowly reading through <https://wayland-book.com/> to get the idea
  where it should ideally be implemented: in the terminal, in `mc` or
  somewhere else.

## Parting words

When I read through <https://wayland-book.com/> I got a nice feeling of
being able to write nice smooth demos. Just like in `DOS` times when you
could write pixels right into video RAM at the right `VSYNC` time (`0x3da`
port).
It's probably no harder in `X11`. I never got to look at `X11` `MIT-SHM`
extension fearing of its lack of network transparency support. I spent
some time playing with
<https://wayland-book.com/xdg-shell-basics/example-code.html> example.

Architecturally `wayland` feels a lot like `X11`: both are RPC protocols
over `UNIX` socket to interact with objects on the server asynchronously
via requests and events. But the difference in the details is also
startling: frame perfect design gives a great user experience, many
things are a bit different that they used to be and require different
solution.

I like `sway` / `wayland` experience so far and will try to port a few small
things to it from `X11/OpenGL`.

Full `sway` config I got: <https://github.com/trofi/home/blob/master/.config/sway/config>.

Have fun!
