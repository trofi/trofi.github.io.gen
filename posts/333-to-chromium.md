---
title: "To chromium"
date: April 20, 2025
---

## Tl;DR

I switched from `firefox` to `chromium` as a primary web browser on my
desktop.

## On `firefox`

I was a happy `firefox` since it's `1.5` release. Internet says it was
released in 2005. This made a 20-year run for me. The web changed so
much since then. Adobe Flash went away and web 2.0 `javascript`-heavy
applications took it's place. At some point I had to start using content
filtering extensions to be able to browse the web.

`firefox` was able to keep up with the times most of the time. It felt
like at some point UI became too sluggish. Subjectively `quantum` 2017
release made it snappy again.

Fast forward to 2025 `firefox` mostly meets my needs, but there are a
few performance warts I don't know how to deal with:

- Some of web-based instance messengers are very slow in `firefox`: when
  I switch to the tab it freezes the whole of `firefox` for a few
  seconds (it happens every time I switch to a tab, not just for the
  first time).
- Branch selection (drop-down menu) at pull request creation time on
  `github` is visibly slow on repositories with many branches (`200+`
  in `nixpkgs`).
- `firefox` startup time on mostly empty user profiles on HDDs are very
  slow: about tens of seconds.

Over past few years I have encountered a few widespread bugs in `firefox`:

- `100%` CPU usage on `HTTP3`: <https://bugzilla.mozilla.org/show_bug.cgi?id=1749914>
- `tab crashes due to LLVM bug`: <https://bugzilla.mozilla.org/show_bug.cgi?id=1741454>

## On `chrome`

I already used `chrome` at work for about 10 years. And 3 years ago I
started using `chrome` on a `chromebook` laptop for some of personal
things. But for a personal desktop my strong preference prefer is not to
use proprietary software.

With a recent shift to AI and advertising at Mozilla I wondered what are
the alternatives to `firefox` there are if I should give `chromium` a
proper try.

## On `chromium`

After about 2 months using `chromium` I should say that it is very
pleasant to use. Subjectively fonts look a bit better in `chromium` and
most performance hiccups I encountered in `firefox` disappeared (but a
new one appeared, mentioned below).

Helper pages like `chrome://about`, `chrome://flags` and `chrome://gpu`
are a reasonable substitute for `firefox`'s `about:config`.

I also discovered a few bugs/warts/known-issues as well:

- `wayland` backend is not enabled by default and needs either a flag
  like `--enable-features=UseOzonePlatform --ozone-platform=wayland` or
  an option selected at
  `chrome://flags > Preferred Ozone platform > Wayland`.

  While it's a one-off setup it feels like `wayland` might not be the
  primary target for `linux` desktops.

- `pdf` viewer is quite a bit slower than in `firefox`. 350-paged doc
  make `chromium` visibly struggle to scroll around, might be a known
  <https://issues.chromium.org/issues/345117890>.

  I have to fall back to local viewers for larger docs.

- `chromium` syncs on disk somewhat frequently. There is a 15-years old
  <https://issues.chromium.org/issues/41198599> that mentions it's all
  the actions user does are synced on dusk time to time. I don't think
  it's a real problem for modern SSDs, but still it feel quite wasteful.

- `sway` sometimes crashes completely when I visit certain utility
  provider sites with a message like:

  ```
  00:00:25.696 [sway/sway_text_node.c:110] cairo_image_surface_create failed: invalid value (typically too big) for the size of the input (surface, pattern, etc.)
  00:00:25.696 [sway/sway_text_node.c:110] cairo_image_surface_create failed: invalid value (typically too big) for the size of the input (surface, pattern, etc.)
  sway: render/pass.c:23: wlr_render_pass_add_texture: Assertion `box->x >= 0 && box->y >= 0 && box->x + box->width <= options->texture->width && box->y + box->height <= options->texture->height' failed.
  ```

  There is a bunch of open bugs with related error messages. I looks
  like those are usually `sway` or `wlroots` robustness bugs. `chromium`
  is probably also at fault here trying to create surfaces of
  unreasonable dimensions.

  Installing `sway` and `wlroots` from `git` `master` fixed all crashes
  for me.

Have fun!
