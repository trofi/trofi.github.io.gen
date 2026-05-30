---
title: "Trying out niri"
date: May 30, 2026
---

I have been using `sway` for [a while](/posts/261-trying-out-wayland.html).
`sway` is good. I can remember only one bug where `chromium` was able
to trigger assertion failures and compositor crashed on huge 32K-wide
surfaces. It was a bug in `wlroots` which was fixed quickly.

I occasionally look around for other compositors that
I could use to check out slightly different tiling ideas and fancy
features of a modern `wayland` desktop.
This time I tried [`niri`](https://niri-wm.github.io/niri/).
It looked quite close to `sway`, uses `rust` as implementation language
and `kdl` as a config language.

I turned `niri` to match my `sway` config very quickly.

## `niri` Config Tweaks

I switched `Alt` as a primary `Mod` key to
keep window dragging via `Alt+<drag>` instead of `Super+<drag>`
(`niri` calls `Mod4` a `Super`):

```kdl
input {
    // Alt to allow for Alt-drag for floating windows.
    mod-key "Alt"
    mod-key-nested "Super"
}
```

I disabled all animations to get UI to react faster:

```kdl
animations {
    off
}
```

I created named workspaces to simulate static workspaces (I did the same
in `sway`, neither compositor considers it a preferred mode):

```
workspace "1: any"
workspace "2: irc"
workspace "3: jabber"
workspace "4: any"
workspace "5: liferea"
workspace "6: web"
workspace "7: mail"
workspace "8: net"
workspace "9: music"

binds {
    Control+F1 { focus-workspace "1: any"; }
    Control+F2 { focus-workspace "2: irc"; }
    Control+F3 { focus-workspace "3: jabber"; }
    Control+F4 { focus-workspace "4: any"; }
    Control+F5 { focus-workspace "5: liferea"; }
    Control+F6 { focus-workspace "6: web"; }
    Control+F7 { focus-workspace "7: mail"; }
    Control+F8 { focus-workspace "8: net"; }
    Control+F9 { focus-workspace "9: music"; }
    Ctrl+Shift+F1 { move-column-to-workspace "1: any"; }
    Ctrl+Shift+F2 { move-column-to-workspace "2: irc"; }
    Ctrl+Shift+F3 { move-column-to-workspace "3: jabber"; }
    Ctrl+Shift+F4 { move-column-to-workspace "4: any"; }
    Ctrl+Shift+F5 { move-column-to-workspace "5: liferea"; }
    Ctrl+Shift+F6 { move-column-to-workspace "6: web"; }
    Ctrl+Shift+F7 { move-column-to-workspace "7: mail"; }
    Ctrl+Shift+F8 { move-column-to-workspace "8: net"; }
    Ctrl+Shift+F9 { move-column-to-workspace "9: music"; }
}
```

I disabled client side decorations:

```kdl
prefer-no-csd
```

The whole config hides
[here](https://codeberg.org/trofi/home/src/branch/main/.config/niri/config.kdl).

## `waybar` Tweaks

`waybar` changes looked even smaller. I mimicked `sway` plugins:

```diff
--- a/.config/waybar/config
+++ b/.config/waybar/config
@@ -3,19 +3,25 @@
     "spacing": 10, // Gaps between modules (4px)

     "modules-left": [
+        "niri/workspaces",
         "sway/workspaces",
         "sway/mode",
         "sway/scratchpad"
     ],
     "modules-center": [
+        "niri/window",
         "sway/window"
     ],
     "modules-right": [
         "idle_inhibitor",
+        "niri/language",
         "sway/language",
         "custom/upgrade-status",
     ],

+    "niri/mode": {
+        "format": "<span style=\"italic\">{}</span>"
+    },
     "sway/mode": {
         "format": "<span style=\"italic\">{}</span>"
     },

```

## Niceties

I'm using `niri` for a month as my main compositor on the desktop.

### Less Window Resize

I was pleasantly surprised to see that windows flicker even less now:
`niri` does not resize existing windows and moves them around instead. I
suspect it causes less redraws to existing apps. Such resizes was a big
stopper for me when I tried `hyprland`.

### Cursor Removal on Timeout and on Typing

`niri` has a way to hide mouse cursor after a timeout:

```
cursor {
    hide-when-typing
    hide-after-inactive-ms 1000
}
```

I use cursors of ridiculous size. I did not know I needed this feature!

### Monitor Power off Action

On `sway` I used to send signal to `swayidle` to shut off monitors on
a key binding. In `niri` there is a special `power-off-monitors` action
for that:

```kdl
binds {
    Scroll_Lock { power-off-monitors; }
}
```

### Print Screen Built-In

Similar for screenshots. There is quite the selection of various shots:

```kdl
    Print { screenshot; }
    Ctrl+Print { screenshot-screen; }
    Alt+Print { screenshot-window; }
```

## Snags and Problems

I did not encounter any major problems but found a few quirks and
missing bits.

### Running Nested `niri` Breaks Key Bindings

On a default config you can run `niri` as a regular window application.
To distinguish where hotkeys should be handled (host vs windowed) `niri`
provides `input.mod-key` / `input.mod-key-nested` configuration. But I
use too many hotkeys and effectively use both `Mod` key variants for
various keys. As a result when nested `niri` is started it seems to do
something to my host `niri` and some keys stop working. I need to debug
it in more detail. It feels like an easily fixable problem.

### No Taskbar View by Default

On `sway` I could see all the windows on the workspace by looking at the
window top. `niri` does not provide that by default. I did not find a
substitute yet. Rumors say I could write a `waybar` plugin.

## Fun Effects

`niri` uses slightly different approach to `XWayland` startup than
`sway`. I decided not to configure it at all to see what I'm using `X11`
for still. And I realized the only application that was still trying to
probe `X11` was `chromium`. I had to add `--ozone-platform=wayland` to
my browser startup script to get it to start in `X11`-less environment.

I very occasionally run `xwayland-satellite` to get one-off things to
run.

## Parting Words

`niri` is surprisingly nice! For me it was very easy to switch from
`sway` as both expose similar concepts through configuration. I like
`kdl` as a configuration language and am glad to see more and more
projects use it.

`niri` does not resize existing windows (I don't use splits that
frequently), but moves them around instead. It looks like this alone
reduces application flicker when they have to resize and reach by
updating UI.

Have fun!
