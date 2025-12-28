---
title: "zellij terminal emulator"
date: December 28, 2025
---

## my `tmux` setup

I use `tmux` by default for all my terminal sessions since around 2016
(almost 10 years!): on most days I switch from local desktop keyboard to
a laptop and back to run/debug stuff on my desktop. I usually have 4
sessions: various builders, development session, chat session and
various one-off investigations.

I have a moderate [`~/.tmux.conf`](https://github.com/trofi/home/blob/master/.tmux.conf):

```
set -ga terminal-overrides ',xterm*:smcup@:rmcup@'

set -sg escape-time 0

set -g mouse on
bind -T root WheelUpPane   if-shell -F -t = "#{alternate_on}" "send-keys -M" "select-pane -t =; copy-mode -e; send-keys -M"
bind -T root WheelDownPane if-shell -F -t = "#{alternate_on}" "send-keys -M" "select-pane -t =; send-keys -M"

set -g @scroll-speed-num-lines-per-scroll 3

# override default
set -g status-right-length 60 # was 40
set -g status-right '#h, %Y-%m-%d %H:%M' # was something like '"#h", #S'

# Start windows and panes at 1, not 0
set -g base-index 1
setw -g pane-base-index 1

# defatul is ~2000
set-option -g history-limit 10000

# Allow title update from within tmux apps
set-option -g set-titles on

# Host-specific overrides
if-shell "[ -e ~/.tmux.conf.local ]" "source-file ~/.tmux.conf.local"

# Extend default variable list of:
#   "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY"
set-option -g update-environment "DISPLAY SSH_ASKPASS SSH_AUTH_SOCK SSH_AGENT_PID SSH_CONNECTION WINDOWID XAUTHORITY   WAYLAND_DISPLAY SWAYSOCK I3SOCK"

# Join current window into a previous one. Should act as inverse of Ctrl-b !
bind-key j "join-pane -s !"
```

While a bit wordy it's not a big or complicated setup. And it already
contains a few hacks like `set -sg escape-time 0` to make `ESC` button
act more responsive. Without it switching modes in `vim`-like editors
has a perceptible delay. I had to press `ESC` twice to get something out
of `vim` instantly.

## a `tmux` hickup

A month ago I casually `ssh`ed on my desktop and was not able to access
any of my running `tmux` sessions:

```
$ tmux a
open terminal failed: not a terminal
$ tmux
open terminal failed: not a terminal
```

This happened because `tmux` updated from `3.5a` version to `3.6` and
changed its protocol implementation between server (still ran `3.5a`)
and the client (updated to `3.6`). The workaround was trivial: run `3.5`
client for a while until the machine get scheduled for a reboot.

I debugged it a bit and find out it was an intentional change. I filed
[`tmux issue #4711`](https://github.com/tmux/tmux/issues/4711) to improve
the error message on `tmux` side.
From what I understand at least on `linux` the change that caused the
protocol change was entirely in `tmux` source code base (around
`compat/imsg.h` / `compat/imsg.c` files imported from `OpenBSD`).
`tmux` author decided not to improve the error message.

## `zellij`

This event prompted me to wonder what are the other terminal
multiplexers out there.

I tried `zellij` and am using it for the past month. It's not a drop-in
replacement for `tmux` but it gets very close for my use cases. I
collected a few niceties and a few snags below I encountered while using
it.

### nice: `zellij` retains many `Ctrl-b` `tmux`-style keys as is

Default `zellij` configuration is usable as is for a `tmux` user:
`Ctrl-b c` opens a new pane (as expected), `Ctrl-b ,` renames a tab and
so on. That makes it very easy to try `zellij` without any exploration
of config file format.

### snag: some keybindings interfere with other applications

`zellij` uses a few escape-style initial sequences, not just
`tmux`-style `Ctrl-b`, but also `Ctrl-p` (panes), `Ctrl-n` (resizes),
`Ctrl-h` (moves), `Ctrl-o` (session operations) and a bunch of `Alt-`
ones.

Sometimes these escapes interfere with rich applications like `mc`,
`vifm`, `vim` or `helix` editors.

On the bright side many of them are very convenient, like `Alt-n` /
`Alt-f` to get a short-lived pane.

Status bar always makes it clear that you got into one of `zellij`
modes. But I had to disable quite a few `Ctrl-` and `Alt-`-based key
bindings in favor of deeper nested `Ctrl-b` ones in `tmux` style.

### nice: `Ctrl-g` to disable all the `zellij` bindings

Given the above `zellij` has a nice `Ctrl-g` kill-switch to turn all
bindings off (except `Ctrl-g` itself).

### nice: scrollback buffer editing

When in the scrollback scrolling mode (`Ctrl-b [` in` tmux`) I sometimes
want to save part of the log (or all the contents) into a file. In `zellij`
it's right there at `e` key: it opens default editor with full contents.

### nice: fast scrolling on copy/paste from the scrollback

Very occasionally I want to copy 100-200 lines of a scrollback and paste
it into the browser. I usually use the mouse and in `tmux` it was very
slow for me. I did not always succeed on a `ChromeOS` terminal.

### nice: modern features

I noticed that many features like link uderscores and tooltip pop-ups
work in` zellij` without any configuration just like they work in a host
terminal. `tmux` does not advertise some of them.

### snag: `CPU` / `RAM` usage is high

`zellij` can use quite a bit of `CPU` (and `RAM`) if the program pipes
out a lot of text. For example `cat -v /dev/zero` command will use
`135%` `CPU` on my system with quite a bit of `RAM` usage (I `Ctrl-C`ed
at `15GiB`). 

It's not as bad on more typical multiline workloads.

There is an existing [report](https://github.com/zellij-org/zellij/issues/3594)
to get it slightly better.

### snag: a banner in the status line

`zellij` keeps its verbatim name in status bar as an advertisement bit.
Which takes away 10 bytes from the status bar
([the report](https://github.com/zellij-org/zellij/issues/4504)).

In theory it's a one-liner change. But patching it out is not very
convenient as `zellij` implements plugins as `wasm` binaries and ships
the status bar as precompiled `.wasm` file. I did not manage to rebuild
it locally yet.

### snag: no `tmux`-style mouse drag support

I liked how `tmux` allows you to resize panes just by dragging them.
`zellij` did not implement it yet ([the report](https://github.com/zellij-org/zellij/issues/1262)).

`Ctrl-n` and arrow keys would have to do for now.

### snag: no environment variable clobber support

`tmux` has a nice variable clobbering feature when `DISPLAY`,
`SSH_AUTH_SOCK` and a few other variables are updated with variable
values of the most recent attached client. As a result `ssh-agent`,
`X11` sessions and other things Just Work in newly opened panes.
`zellij` does not have the feature yet
([the report](https://github.com/zellij-org/zellij/issues/1637)'

### snag: no support for editing keys in tab editor

In `tmux` when renaming a tab you can use things like `Ctrl-w` to delete
existing word. `zellij` dumps a `[119;5u` escape.

## parting words

When I started using `zellij` I got a lot more than I expected:

- friendly UI that tells you what modes are there and which one is active
- nice and short configuration file format
- modern terminal features support (URL underscores, tooltips)
- mostly compatible with `tmux` key bindings
- intuitive text selection in the scrollback

But it has quite a few snags as well:

- banner in the status line
- no environment variable clobber support
- default config needs some tweaking to be more usable:

  * disable hello pop-up (`show_startup_tips false`)
  * disable key bindings that clash with `helix` editor
  * disable pane frames by default (`pane_frames false`)
  * disable session serialization (`session_serialization false`)

So far `zellij` is a nice alternative to `tmux` for me.

Have fun!
