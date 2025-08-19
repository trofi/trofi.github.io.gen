---
title: "Trying out helix editor"
date: February 15, 2025
---

This is another February story about text editors similar to the
[`vim` one](/posts/277-from-mcedit-to-vim.html). You might want to
ignore this one as well :)

## Tl;DR

`helix` is a nice program: I switched to it from `vim` as a default text
editor. If you never heard of `helix` editor and are `vim` or `nvim`
user I suggest you to look at it. `hx --tutor` is short and yet
it covers a few cool things. <https://helix-editor.com/> has a nice
`asciinema` intro and shows expected look and feel.

## Background

I am a happy 2 years old `vim` user with a simple
[`~/.vimrc` config](https://github.com/trofi/home/blob/master/.vimrc).
Strong `vim` features for me are:

- startup speed
- UI speed
- basic spell checking
- syntax highlighting for many languages
- tab/space whitespace highlighting
- configurable color scheme (ideally a blue one)
- `emacs`-style page scrolling and line editing when in insert mode

I still manage to use `vim` without any external plugins.

The weak `vim` points for me are:

- `vim`-specific configuration language (I don't know how to read anything
  beyond trivial `set` assignments)
- `vim`-specific regex language extensions (`:h /magic`)
- lack of language server protocol support (`LSP`) support without
  external plugins
- defaults keep compatibility with old versions of `vim` which sometimes
  don't make sense to me as a new user
- it's written in an ancient form of `C` which is known to trigger some
  ubiquitous safety checks and have to disable them like
  [`-D_FORTIFY_SOURCE=1` hack](https://github.com/vim/vim/issues/5581)
- "backwards" model of many actions in normal mode

To expand a bit on "backwards" model here is a simple example: in `vim`
the key sequence `df<` will delete (`d`) everything from current
position to `<` symbol inclusive. But you will not see what exactly
`vim` is about to delete until you press `<`. Would be nice to see what
`f<` selects first and only then press `d` with more confidence. As a
result I rarely use such shortcuts despite them being very convenient
for a common editing use case. `vim`'s very own `vf<d` command sequence
is a lot more intuitive to what I would expect, but that requires switch
to a visual mode (prefixed `v`).

## A fun problem

From time to time I use `vim` to write `markdown` files (such as this
blog post). I like pasting code snippets here and there for better
illustration. Once day I idly wondered if `vim` could be taught to
support syntax highlighting of the code snippets within the `markdown`
files:

````markdown
# Would it not be magic if it just worked?

An example `c` snippet within `markdown`:

```c
// What's up here with the highlight?
int like_this_one(long long);
```
````

`vim` does not do any special highlighting in a `c` block.

Many days later I encountered a mastodon thread that mentioned `vim`
proposal to use `TextMate` grammar
[`Issue#9087`](https://github.com/vim/vim/issues/9087). It discussed
various options of different highlighter engines, their pros, cons, and
what `vim` should use longer term.
[`Tree-sitter`](https://tree-sitter.github.io/tree-sitter/) was
specifically mentioned multiple times there as The Solution to all
the highlighting problems an editor could have. It's a long discussion
with many branches.
From there I learned about (and started using) a few `tree-sitter` based
programs, like [`bat`](https://github.com/sharkdp/bat),
[`difftastic`](https://difftastic.wilfred.me.uk/) and
[`helix`](https://helix-editor.com/).

`helix` editor was mentioned as one of `tree-sitter` users. I heard
about `helix` before from my friend. At the time I just started my `vim`
journey and I did not give `helix` a serious try.
But this time I felt I was able to compare both.

## My setup

I got `helix` up and running to the state I could use it by default in
2 short evenings. After that I did a few incremental tweaks. My whole
configuration right now is one page long:

```toml
# $ cat config.toml
[editor]
auto-pairs = false
bufferline = "always"
rulers = [73]
true-color = true # TMUX term does not always agree

[editor.statusline]
# added "file-type", "position-percentage"
right = ["file-type", "diagnostics", "selections", "register", "position", "position-percentage", "file-encoding"]

[editor.whitespace.render]
space = "all"
tab = "all"
nbsp = "all"
nnbsp = "all"

[editor.whitespace.characters]
tab = ">"
tabpad = "-"

[keys.insert]
C-c = "normal_mode"
C-up = ["scroll_up", "move_visual_line_up"]
C-down = ["scroll_down", "move_visual_line_down"]

[keys.normal]
C-c = "normal_mode"
C-up = ["scroll_up", "move_visual_line_up"]
C-down = ["scroll_down", "move_visual_line_down"]
ins = ["insert_mode"]

[keys.select]
C-c = "normal_mode"
C-up = ["scroll_up", "move_visual_line_up"]
C-down = ["scroll_down", "move_visual_line_down"]
```

On top of that I enabled a few language servers not configured in
`helix` by default:

```toml
# $ cat languages.toml

# spell checker
[language-server.harper-ls]
command = "harper-ls"
args = ["--stdio"]

[[language]]
name = "markdown"
language-servers = ["marksman", "harper-ls"]
auto-format = false

[language-server.rust-analyzer.config]
check.command = "clippy"
```

## Niceties

I was surprised to discover how much `helix` already provides without
much extra configuration:

- 24-bit colors in the terminal emulator (I use `alacritty` most of the
  time and I appreciate finer grained colors)
- a ton of pre-configure `LSP` servers: `hx --health` reports 273 lines
- helpful pop-ups when prefix keys are pressed, like `<space>`, `:`, or
  `m`
- `tree-sitter`-based syntax highlighting makes highlighting more
  consistent across languages

### `toml` configuration language

I grew to like `toml` compared to other custom `.ini`-like formats.
Custom configurations are sometimes not general enough. For example in
`nix.conf` config there is no way (to my knowledge) to add a trailing
whitespace to:

```ini
bash-prompt-suffix = dev>
```

`toml` on the other hand, is more predictable in this regard. It is quite
common and expressive enough to encode simple arrays and strings with
any contents.

I'm a bit afraid of the configurations that are programming languages.
It's not too bad for me when they are general-purpose languages with
good error messages, well understood semantics, and present
introspection for available options and helpers.

### Color themes can be set in `RGB`

Using full `RGB` range to define color elements is great. `helix` comes
with a nice dark default theme suitable for long editing sessions.

The only caveat of a default theme is that some colors are not unique
for cases where it matters. For example, to work around a bug in default
theme I needed to pick a different color for secondary selection:

```toml
# $ cat themes/sf.toml
inherits = "default"

# workaround https://github.com/helix-editor/helix/issues/12601
"ui.selection" = { bg = "#540020" }
"ui.selection.primary" = { bg = "#540099" }
```

Using `RGB` is so much better than picking a pre-defined color. I did
not know I need it until I tried :)

### Selection and multi-selection feels intuitive

`helix` does show what navigation commands select before I about to do
an action. An example would be `vim` `df"` sequence compared to
`helix` `f"d` sequence. Before pressing `d` I am more confident what
it is about to delete.

After using `helix` for a while I am actually more comfortable using
`vim` `f` / `t` (and similar) navigation commands because I understand
better what they actually do.

Multi-selection is also very natural: you create a bunch of cursors
based on you search (`s` command) in your selection (or by extending a
column with `C` command) and start modifying text interactively at each
active cursor at the same time. In multi-selection mode it's more
natural to use navigation commands like `f`, `t` and `w` to do bulk
edits. In `vim` I used to use arrow keys more and did not see much use
for more complex navigation commands. But now once I'm used to then I'm
using them in `vim` as well.

### IDE experience is unexpectedly good

The `LSPs` provide you a navigation, hints, symbol search and so much
more. It's so easy to explore new and existing projects for various
cross-references. Before jumping into the target you can look at the bit
of context in preview and it might be enough for a thing you are
looking for!

Even in this post `<space>s` (symbol lookup) provides a Table Of
Contents output with a preview.

For development projects `<space>f` provides you a file picker with a
fuzzy search. Now I have to rely a lot less on mashing `<TAB>` in the
shell to get to a file I want to edit. I even installed `fzf` to emulate
similar fuzzy search experience when I need it in `bash` to pass a file
to other programs.

To make `clangd` to work (a `C` or `C++` LSP) one needs
a `compile_commands.json` file. `meson`-based projects just create it
unconditionally, `cmake`-based projects do it after
`-DCMAKE_EXPORT_COMPILE_COMMANDS=YES` option is enabled and for
`autotools`-based projects there is a
[`bear`](https://github.com/rizsotto/Bear) hack to wrap `make` and
extract the commands after the build.

`bear` is not able to handle projects like `gcc` where local `gcc` is
used to compile most of the code. But smaller ones work good enough.

<https://github.com/helix-editor/helix/wiki/Language-Server-Configurations>
has tips for many more language servers.

## Snags

Over past month of `helix` use as a default editor I encountered a few
limitations I had to work around or just accept the limitation.

### Trailing whitespace highlighting (minor)

Whitespace highlighting is a bit blunt: I would prefer spaces to be
highlighted only in trailing context while highlighting tabs everywhere.
<https://github.com/helix-editor/helix/issues/2719>.

But it's not a big deal. I need to be careful to copy text into clipboard
buffer not with a mouse selection but via `<space>y`

### Spell checking (minor)

I was surprised to see that `helix` does not yet have spell checking
integration and was afraid I could not use it as I make huge amount of
typos and rely on `aspell` so much. The integration is tracked by
<https://github.com/helix-editor/helix/issues/11660>.

But luckily there are a few language servers that do implement spell
checking. I'm using `harper` as:

```toml
[language-server.harper-ls]
command = "harper-ls"
args = ["--stdio"]
```

It works reasonably well for English but does not support anything else.
Having an `LSP` based on something like `aspell` would be nice.

### Default theme colors (minor)

`helix` tutorial has section that demonstrates primary and secondary
selections via `(` and `)` navigation. But the colors of both selection
types are identical in the default theme. That was very confusing. The
issue is 2.5 years old:
<https://github.com/helix-editor/helix/issues/12601>.

Luckily the workaround is trivial: you can modify default theme just
for selection distinct colors:

```toml
inherits = "default"

"ui.selection" = { bg = "#540020" }
"ui.selection.primary" = { bg = "#540099" }
```

### Saving last cursor position in the file

I like the save/restore of the file cursor in edited file. It was a
default `vim` setting. `helix` does not have an equivalent yet:
<https://github.com/helix-editor/helix/issues/1133>

### Buffer search only highlights current search, not all

In `vim` I was using `set hlsearch` option to highlight (and keep
highlighted) all the occurrences that match the search. Not just the
current one. `helix` is yet to implement it:
<https://github.com/helix-editor/helix/issues/1733>.

### Generic autocomplete

I like autocompletion of arbitrary words in a text file.
So far `helix` only ever does `LSP` autocompletion.

<https://github.com/helix-editor/helix/issues/1063> tracks the addition
of a simple keyword-based completer.

## Parting words

`helix` feels like a good modern `vim` successor for my use cases. It's
extensive use of `RGB` colors and unicode characters gives a look and
feel of a program beyond a terminal application. A ton of pre-configured
`LSPs` makes it a nice lightweight code navigator on par with IDE
experience.

Have fun!
