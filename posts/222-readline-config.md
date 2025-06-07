---
title: "readline config"
date: December 12, 2020
---

`bash` and a few other tools use `readline` library to implement
interactive command line interface with history support, tab completion
and text editing capabilities like copy/paste buffers and even macro
commands.

Here is my tiny `~/.inputrc` to enable a bit more completion color:

``` 
# cat ~/.inputrc
set colored-completion-prefix on
set page-completions off
set colored-stats on
set show-all-if-ambiguous on
set blink-matching-paren on
```

These all are minor visual and minor tab repeat changes.
