---
title: haskell's 'let' list expression
date: October 14, 2010
---

Начал-таки читать [`haskell-2010` report](http://www.haskell.org/definition/haskell2010.pdf)
и вдруг(!) среди прочего обнаружил, что `list expressions` умеют `let`:

~~~~ { .haskell }
{-# Prelude> #-} [ f y | x <- [1..10], let y = x + 2, let f x = x^2]
[9,16,25,36,49,64,81,100,121,144]
~~~~
