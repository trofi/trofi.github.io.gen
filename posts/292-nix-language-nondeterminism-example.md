---
title: "Nix Expression Language non-determinism example"
date: July 17, 2023
root: "http://trofi.github.io"
---

Today Vladimir Kryachko found a very
[curious nixpkgs bug](https://github.com/NixOS/nixpkgs/issues/244045)!

Here it's simplified version:

Good:

```
$ nix-instantiate -E 'let n = import <nixpkgs> {}; in n.pkgs.nix'
/nix/store/6gzxax0rl0k1n3hg0s22jnj6c1c0aj3b-nix-2.15.1.drv
```

Bad:

```
$ nix-instantiate -E 'let n = import <nixpkgs> {}; extensions = []; in n.pkgs.nix'
error:
       … while calling the 'derivationStrict' builtin

         at /builtin/derivation.nix:9:12: (source not available)

       … while evaluating derivation 'nix-2.15.1'
         whose name attribute is located at /nix/store/21nmswlqvkpbv9ykprrf7x6hvzi5djhf-source/pkgs/stdenv/generic/make-derivation.nix:300:7

       … while evaluating attribute 'configureFlags' of derivation 'nix-2.15.1'

         at /nix/store/21nmswlqvkpbv9ykprrf7x6hvzi5djhf-source/pkgs/stdenv/generic/make-derivation.nix:358:7:

          357|       # This parameter is sometimes a string, sometimes null, and sometimes a list, yuck
          358|       configureFlags = let inherit (lib) optional elem; in
             |       ^
          359|         configureFlags

       error: assertion '(final).hasSharedLibraries' failed
```

How can `extensions = []` affect anything? It should not!

Quick quiz: Why does crash happen? Is it a `nix` bug, `nixpkgs` bug or
neither?

## Nix Expression Language

[nix package manager](https://nixos.org/manual/nix/stable/) uses
[nix expression language](https://nixos.wiki/wiki/Overview_of_the_Nix_Language).

It's a pure (immutable values) lazy (call-by-need) dynamically typed
language. It is simple and short on builtin primitives. The simpler
expressions are usually easy to read and reason about.

`nix repl` provides great interactive environment to poke at things:

```
$ nix repl
nix-repl> let a = 1; b = 2; in a + b
3

nix-repl> (a: b: a + b) 1 2
3

nix-repl> 1 == 2
false
```

The larger expressions require some experience and care to figure out.
It has first class anonymous functions (lambdas) and attribute sets
(tables). Attribute sets are key-value pairs one can access via `.` dot
operator. Attribute sets also provide merge and other operations:

```
nix-repl> { a = 1; b = 2; } // { b = 4; c = 5; }
{ a = 1; b = 4; c = 5; }

nix-repl> { a = 1; b = 2;}.b
2

nix-repl> { a = 1; b = 2; } == { b = 2; a = 1; }
true

nix-repl> { b = 2; a = 1; }
{ a = 1; b = 2; }
```

They are called sets because attribute name order is not supposed to
matter.

## Nix Expression Language Exceptions

It's so simple! What could possibly go wrong?

Lazy languages frequently have a special property: among other things
it's hard to reason about code's performance! As performance depends not
just on the code that constructs the value, but also on the code that
inspects the value.

It might sound like a minor optimization problem. But in practice it
allows (and even encourages) to employ lazy evaluation as a flow control
mechanism: infinite lists, partially initialized data structures and
similar techniques are ubiquitous idioms.

Laziness can be observed in the following example:

```
nix-repl> let f = x: f x; in f 1
error: stack overflow (possible infinite recursion)

nix-repl> { unused = let f = x: f x; in f 1; used = 1;}.used
1

nix-repl> { unused = let f = x: f x; in f 1; used = 1;}.unused
error: stack overflow (possible infinite recursion)
```

Here we defined function `f` which calls itself indefinitely:
`f (f (f (... f (1) ) ) ... )` as long as we don't try to print or access that `unused`
field with infinite recursion things work just fine.

`nix expression language` also allows you to generate exceptions out of
pure code by constructing special values via `throw` or `assert`
keywords:

```
nix-repl> throw "gah"
error:
       … while calling the 'throw' builtin
       error: gah

nix-repl> assert true; 42
42

nix-repl> assert false; 42
error: assertion 'false' failed
```

## The actual problem

Apparently the above is enough to get non-deterministic evaluation!

The example would be a comparison of two sets with unevaluated
exceptions:

```
nix-repl> { a = 1; b = throw "meh"; } == { a = 2; b = 1; }
false

nix-repl> { a = 1; b = throw "meh"; } == { a = 1; b = 1; }
error:
       error: meh
```

Note how `b` is not causing any troubles in the first case but fails in
the second case.

And more: if we just rename `a` and `b` in the first case the
behaviour will change (needs an interpreter restart):

```
nix-repl> { b = 1; a = throw "meh"; } == { b = 2; a = 1; }
error:
       error: meh
```

It means that comparison operator somehow orders sets internally. If
they were ordered by an attribute name it would not be too bad (the
failure would be deterministic) but still annoying: alpha
conversion (renames) causing evaluation difference in pure lazy
languages is an unexpected property.

Let's drop into a one-liner evaluator. How about this one:

```
$ nix-instantiate --eval -E '{ a = 1; b = throw "meh"; } == { a = 2; b = 1; }'
false
$ nix-instantiate --eval -E 'let b = 1; in { a = 1; b = throw "meh"; } == { a = 2; b = 1; }'
error:
       error: meh
```

Aren't these supposed to be literally the same thing?

Apparently `nix` evaluator "interns" all new symbols (immutable strings,
de-duplication mechanism) in the order it encounters them. If `b`
happens to be the first it will affect all the attribute set traversals
after it!

## The workarounds time!

So how would you work the thing around in `nixpkgs` if you already
happen to define `extensions` string early? Simple! Define also
`isStatic` symbol somewhere as well:

Good:

```
$ nix-instantiate -E 'let n = import <nixpkgs> {}; in n.pkgs.nix'
/nix/store/6gzxax0rl0k1n3hg0s22jnj6c1c0aj3b-nix-2.15.1.drv
```

Bad:

```
nix-instantiate -E 'let n = import <nixpkgs> {}; extensions = []; in n.pkgs.nix'
error:
       error: assertion '(final).hasSharedLibraries' failed
```


And good again:

```
$ nix-instantiate -E 'let n = import <nixpkgs> {}; isStatic = true; extensions = []; in n.pkgs.nix'
/nix/store/6gzxax0rl0k1n3hg0s22jnj6c1c0aj3b-nix-2.15.1.drv
```

It's not a very practical workaround. But I find it funny.

To see why it works one needs to know where the `extensions` attribute
name comes from.

It comes from the following `nixpkgs` code in the guts
of `pkgsStatic.*` package definitions:

```
nix-instantiate --eval -E 'let n = import <nixpkgs> {}; in n.pkgs.stdenv.hostPlatform == n.pkgsStatic.stdenv.hostPlatform'
false
```

`hostPlatform` is a big attribute set with the main difference in
`isStatic` field:

```
nix-repl> pkgs.stdenv.hostPlatform.isStatic
false

nix-repl> pkgsStatic.stdenv.hostPlatform.isStatic
true
```

Usually that is the cutoff when we compare two attrsets. But if we add
an `extensions` into the picture:

```
nix-repl> pkgs.stdenv.hostPlatform.extensions
{ executable = ""; library = ".so"; sharedLibrary = ".so"; staticLibrary = ".a"; }

nix-repl> pkgsStatic.stdenv.hostPlatform.extensions
{ executable = ""; library = ".a"; sharedLibrary = «error: error: assertion '(final).hasSharedLibraries' failed
```

Note how `sharedLibrary` always fails to evaluate.

## How could we fix and prevent it?

I think it would be reasonable to have at least the optional mode in
`nix` evaluator to perform attrset comparisons eagerly to uncover
potential evaluation instability like that.

I proposed one in [PR nix/8711](https://github.com/NixOS/nix/pull/8711).
It manages to catch this infelicity as is:

```
$ NIX_VALIDATE_EVAL_NONDETERMINISM=1 nix-instantiate --eval -E 'let n = import <nixpkgs> {}; in n.pkgs.stdenv.hostPlatform == n.pkgsStatic.stdenv.hostPlatform'
error:
...
       error: assertion '(final).hasSharedLibraries' failed
```

On the `nixpkgs` side no comparable attrsets should contain any
exception values. It's better not to include the attribute at all than
have it throw like that.

It would be a good idea to cut down amount of abstraction layers in
`lib/systems/default.nix` so errors would be not as cryptic for
newcomers.

## Parting words

Pure lazy evaluation has it's own caveats and causes non-deterministic
evaluation. With luck some form of
[PR nix/8711](https://github.com/NixOS/nix/pull/8711) will enter `nix`
and one would be able to add CI checks against such problems.

Otherwise local patches would have to do.

Have fun!
