---
title: "AoC in rust"
date: December 25, 2021
---

:PostID: 232
:Title: "AoC in rust"
:Keywords: AoC, rust, REPL
:Categories: notes

Tl;DR
-----

For me **rust** was pleasant to write small code snippets like AoC
solutions. Rust has a decent REPL in form of **evcxr**.

Story mode
----------

Following Bill's suggestion I gave `Advent of Code <https://adventofcode.com/>`_
a try this year.

I did not know how much time it would take me. I hoped it would not be
more than 30 minutes per morning for each of the problems. It was true
for most problems, but some took a lot more than that.

haskell
=======

Problems 1 to 7 I solved in haskell as I'm most familiar with it compared
to other languages when it comes to one liners and throw-away scripts.
Here is an example of solution for
`Problem 5 part 1 <https://adventofcode.com/2021/day/5>`_ to get the feeling
of how I would write things:

.. code-block:: haskell

    {-# OPTIONS_GHC -Wall #-}
    import qualified Control.Monad as CM
    import qualified Data.Char as DC
    import qualified Data.List as DL
    
    type I = Integer
    
    data L = H I I I
           | V I I I
           | Unsupported
    
    solve :: [L] -> Integer
    solve = DL.genericLength
          . filter (\g -> length g > 1)
          . DL.group
          . DL.sort
          . concatMap expand
        where expand (H y x1 x2) = [ (x,y) | x <- [x1..x2]]
              expand (V x y1 y2) = [ (x,y) | y <- [y1..y2]]
              expand Unsupported = []
    
    readInput :: String -> [L]
    readInput inp = parseL <$> lines inp
        where parseL s = norm ((x1, y1), (x2, y2))
                  where [x1,y1,x2,y2] = read <$> words (map (\c -> if DC.isDigit c then c else ' ') s)
              norm ((x1, y1), (x2, y2)) = case () of
                _ | x1 == x2 -> V x1 (min y1 y2) (max y1 y2)
                _ | y1 == y2 -> H y1 (min x1 x2) (max x1 x2)
                -- _            -> error $ "Unexpected line: " ++ show l
                _            -> Unsupported
    
    main :: IO ()
    main = CM.forM_ ["example", "input"] $ \fn -> do
        f <- readFile fn
        let inp = readInput f
            ans = solve inp
        print $ (fn, ans)

Error handling is virtually non-existent. Efficiency (list sorting
instead of using something like **Data.Set**) is probably crippled
in favour of smaller code length.

rust
====

The 1-7 problems looked too simple and I gave **rust** a try starting
from problem 8.

My **rust** background is tiny: I read a few rust books before but never
tried writing anything more than a hello world. AoC set of challenges
looked like a good opportunity to get used to syntax and some basics of
it's standard library.

Here is my solution for `Problem 8 part 1 <https://adventofcode.com/2021/day/8>`_
and my first "non-trivial" rust program:

.. code-block:: rust

    use std::{*, fs::*, io::*};
    
    fn get_input(input_file: &str) -> Vec<Vec<String>> {
        let r = BufReader::new(File::open(input_file).unwrap());
    
        return r.lines().map(|l|
            l.unwrap()
             .split(' ')
             .map(|s| String::from(s))
             .skip_while(|e| e != "|").skip(1)
             .collect()
        ).collect();
    }
    
    fn main() {
        for input_file in ["example", "input"] {
            let input = get_input(input_file);
    
            let ans = input.into_iter()
                           .flatten()
                           .filter(|e|
                               [2usize,3,4,7].contains(&e.len())
                           )
                           .count();
            println!("{}: {}", input_file, ans);
        }
    }

Rust certainly has a feel of haskell at least when it comes to chaining
operations on sequences.  It's non-idiomatic in absent error handling,
inefficient in terms of excessive lifetimes for temporaries. But look
quite readable.

rust resources
==============

To get more familiarity with the language I reread https://doc.rust-lang.org/book/
and used https://doc.rust-lang.org/std/index.html extensively to look up
useful helper in a standard library.

rust REPL
=========

As a playground I was initially using https://godbolt.org/ and **rustc**
in the command line. But it was a bit tedious when it comes down to
exploration of simple operations. I needed a **ghci** equivalent
(a haskell REPL environment).

Looks like **evcxr** (https://github.com/google/evcxr/blob/main/evcxr_repl/README.md)
is a popular **rust** REPL out there. Luckily **nixpkgs** providesit in
the standard repository:

.. code-block::

    $ nix run nixpkgs#evcxr
    Welcome to evcxr. For help, type :help
    >> 'a'.to_up<TAB>
    >> 'a'.to_uppercase()
    ToUppercase(One('A'))
    >> String::from_iter('a'.to_uppercase())
           ^^^^^^^^^ function or associated item not found in `String`
    no function or associated item named `from_iter` found for struct `String` in the current scope
    help: items from traits can only be used if the trait is in scope
    help: the following trait is implemented but not in scope; perhaps add a `use` for it:
    
    use std::iter::FromIterator;
    help: there is an associated function with a similar name
    
    from_utf8
    >> use std::iter::FromIterator;
    >> String::from_iter('a'.to_uppercase())
    "A"

It supports type inspection for bound variables:

.. code-block::

    >> let v = [1,2,3];
    >> :vars
    v: [i32; 3]

And even supports use of external crates:

.. code-block::

    >> :dep "quickcheck"
    >> fn is_ok(b: bool)->bool { b }
    >> quickcheck::quickcheck(is_ok as fn(bool)->bool)
    thread '<unnamed>' panicked at '[quickcheck] TEST FAILED. Arguments: (false)', .../quickcheck-1.0.3/src/tester.rs:165:28

I spent a few hours poking at various containers, iterators and
available methods.

Prepared with these basics I managed to solve the rest of AoC challenges.

Nice AoC challenges
===================

Most AoC problems were straightforward and did not really pose a real challenge.

But some of them were not so simple. Here is my list of the nicest ones:

- `Problem 14 <https://adventofcode.com/2021/day/14>`_: "Extended Polymerization"
- `Problem 19 <https://adventofcode.com/2021/day/19>`_: "Beacon Scanner"
- `Problem 23 <https://adventofcode.com/2021/day/23>`_: "Amphipod"
- `Problem 24 <https://adventofcode.com/2021/day/24>`_: "Arithmetic Logic Unit"

"Extended Polymerization" is a susprisingly concise problem that managed to trick me
more than once. First, I did not notice exponential growth. Then it took me a while
to reformulate correctly it in a form suitable for dynamic programming solution.
And then I missed a few subtleties.

"Beacon Scanner" looked like a tedious problem, but it has many fun angles to explore.
For some reason on my input not all expected pairs in the solution had 12+ points
overlap. That wedged me for a day of debugging exploration of data.

"Amphipod" reminded me of a Norvig's online AI class I took almost 10 years ago.
Second best problem of this year's AoC I'd say.

"Arithmetic Logic Unit" is the only problem I managed to solve using pen and paper.
Best problem of this year's AoC on my rating :) It has a few hidden layers that
transform a problem statement in leasantly surprising ways. It remindedme of
https://www.hacker.org challenges. These are the best :)

Parting words
=============

While coding the solutions I made many mistakes like use-after-free, index
out-of-bounds and similar. Borrow checker errors are great at explaining
the lifetimes and sources of references. Integer overflow runtime checks
are also nice.

It feels like **rust** is very explicit about value moves and copies. It's a
nice feeling of confidence where you create (potentially expensive) object
copies and when you can just cheaply move them around.

Compiler warnings are also superb: superfluous **mut** annotations, dead code,
even accidental camel case!

If you are thinking of trying **rust** but did not yet you certainly should.

- **rust** is very friendly to newcomers like me
- **evcxr** is a nice REPL
- AoC is cool!

Have fun!
