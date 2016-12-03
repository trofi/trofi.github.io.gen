---
title: fuzzing re2c
date: December 2, 2016
---

:PostID: 194
:Title: fuzzing re2c
:Keywords: haskell, fuzzing, re2c
:Categories: notes

My dearest friend maintains `re2c <http://re2c.org/>`_ (The fast
C lexer generator).

Conceptually  **re2c** is a simple tool:
it accepts single source file in **.re** format and generates
file in **.c** format.

It usually works fine but occasionally (mostly in development branches)
**re2c** fails to produce valid output. `Extensive test suite <https://github.com/skvadrik/re2c/tree/master/re2c/test>`_
is meant to cover past errors and complex lexers seen in the wild
but it does not help testing new features and bizarre corner cases.

One day inspired with `QuickFuzz <http://quickfuzz.org/>`_ I've decided
to spend 30 minutes to write a fuzzer for **re2c**.

I've started from very simple model: we'll support only basic
set of operations. The full list is:

- "a"
- "b"
- "(" ... ")" (brackets)
- "*" (star)
- concatenation
- "|" (alternative)

Haskell community has a wonderful **QuickCheck** library (`paper <http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf>`_)
to generate random instances of a given datatype. The only thing I need
is to put these random instances into the file and run **re2c**
selfvalidation on it. **-S** (`skeleton mode <http://re2c.org/manual/features/skeleton/skeleton.html>`_)
option generates a validator, not a normal lexer.

.. raw:: html

   <!--more-->

.. code-block:: haskell

    -- qc_re2c.hs
    {-# LANGUAGE LambdaCase #-}
    
    import qualified Test.QuickCheck as Q
    import qualified Test.QuickCheck.Monadic as QM
    import qualified System.Random as SR
    import qualified System.Process as SP
    import qualified System.Exit as SE
    import qualified Data.ByteString.Char8 as BS
    
    data E = A
           | B
           | Alt E E
           | Cat E E
           | Star E
    
    instance Show E where
      show = \case
        A       -> "\"a\""
        B       -> "\"b\""
        Alt l r -> "(" ++ show l ++ "|" ++ show r ++ ")"
        Cat l r -> "(" ++ show l ++ show r ++ ")"
        Star e  -> "(" ++ show e ++ ")" ++ "*"
    
    instance Q.Arbitrary E where
        arbitrary = do d <- Q.choose (0,5) :: Q.Gen Int
                       arbitrary_d d
    
    arbitrary_d 0 = Q.oneof [ pure A, pure B ]
    arbitrary_d d = Q.frequency [ (10, pure A)
                                , (10, pure B)
                                , (20,  Alt <$> arbitrary_d d' <*> arbitrary_d d')
                                , (20,  Cat <$> arbitrary_d d' <*> arbitrary_d d')
                                , (20, Star <$> arbitrary_d d')
                                ]
        where d' = pred d
    
    foo :: IO [E]
    foo = Q.generate Q.arbitrary
    
    prop_test_re2c :: E -> E -> Q.Property
    prop_test_re2c r1 r2 = QM.monadicIO $ do
        let re_file = unlines [ "/*!re2c"
                              , show r1 ++ " {}"
                              , show r2 ++ " {}"
                              , "*/"
                              ]
        s1 <- QM.run $ do BS.writeFile "a.re" $ BS.pack re_file
                          SP.system "re2c -Werror-undefined-control-flow -S a.re -o a.c 2>>re2c_last_warning || exit 42 && gcc a.c -o a && ./a"
        QM.assert $ s1 `elem` [SE.ExitSuccess, SE.ExitFailure 42]
    
    main :: IO ()
    -- main = Q.quickCheck prop_test_re2c
    main = Q.quickCheckWith Q.stdArgs { Q.maxSuccess = 10000 } prop_test_re2c

Running the thing is easy:

.. code-block:: bash

    $ runhaskell qc_re2c.hs

It generates samples similar to the following:

.. code-block:: c

    /*!re2c
    (((("b")*)*|(("b")*)*))* {}
    "b" {}
    */

We can extend fuzzer to support things like "[^abc]" (set negation), add more regex clauses and so on.

Found issues so far:

- `something scary WRT comparison <https://github.com/skvadrik/re2c/commit/9e63260cebd72183c9401addbf21767edbe39176>`_
- `something even scarier <https://github.com/skvadrik/re2c/commit/2db0433b08bbda2d872eeae26b159a0d5e2daf2a>`_
- `this has nice test case <https://github.com/skvadrik/re2c/commit/d07bc5ce04beb25e1a92600bb36141263b5714e7>`_

Now computer can write (and trim) code samples for you.

Simple!
