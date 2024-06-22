{-# LANGUAGE OverloadedStrings #-}
module Gnuplot (
    inlineWithGnuplot
) where

import           Hakyll
import qualified Text.Pandoc as TP
import qualified Text.Pandoc.Walk as TPW
import qualified Data.Text as DT

inlineWithGnuplot :: TP.Pandoc -> Compiler TP.Pandoc
inlineWithGnuplot = TPW.walkM inlineGnuplot

{- Support for `SVG` generation with help of `gnuplot` tool in
   inline markdown snippets like:

   ```{render=gnuplot}
   plot [-pi:pi] sin(x)
   ```
 -}
inlineGnuplot :: TP.Block -> Compiler TP.Block
inlineGnuplot cb@(TP.CodeBlock (id, classes, namevals) contents)
  | ("render", "gnuplot") `elem` namevals
  = TP.RawBlock (TP.Format "html") . DT.pack <$> (
      unixFilter "gnuplot"
          [ "--default-settings"
          , "-e", "set terminal svg"
          , "-"]
          (DT.unpack contents))
inlineGnuplot x = return x

