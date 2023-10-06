{-# LANGUAGE OverloadedStrings #-}
module Graphviz (
    inlineDotWithGrapthviz
) where

import           Hakyll
import qualified Text.Pandoc as TP
import qualified Text.Pandoc.Walk as TPW
import qualified Data.Text as DT

inlineDotWithGrapthviz :: TP.Pandoc -> Compiler TP.Pandoc
inlineDotWithGrapthviz = TPW.walkM inlineDot

{- Support for `SVG` generation with help of graphviz `dot` tool in
   inline markdown snippets like:

   ```{render=dot}
   digraph { A -> B }
   ```
 -}
inlineDot :: TP.Block -> Compiler TP.Block
inlineDot cb@(TP.CodeBlock (id, classes, namevals) contents)
  | lookup "render" namevals == Just "dot"
  = TP.RawBlock (TP.Format "html") . DT.pack <$> (unixFilter "dot" ["-Tsvg"] (DT.unpack contents))
inlineDot x = return x

