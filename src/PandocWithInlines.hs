{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module PandocWithInlines (pageCompiler, PWI(..)) where

import qualified Control.Monad as CM
import qualified Control.Monad.State as CMS
import qualified Data.Binary as DB
import qualified Data.ByteString as DB
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Maybe as DM
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified GHC.Generics as GG

import qualified Hakyll as H
import qualified Hakyll.Web.Pandoc as HWP

import System.FilePath ((</>))
import qualified System.FilePath as SF

import qualified Text.Pandoc as TP
import qualified Text.Pandoc.Builder as TPB
import qualified Text.Pandoc.Walk as TPW
import qualified Text.Pandoc.Options as TPO

data PWI = PWI {
    -- rendered HTML page, inlines already moved out to external links
    rendered :: H.Item String
    -- rendered HTML page for RSS, the main difference from `rendered` is
    -- disabled highlighting and no postAction ran.
  , renderedRSS :: H.Item String
    -- inline data referred by HTML pages
  , inlines :: [(String, H.Item DBL.ByteString)]
} deriving (GG.Generic)

deriving instance DB.Binary PWI

instance H.Writable PWI where
    write path item = do
        -- emit page itself:
        let PWI pand _pandRSS inls = H.itemBody item
        H.write path pand
        -- emit inlines nearby:
        CM.forM_ inls $ \(fp, contents) -> do
            H.makeDirectories fp
            H.write fp contents

pandocReaderOptions :: TPO.ReaderOptions
pandocReaderOptions = HWP.defaultHakyllReaderOptions {
    TPO.readerStandalone = True
}

pandocWriterOptions :: TPO.WriterOptions
pandocWriterOptions = HWP.defaultHakyllWriterOptions{
    TPO.writerHTMLMathMethod = TPO.MathML
}

pandocRSSWriterOptions :: TPO.WriterOptions
pandocRSSWriterOptions = pandocWriterOptions{
    -- disable highlighting
    TPO.writerHighlightStyle = Nothing
}

data PandocInline = DotInline DT.Text
                  | GnuplotInline DT.Text

type PandocInlines = [(String, PandocInline)]

pandocInlineCompiler :: PandocInline -> H.Compiler (H.Item DBL.ByteString)
pandocInlineCompiler pil = do
    out <- case pil of
        DotInline contents ->
            H.unixFilterLBS "dot" ["-Tsvg"]
                                  (DB.fromStrict $ DTE.encodeUtf8 contents)
        GnuplotInline contents ->
            H.unixFilterLBS "gnuplot" [ "--default-settings"
                                      , "-e", "set terminal svg"
                                      , "-"]
                                      (DB.fromStrict $ DTE.encodeUtf8 contents)
    H.makeItem out

{-
   Traverse `pandoc` and substitute raw blocks with links to external
   files. Examples:

   - Gnuplot tool:

     ```{render=gnuplot}
     plot [-pi:pi] sin(x)
     ```

  - Graphviz `dot` tool:

     ```{render=dot}
     digraph D { A -> B }
     ```

  Removed blocks are returned as pairs of file paths and raw content
  insode the quotes.
-}
pandocExtractInlines :: FilePath -> FilePath -> TP.Pandoc -> (TP.Pandoc, (Int, PandocInlines))
pandocExtractInlines urlPrefix pathPrefix pand =
    let inline :: TP.Block -> CMS.State (Int, PandocInlines) TP.Block
        inline (TP.CodeBlock (_bid, _classes, namevals) contents)
          | ("render", "dot") `elem` namevals = do
              (n, l) <- CMS.get
              let fn = "fig-" ++ show n ++ ".gv.svg"
              let url = urlPrefix ++ "/" ++ fn
              let e = DotInline contents
              CMS.put ((succ n, (pathPrefix </> fn, e):l))
              return $ TP.Plain [TPB.Image TP.nullAttr [] ((DT.pack url), "")]
          | ("render", "gnuplot") `elem` namevals = do
              (n, l) <- CMS.get
              let fn = "fig-" ++ show n ++ ".gp.svg"
              let url = urlPrefix ++ "/" ++ fn
              let e = GnuplotInline contents
              CMS.put ((succ n, (pathPrefix </> fn, e):l))
              return $ TP.Plain [TPB.Image TP.nullAttr [] ((DT.pack url), "")]
        inline x = return x
    in CMS.runState (TPW.walkM inline pand) (0, [])

pageCompiler :: (H.Item String -> H.Compiler (H.Item String)) -> H.Compiler (H.Item PWI)
pageCompiler postRenderAction = do
    -- store inlines in a separate directory tree:
    -- $root/posts/foo.html
    -- $root/posts.data.inline/foo/fig-0.gv.svg
    identifier <- H.getUnderlying
    itemName <- SF.takeBaseName . DM.fromJust <$> H.getRoute identifier
    let itemRel = "posts.data.inline" </> itemName

    let pathPrefix = H.destinationDirectory H.defaultConfiguration </> itemRel
    let urlPrefix = "/" ++ itemRel

    is <- H.getResourceBody
    ipandoc <- H.readPandocWith pandocReaderOptions is
    let (transformed, (_ilen, inls)) = pandocExtractInlines urlPrefix pathPrefix $ H.itemBody $ ipandoc
    itransformed <- H.makeItem transformed
    let irendered = H.writePandocWith pandocWriterOptions itransformed
    let irenderedRSS = H.writePandocWith pandocRSSWriterOptions itransformed

    ipost_rendered <- postRenderAction irendered

    irendered_inlines <- CM.forM inls $ \(fp, pil) -> do
        ri <- pandocInlineCompiler pil
        return (fp, ri)

    H.makeItem $ PWI ipost_rendered irenderedRSS irendered_inlines
