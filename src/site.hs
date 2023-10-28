{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Monoid (mappend, mconcat)
import qualified Control.Monad as CM
import           Hakyll
import qualified Hakyll.Web.Pandoc as HWP

import qualified Text.Pandoc.Options as TPO

import qualified AbsolutizeUrls as AU
import qualified Graphviz as G

pageCompiler :: Compiler (Item String)
pageCompiler = pandocCompilerWithTransformM
    HWP.defaultHakyllReaderOptions{
      TPO.readerStandalone = True
    }
    HWP.defaultHakyllWriterOptions{
      TPO.writerHTMLMathMethod = TPO.MathML
    }
    G.inlineDotWithGrapthviz

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts.data/**.dot" $ do
        route   $ setExtension "svg"
        compile $ getResourceLBS >>= withItemBody (unixFilterLBS "dot" ["-Tsvg"])

    match "posts.data/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "pages/*" $ do
        route $ gsubRoute "pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "static/README.md" $ do
        route   (constRoute "README.md")
        compile copyFileCompiler

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "blog"                `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 30) . recentFirst >>= (CM.mapM $ AU.absolutizeUrls rssRoot)
                >>= renderAtom (feedConfiguration "All posts") feedCtx

    create ["feed/rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 30) . recentFirst >>= (CM.mapM $ AU.absolutizeUrls rssRoot)
                >>= renderRss (feedConfiguration "All posts") feedCtx

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = "trofi - " ++ title
    , feedDescription = "trofi's blog"
    , feedAuthorName = "Sergei Trofimovich"
    , feedAuthorEmail = "slyich@gmail.com"
    -- TODO: switch newer entries to https:// to avoid
    -- feed spam with outdated entries.
    , feedRoot = "http://trofi.github.io"
    }

rssRoot :: String
rssRoot = feedRoot $ feedConfiguration $ error "title should not be used"
