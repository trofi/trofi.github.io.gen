{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Monad as CM
import           Hakyll

import qualified AbsolutizeUrls as AU
import qualified PandocWithInlines as PWI

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
        compile $ PWI.pageCompiler >>= PWI.withPandocItemBody (\html ->
            loadAndApplyTemplate "templates/default.html" postCtx html
            >>= relativizeUrls)

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ PWI.pageCompiler >>= PWI.withPandocItemBody (\html ->
            loadAndApplyTemplate "templates/default.html" postCtx html
            >>= relativizeUrls)

    match "static/README.md" $ do
        route   (constRoute "README.md")
        compile copyFileCompiler

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< (fmap (fmap (itemBody . PWI.renderedRSS))) <$> (loadAll "posts/*" :: Compiler [Item PWI.PWI])
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
            (loadAll "posts/*" :: Compiler [Item PWI.PWI])
                >>= fmap (take 30) . recentFirst . fmap (fmap (itemBody . PWI.renderedRSS))
                >>= (CM.mapM $ AU.absolutizeUrls rssRoot)
                >>= renderAtom (feedConfiguration "All posts") feedCtx

    create ["feed/rss.xml"] $ do
        route idRoute
        compile $ do
            (loadAll "posts/*" :: Compiler [Item PWI.PWI])
                >>= fmap (take 30) . recentFirst . fmap (fmap (itemBody . PWI.renderedRSS))
                >>= (CM.mapM $ AU.absolutizeUrls rssRoot)
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
    , feedRoot = "https://trofi.github.io"
    }

rssRoot :: String
rssRoot = feedRoot $ feedConfiguration $ error "title should not be used"
