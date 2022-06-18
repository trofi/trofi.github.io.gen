{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import qualified Hakyll.Web.Pandoc as HWP

import qualified Text.Pandoc.Options as TPO

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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith HWP.defaultHakyllReaderOptions{TPO.readerStandalone = True} HWP.defaultHakyllWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "static/README.md" $ do
        route   (constRoute "README.md")
        compile copyFileCompiler

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- fmap (take 20) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            loadAll "posts/*"
                >>= fmap (take 30) . recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx

    create ["feed/rss.xml"] $ do
        route idRoute
        compile $ do
            loadAll "posts/*"
                >>= fmap (take 30) . recentFirst
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
    , feedAuthorEmail = "slyfox@inbox.ru"
    , feedRoot = "http://trofi.github.io"
    }
