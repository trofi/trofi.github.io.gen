-- Based on hakyll's relativizeUrls
-- The only difference is that we pass absolute base.
module AbsolutizeUrls (absolutizeUrls) where

import           Data.List            (isPrefixOf)
import           Hakyll.Core.Compiler
import           Hakyll.Core.Item
import           Hakyll.Web.Html

absolutizeUrls :: String -> Item String -> Compiler (Item String)
absolutizeUrls root item = do
    route <- getRoute $ itemIdentifier item
    return $ case route of
        Nothing -> item
        Just r  -> fmap (absolutizeUrlsWith $ root) item

absolutizeUrlsWith :: String  -- ^ Path to the site root
                   -> String  -- ^ HTML to relativize
                   -> String  -- ^ Resulting HTML
absolutizeUrlsWith root = withUrls rel
  where
    isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
    rel x   = if isRel x then root ++ x else x
