{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    -- Compress CSS
    match "data/css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- Render posts
    match "data/posts/*" $ do
        route $ setExtension ".html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "data/templates/post.html"
            >>> applyTemplateCompiler "data/templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "全日記")
        >>> requireAllA "data/posts/*" addPostList
        >>> applyTemplateCompiler "data/templates/posts.html"
        >>> applyTemplateCompiler "data/templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "机上日記")
        >>> requireAllA "data/posts/*" (id *** arr (take 3 . reverse . sortByBaseName) >>> addPostList)
        >>> applyTemplateCompiler "data/templates/index.html"
        >>> applyTemplateCompiler "data/templates/default.html"
        >>> relativizeUrlsCompiler

    -- Read templates
    match "data/templates/*" $ compile templateCompiler

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . sortByBaseName)
        >>> require "data/templates/position.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody
config :: HakyllConfiguration
config = defaultHakyllConfiguration { deployCommand = deploy }
  where deploy = "make deploy && make clean"
