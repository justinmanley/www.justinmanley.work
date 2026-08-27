{-# LANGUAGE OverloadedStrings #-}

module Site.PageCompiler (pageCompiler, markdownPageCompiler) where

import Data.List (stripPrefix)
import Hakyll (Compiler, Context, Item, getResourceBody, getRoute, itemIdentifier, loadAndApplyTemplate, pandocCompiler, relativizeUrls, withUrls)
import Site.Strings (removeString)

fullPathForItem :: FilePath -> FilePath -> FilePath
fullPathForItem root path =
  case stripPrefix "./" path of
    Nothing -> path
    Just basePath -> "/" ++ removeString "index.html" root ++ basePath

-- Surely there is a better way to do this? I'm not sure why this is even
-- necessary in the first place. For some reason, page-relative links are
-- interpreted relative to the parent directory, not the directory in which
-- the page is located.
withUrlsCompiler :: (FilePath -> FilePath -> FilePath) -> Item String -> Compiler (Item String)
withUrlsCompiler transformFilepath item = do
  maybeRoute <- getRoute $ itemIdentifier item
  return $ case maybeRoute of
    Nothing -> item
    Just route -> withUrls (transformFilepath route) <$> item

-- Rather than calling this on the same page in multiple places, it would be better
-- to cache a snapshot and then load it. However, it seems difficult to do this without
-- introducing a circular dependency.
pageCompilerTransformingUrls :: Compiler (Item String) -> (FilePath -> FilePath -> FilePath) -> Context String -> Compiler (Item String)
pageCompilerTransformingUrls bodyCompiler transformFilepath context =
  bodyCompiler
    >>= loadAndApplyTemplate "templates/post.html" context
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= withUrlsCompiler (\route -> transformFilepath route . fullPathForItem route)
    >>= relativizeUrls

-- Compiler for pages whose body is already HTML, with no extra URL transformation.
pageCompiler :: Context String -> Compiler (Item String)
pageCompiler = pageCompilerTransformingUrls getResourceBody (const id)

-- Compiler for pages whose body is written in Markdown, with no extra URL
-- transformation. Raw HTML embedded in the Markdown is passed through untouched.
markdownPageCompiler :: Context String -> Compiler (Item String)
markdownPageCompiler = pageCompilerTransformingUrls pandocCompiler (const id)