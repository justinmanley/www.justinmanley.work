{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Notegraph.Notegraph (compile) where

import Data.Functor ((<&>))
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text as Text (Text, pack, replace, unpack)
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item (itemBody),
    Routes,
    Rules,
    compressCss,
    constRoute,
    copyFileCompiler,
    customRoute,
    defaultContext,
    field,
    fromGlob,
    getResourceBody,
    getResourceString,
    getRoute,
    idRoute,
    listFieldWith,
    loadAndApplyTemplate,
    loadBody,
    makeItem,
    match,
    pandocCompiler,
    preprocess,
    relativizeUrls,
    route,
    setExtension,
    templateBodyCompiler,
    toSiteRoot,
    unixFilter,
    version,
    withItemBody,
    withUrls,
    (.||.),
  )
import Hakyll qualified
import Hakyll.Core.Identifier as Identifier
import Hakyll.Core.Item (itemIdentifier)
import Hakyll.Core.Provider (resourceFilePath)
import System.Directory (listDirectory)
import System.FilePath (joinPath)
import Text.HTML.TagSoup qualified as TagSoup
import Text.Read (readMaybe)

compile :: Context String -> Rules ()
compile projectsContext = do
  -- TODO: Create a "compiler" which clones the notegraph-tutorial repo and
  -- checks out the specified commit hash.

  -- TODO: Replace this with a match for notegraph-tutorial/src/*.ts and
  -- a compiler which runs webpack to generate the javascript bundle.
  match "projects/notegraph/notegraph-tutorial/main.js" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/notegraph/node_modules/**" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/notegraph/notegraph-tutorial/style.css" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/notegraph/images/**" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/notegraph/examples/*.json" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match ("projects/notegraph/fragments/*.md" .||. "projects/notegraph/fragments/*.html") $ do
    Hakyll.compile pandocCompiler

  match "projects/notegraph/index.html" $ do
    route $ setExtension "html"
    Hakyll.compile $ pageCompiler context

  match "projects/notegraph/style.scss" $ do
    route $ setExtension "css"
    Hakyll.compile $ do
      getResourceString
        >>= compileSass
        <&> fmap compressCss
  where
    context =
      field "head" (\_ -> loadBody "projects/notegraph/fragments/head.html")
        <> projectsContext

removeSubstringRoute :: Text -> Routes
removeSubstringRoute substring = customRoute $ removeString substring . Identifier.toFilePath

removeString :: Text -> String -> String
removeString substring = replaceString substring ""

replaceString :: Text -> Text -> String -> String
replaceString find replacement = Text.unpack . replace find replacement . Text.pack

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

fullPathForItem :: FilePath -> FilePath -> FilePath
fullPathForItem root path =
  case stripPrefix "./" path of
    Nothing -> path
    Just basePath -> "/" ++ removeString "index.html" root ++ basePath

-- TODO: See if it is possible to consolidate this with the compileSass
-- function in Site.hs.
-- If this compiler fails, then you may need to install the `sass` binary (see
-- README for more details).
compileSass :: Item String -> Compiler (Item String)
compileSass = withItemBody (unixFilter "sass" ["--stdin"])

-- Rather than calling this on the same page in multiple places, it would be better
-- to cache a snapshot and then load it. However, it seems difficult to do this without
-- introducing a circular dependency.
pageCompilerTransformingUrls :: (FilePath -> FilePath -> FilePath) -> Context String -> Compiler (Item String)
pageCompilerTransformingUrls transformFilepath context =
  getResourceBody
    >>= loadAndApplyTemplate "templates/post.html" context
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= withUrlsCompiler (\route -> transformFilepath route . fullPathForItem route)
    >>= relativizeUrls

-- Compiler with no extra URL transformation
pageCompiler :: Context String -> Compiler (Item String)
pageCompiler = pageCompilerTransformingUrls (const id)
