{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifestory.Lifestory (compile) where

import Data.List (isPrefixOf, stripPrefix)
import Data.Text as Text (Text, pack, replace, unpack)
import Debug.Trace (trace)
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item (itemBody),
    Routes,
    Rules,
    copyFileCompiler,
    customRoute,
    defaultContext,
    getRoute,
    idRoute,
    listFieldWith,
    loadAndApplyTemplate,
    makeItem,
    match,
    pandocCompiler,
    relativizeUrls,
    route,
    setExtension,
    templateBodyCompiler,
    toSiteRoot,
    withUrls,
  )
import Hakyll qualified
import Hakyll.Core.Identifier as Identifier
import Hakyll.Core.Item (itemIdentifier)
import System.FilePath (joinPath)

compile :: Context String -> Rules ()
compile projectsContext = do
  -- TODO: Create a "compiler" which clones the lifestory repo and
  -- checks out the specified commit hash

  -- TODO: Replace this with a match for lifescroll/src/Main.elm and
  -- a compiler which runs `elm make --output=main.js`
  match "projects/lifestory/version/*/lifescroll/main.js" $ do
    route $ removeSubstringRoute "lifescroll/"
    Hakyll.compile copyFileCompiler

  -- TODO: Replace this with a compiler which runs `npm install`.
  match "projects/lifestory/version/*/lifescroll/node_modules/elm-canvas/elm-canvas.js" $ do
    route $ removeSubstringRoute "lifescroll/node_modules/elm-canvas/"
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/lifescroll/page.js" $ do
    route $ removeSubstringRoute "lifescroll/"
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/patterns/*.rle" $ do
    route $ idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/template.html" $ Hakyll.compile templateBodyCompiler

  match "projects/lifestory/version/*/index.md" $ do
    route $ setExtension "html"
    Hakyll.compile $ do
      pandocCompiler
        >>= loadAndApplyVersionedTemplate projectsContext
        >>= fullPathForItemCompiler
        >>= relativizeUrls

loadAndApplyVersionedTemplate :: Context String -> Item String -> Compiler (Item String)
loadAndApplyVersionedTemplate context item =
  loadAndApplyTemplate (versionedTemplateIdentifier item) context item

versionedTemplateIdentifier :: Item a -> Identifier
versionedTemplateIdentifier =
  Identifier.fromFilePath
    . Text.unpack
    . replace "index.md" "template.html"
    . Text.pack
    . Identifier.toFilePath
    . itemIdentifier

removeSubstringRoute :: Text -> Routes
removeSubstringRoute substring = customRoute $ removeString substring . Identifier.toFilePath

removeString :: Text -> String -> String
removeString substring = Text.unpack . replace substring "" . Text.pack

-- Surely there is a better way to do this? I'm not sure why this is even
-- necessary in the first place. For some reason, page-relative links are
-- interpreted relative to the parent directory, not the directory in which
-- the page is located.
fullPathForItemCompiler :: Item String -> Compiler (Item String)
fullPathForItemCompiler item = do
  maybeRoute <- getRoute $ itemIdentifier item
  return $ case maybeRoute of
    Nothing -> item
    Just route -> withUrls (fullPathForItem route) <$> item

fullPathForItem :: FilePath -> FilePath -> FilePath
fullPathForItem root path =
  case stripPrefix "./" path of
    Nothing -> path
    Just basePath -> "/" ++ removeString "index.html" root ++ basePath