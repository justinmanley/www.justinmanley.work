{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifestory.Lifestory (compile) where

import Data.Functor ((<&>))
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
    compressCss,
    copyFileCompiler,
    customRoute,
    defaultContext,
    getResourceString,
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
    unixFilter,
    withItemBody,
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
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/template.html" $ Hakyll.compile templateBodyCompiler

  match "projects/lifestory/version/*/index.md" $ do
    route $ setExtension "html"
    Hakyll.compile $ do
      pandocCompiler
        >>= loadAndApplyVersionedTemplate projectsContext
        >>= fullPathForItemCompiler
        >>= relativizeUrls

  match "projects/lifestory/version/*/style.scss" $ do
    route $ setExtension "css"
    Hakyll.compile $ do
      getResourceString
        >>= compileSass
        <&> fmap compressCss

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

-- TODO: See if it is possible to consolidate this with the compileSass
-- function in Site.hs.
-- If this compiler fails, then you may need to install the `sass` binary (see
-- README for more details).
compileSass :: Item String -> Compiler (Item String)
compileSass = withItemBody (unixFilter "sass" ["--stdin"])
