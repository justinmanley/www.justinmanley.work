{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifestory.Lifestory (compile) where

import Data.Functor ((<&>))
import Data.List (isPrefixOf, stripPrefix)
import Data.Set qualified as Set
import Data.Text as Text (Text, pack, replace, unpack)
import Debug.Trace (trace, traceShow)
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
    field,
    getResourceString,
    getRoute,
    idRoute,
    listFieldWith,
    loadAndApplyTemplate,
    loadBody,
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
import Text.HTML.TagSoup qualified as TagSoup

compile :: Context String -> Rules ()
compile projectsContext = do
  -- TODO: Create a "compiler" which clones the lifestory repo and
  -- checks out the specified commit hash

  -- TODO: Replace this with a match for lifescroll/src/Main.elm and
  -- a compiler which runs `elm make --output=main.js`
  match "projects/lifestory/version/*/lifescroll/main.js" $ do
    route $ removeSubstringRoute "lifescroll/"
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/lifescroll/page.js" $ do
    route $ removeSubstringRoute "lifescroll/"
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/patterns/*.rle" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/patterns/*.json" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/version/*/templates/*.html" $ Hakyll.compile templateBodyCompiler

  match "projects/lifestory/version/*/fragments/*.html" $ do
    Hakyll.compile pandocCompiler

  match "projects/lifestory/version/*/index.md" $
    do
      route $ setExtension "html"
      Hakyll.compile $ do
        pandocCompiler
          >>= loadAndApplyVersionedTemplate "templates/body.html" projectsContext
          >>= loadAndApplyTemplate "templates/default.html" context
          >>= fullPathForItemCompiler
          >>= relativizeUrls
          <&> fmap addAtomicUpdateRegionToPatternAnchors

  match "projects/lifestory/version/*/style.scss" $ do
    route $ setExtension "css"
    Hakyll.compile $ do
      getResourceString
        >>= compileSass
        <&> fmap compressCss
  where
    context = field "head" (loadVersionedItemBody "fragments/head.html") <> projectsContext

loadAndApplyVersionedTemplate :: Text -> Context String -> Item String -> Compiler (Item String)
loadAndApplyVersionedTemplate templateName context item =
  loadAndApplyTemplate (versionedIdentifier templateName item) context item

loadVersionedItemBody :: Text -> Item String -> Compiler String
loadVersionedItemBody templateName item =
  loadBody (versionedIdentifier templateName item)

versionedIdentifier :: Text -> Item a -> Identifier
versionedIdentifier templateName =
  Identifier.fromFilePath
    . Text.unpack
    . replace "index.md" templateName
    . Text.pack
    . Identifier.toFilePath
    . itemIdentifier

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

-- I would prefer to use Text.XML.Light along with (a modified version of)
-- Site.Urls.mapAttrs. However, Text.XML.Light.showContent garbles the HTML
-- output, encoding the <pattern-anchor> tag with &gt; and &lt; characters,
-- and I'm not sure how to fix it. It's easier to use TagSoup instead.
addAtomicUpdateRegionToPatternAnchors :: String -> String
addAtomicUpdateRegionToPatternAnchors =
  TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen tagName attributes) =
      TagSoup.TagOpen tagName $ concatMap transformAttr attributes
    tag x = x

    -- TODO: Restrict this to only pattern-anchor tags (it is currently
    -- being applied to <script> tags as well.)
    transformAttr (k, v) =
      if k == "src"
        then [(k, v), ("rendering-options", replaceString "rle" "json" v)]
        else [(k, v)]
