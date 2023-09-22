{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Lifestory.Lifestory (compile) where

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
  -- TODO: Create a "compiler" which clones the lifescroll repo and
  -- checks out the specified commit hash.

  -- TODO: Replace this with a match for lifescroll/src/*.ts and
  -- a compiler which runs webpack to generate the javascript bundle.
  match "projects/lifestory/**/lifescroll/main.js" $ do
    route $ removeSubstringRoute "lifescroll/"
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/**/lifescroll/page.js" $ do
    route $ removeSubstringRoute "lifescroll/"
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/**/patterns/*.rle" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/**/patterns/*.json" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lifestory/**/templates/*.html" $
    Hakyll.compile templateBodyCompiler

  match ("projects/lifestory/**/fragments/*.md" .||. "projects/lifestory/**/fragments/*.html") $ do
    Hakyll.compile pandocCompiler

  match "projects/lifestory/**/index.md" $ do
    route $ setExtension "html"
    Hakyll.compile $ pageCompiler context

  match "projects/lifestory/**/style.scss" $ do
    route $ setExtension "css"
    Hakyll.compile $ do
      getResourceString
        >>= compileSass
        <&> fmap compressCss

  match "projects/lifestory/**/images/**" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  maybeMaxVersion <-
    preprocess $
      maxVersion <$> listDirectory "projects/lifestory/version"
  case maybeMaxVersion of
    Nothing -> return ()
    Just v -> match (fromGlob $ "projects/lifestory/version/" ++ show v ++ "/index.md") $ version "latest" $ do
      route $ constRoute "projects/lifestory/index.html"
      Hakyll.compile $ pageCompilerTransformingUrls (withVersionedUrl v) context
  where
    context =
      field "head" (compileHead projectsContext)
        <> field "footer" (loadVersionedItemBody "fragments/footer.md")
        <> projectsContext

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

-- I would prefer to use Text.XML.Light along with (a modified version of)
-- Site.Urls.mapAttrs. However, Text.XML.Light.showContent garbles the HTML
-- output, encoding the <pattern-anchor> tag with &gt; and &lt; characters,
-- and I'm not sure how to fix it. It's easier to use TagSoup instead.
addAtomicUpdateRegionToPatternAnchors :: String -> String
addAtomicUpdateRegionToPatternAnchors =
  TagSoup.renderTags . map tag . TagSoup.parseTags
  where
    tag (TagSoup.TagOpen "pattern-anchor" attributes) =
      TagSoup.TagOpen "pattern-anchor" $ concatMap transformAttr attributes
    tag x = x

    transformAttr (k, v) =
      if k == "src"
        then [(k, v), ("rendering-options", replaceString "rle" "json" v)]
        else [(k, v)]

maxVersion :: [FilePath] -> Maybe Integer
maxVersion paths =
  foldr maybeMax Nothing versions
  where
    versions :: [Integer]
    versions = mapMaybe readMaybe paths

    maybeMax :: Integer -> Maybe Integer -> Maybe Integer
    maybeMax i Nothing = Just i
    maybeMax i (Just j) = Just $ max i j

-- Rather than calling this on the same page in multiple places, it would be better
-- to cache a snapshot and then load it. However, it seems difficult to do this without
-- introducing a circular dependency.
pageCompilerTransformingUrls :: (FilePath -> FilePath -> FilePath) -> Context String -> Compiler (Item String)
pageCompilerTransformingUrls transformFilepath context =
  pandocCompiler
    >>= loadAndApplyVersionedTemplate "templates/body.html" context
    >>= loadAndApplyTemplate "templates/default.html" context
    >>= withUrlsCompiler (\route -> transformFilepath route . fullPathForItem route)
    >>= relativizeUrls
    <&> fmap addAtomicUpdateRegionToPatternAnchors

withVersionedUrl :: Integer -> FilePath -> FilePath -> FilePath
withVersionedUrl version _ path =
  -- TODO: Make paths point to the right places in a way that doesn't require
  -- a hardcoded exemption for the 'technical-challenges' blog post (which is
  -- the only link on the page that should _not_ be rewritten because it is not
  -- part of the versioned "bundle".
  if "technical-challenges" `isSuffixOf` path
    then path
    else
      replaceString
        "projects/lifestory"
        (Text.pack $ "projects/lifestory/version/" ++ show version)
        path

-- Compiler with no extra URL transformation
pageCompiler :: Context String -> Compiler (Item String)
pageCompiler = pageCompilerTransformingUrls (const id)

compileHead :: Context String -> Item String -> Compiler String
compileHead context item = do
  head <- loadAndApplyTemplate "templates/post_head.html" context item
  scripts <- loadVersionedItemBody "fragments/head.html" item
  return $ itemBody head ++ "\n" ++ scripts