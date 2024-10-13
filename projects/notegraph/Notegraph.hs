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
import Site.PageCompiler (pageCompiler)
import Site.Sass (compileSass)
import Site.Strings (removeString)
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

  match
    ( "projects/notegraph/node_modules/mathjax/es5/tex-chtml.js"
        .||. "projects/notegraph/node_modules/mathjax/es5/output/chtml/fonts/woff-v2/MathJax_Zero.woff"
        .||. "projects/notegraph/node_modules/mathjax/es5/output/chtml/fonts/woff-v2/MathJax_Main-Regular.woff"
    )
    $ do
      route $ removeSubstringRoute "node_modules/"
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