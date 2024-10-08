{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module LightningField.LightningField (compile) where

import Hakyll
  ( Compiler,
    Context,
    Item,
    Rules,
    copyFileCompiler,
    field,
    idRoute,
    loadBody,
    match,
    pandocCompiler,
    route,
    setExtension,
    (.||.),
  )
import Hakyll qualified
import Site.PageCompiler (pageCompiler)

compile :: Context String -> Rules ()
compile projectsContext = do
  -- TODO: Create a "compiler" which clones the notegraph-tutorial repo and
  -- checks out the specified commit hash.

  -- TODO: Replace this with a match for notegraph-tutorial/src/*.ts and
  -- a compiler which runs webpack to generate the javascript bundle.
  match "projects/lightningfield/static/main.js" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lightningfield/static/style.css" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/lightningfield/static/*.svg" $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match ("projects/lightningfield/fragments/*.md" .||. "projects/lightningfield/fragments/*.html") $ do
    Hakyll.compile pandocCompiler

  match "projects/lightningfield/index.html" $ do
    route $ setExtension "html"
    Hakyll.compile $ pageCompiler context
  where
    context =
      field "head" (\_ -> loadBody "projects/lightningfield/fragments/head.html")
        <> projectsContext
