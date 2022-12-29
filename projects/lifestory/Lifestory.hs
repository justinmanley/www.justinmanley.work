{-# LANGUAGE OverloadedStrings #-}

module Lifestory.Lifestory (compile) where

import qualified Hakyll
import Hakyll (
    Compiler, Context, Rules, Item, Identifier,
    match, 
    route, 
    setExtension, 
    pandocCompiler, 
    loadAndApplyTemplate,
    templateBodyCompiler
    )
import Hakyll.Core.Identifier as Identifier
import Hakyll.Core.Item (itemIdentifier)
import Data.Text as Text (replace, pack, unpack)

compile :: Context String -> Rules ()
compile context = do
    match "projects/lifestory/version/*/template.html" $ Hakyll.compile templateBodyCompiler

    match "projects/lifestory/version/*/index.md" $ do
        route $ setExtension "html"
        Hakyll.compile $ do
            pandocCompiler
                >>= loadAndApplyVersionedTemplate context

loadAndApplyVersionedTemplate :: Context String -> Item String -> Compiler (Item String)
loadAndApplyVersionedTemplate context item =
    loadAndApplyTemplate (versionedTemplateIdentifier item) context item

versionedTemplateIdentifier :: Item a -> Identifier 
versionedTemplateIdentifier = Identifier.fromFilePath 
    . Text.unpack . (replace "index.md" "template.html") . Text.pack
    . Identifier.toFilePath 
    . itemIdentifier