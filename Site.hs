-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Data.Typeable
import Data.Binary (Binary)

import OutOfTheYards.Content.Clean (clean)
import OutOfTheYards.Content.Normalize (normalizeUrls)
-----------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "assets/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.css" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/**/*.html" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= clean
            >>= normalizeUrls postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst 
                =<< loadAllSnapshots "posts/**/*.html" "content"

            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


-----------------------------------------------------------------------------
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

