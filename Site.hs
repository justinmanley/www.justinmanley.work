-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Data.Typeable
import Data.Binary (Binary)
import Debug.Trace (trace)

import OutOfTheYards.Content.Normalize (normalizeUrls)
-----------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "CNAME" $ do
        route   idRoute
        compile copyFileCompiler

    match "assets/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/style.scss" $ do
        route $ setExtension "css"
        compile $ getResourceString 
            >>= withItemBody
                (unixFilter "sass" [ "-s", "--scss", "--load-path=css"])
            >>= return . fmap compressCss

    match "posts/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "posts/**/*.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "post-body"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= normalizeUrls postCtx
            >>= saveSnapshot "post-full"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst 
                =<< loadAllSnapshots "posts/**/*.md" "post-full"

            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/**/*.md" "post-body"
            renderAtom feedConfiguration feedCtx posts

-----------------------------------------------------------------------------
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Out of the Yards"
    , feedDescription = "Writing about digital art, technology, and architecture."
    , feedAuthorName = "Justin Manley"
    , feedAuthorEmail = "manleyjster@gmail.com"
    , feedRoot = "http://outoftheyards.com"
    }
