-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Typeable
import Hakyll

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
            >>= withItemBody (unixFilter "sass" ["css/style.scss"])
            >>= return . fmap compressCss

    match "posts/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "templates/*" $ compile templateCompiler

    -- Metadata for posts published elsewhere on the internet.
    matchMetadata "posts/**/*.md" hasSource $
        -- Posts which have a 'source' field are published somewhere else on the internet.
        -- As a result, they typically do not have a 'body', and should not be included in
        -- the archive of posts published on this website.
        compile $ pandocCompiler

    -- Full content and metadata of posts published on this website.
    matchMetadata "posts/**/*.md" (not . hasSource) $ do
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
            let featuredPosts = recentFirst =<< loadAll "posts/**/*.md"
            let archiveCtx =
                    listField "posts" postCtx featuredPosts `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/home.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshotsMatchingMetadata "posts/**/*.md" "post-body" (not . hasSource)
            renderAtom feedConfiguration feedCtx posts

-----------------------------------------------------------------------------
hasMetadata :: Text -> Metadata -> Bool
hasMetadata key = isJust . HashMap.lookup key

hasSource :: Metadata -> Bool
hasSource = hasMetadata "source"

loadAllSnapshotsMatchingMetadata :: (Binary a, Typeable a) => Pattern -> Snapshot -> (Metadata -> Bool) -> Compiler [Item a]
loadAllSnapshotsMatchingMetadata pattern snapshot metadataPred = do
    matching <- map fst . filter (metadataPred . snd) <$> getAllMetadata pattern
    mapM (\i -> loadSnapshot i snapshot) matching

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
