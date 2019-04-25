-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Typeable
import Hakyll

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

    create ["css/style.css"] $ do
        route $ setExtension "css"
        sassStylesheets <- makePatternDependency "css/*.scss"
        rulesExtraDependencies [sassStylesheets] $ do
            compile $ do
                makeItem ""
                    >>= checkSassStylesheets
                    >>= compileSass  
                    >>= return . fmap compressCss

    match "templates/*" $ compile templateCompiler

    -- Writing
    match "posts/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Metadata for posts published elsewhere on the internet.
    matchMetadata "posts/**/*.md" hasSource $ do
        -- Posts which have a 'source' field are published somewhere else on the
        -- internet. As a result, they typically do not have a 'body', and
        -- should not be published as standalone posts on this website. Omitting
        -- a `route` statement from this `matchMetadata` block ensures that
        -- these posts do not produce standalone HTML output.
        compile $ postPreviewCompiler

    -- Full content and metadata of posts published on this website.
    matchMetadata "posts/**/*.md" (not . hasSource) $ do
        route $ setExtension "html"
        compile $ do
            postPreviewCompiler

            -- This must be the last statement in the do-block, since it is
            -- meant to produce the "final" state of each post (the full
            -- HTML for the standalone post).
            pandocCompiler
                >>= saveSnapshot "post-body"
                >>= loadAndApplyTemplate "templates/post.html" postCtx
                >>= normalizeUrls postCtx
                >>= saveSnapshot "post-full"
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["writing/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshots "posts/**/*.md" "post-preview"
            let archiveCtx =
                    listField "posts" postCtx posts `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/writing.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    -- Art
    match "artworks/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "artworks/**/*.md" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/artwork_preview.html" postCtx
                >>= normalizeUrls postCtx
                >>= saveSnapshot "artwork-preview"

            pandocCompiler
                >>= loadAndApplyTemplate "templates/artwork.html" postCtx
                >>= loadAndApplyTemplate "templates/default.html" postCtx
                >>= relativizeUrls

    create ["art/index.html"] $ do
        route idRoute
        compile $ do
            let artworks = recentFirst =<< loadAllSnapshots "artworks/**/*.md" "artwork-preview"
            let galleryCtx =
                    listField "artworks" postCtx artworks `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/gallery.html" galleryCtx
                >>= loadAndApplyTemplate "templates/default.html" galleryCtx
                >>= relativizeUrls

    -- Home
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshots "posts/**/*.md" "post-preview"
            let homeCtx =
                    listField "posts" postCtx posts `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/home.html" homeCtx
                >>= loadAndApplyTemplate "templates/default.html" homeCtx
                >>= relativizeUrls

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshotsMatchingMetadata "posts/**/*.md" "post-body" (not . hasSource)
            renderAtom feedConfiguration feedCtx posts

-----------------------------------------------------------------------------

-- Helpers

hasMetadata :: Text -> Metadata -> Bool
hasMetadata key = isJust . HashMap.lookup key

hasSource :: Metadata -> Bool
hasSource = hasMetadata "source"

postPreviewCompiler :: Compiler (Item String)
postPreviewCompiler = pandocCompiler
    >>= loadAndApplyTemplate "templates/post_preview.html" postCtx
    >>= normalizeUrls postCtx
    >>= saveSnapshot "post-preview"

loadAllSnapshotsMatchingMetadata :: (Binary a, Typeable a) => Pattern -> Snapshot -> (Metadata -> Bool) -> Compiler [Item a]
loadAllSnapshotsMatchingMetadata pattern snapshot metadataPred = do
    matching <- map fst . filter (metadataPred . snd) <$> getAllMetadata pattern
    mapM (\i -> loadSnapshot i snapshot) matching

-- If this compiler fails with `exit code 1...no such file or directory`,
-- then you need to install `stylelint`. The `stylelint` dependencies are
-- specified in the `package.json` file, so the binary can be installed
-- with just `npm install`. The output of this action is not actually used.
-- If this compiler fails with `exit code 2`, then there is a lint issue
-- with the CSS, and you should run `npx stylelint css/*.scss` at the command
-- line to diagnose the problem.
checkSassStylesheets :: Item String -> Compiler (Item String)
checkSassStylesheets = withItemBody (unixFilter "npx" ["stylelint", "css/*.scss"])

-- If this compiler fails, then you may need to install the `sass` binary (see
-- README for more details).
compileSass :: Item String -> Compiler (Item String)
compileSass = withItemBody (unixFilter "sass" ["css/style.scss"])

postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


-- Configuration

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "Out of the Yards"
    , feedDescription = "Writing about digital art, technology, and architecture."
    , feedAuthorName = "Justin Manley"
    , feedAuthorEmail = "manleyjster@gmail.com"
    , feedRoot = "http://outoftheyards.com"
    }
