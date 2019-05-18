-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (empty, (<|>))
import Data.Binary (Binary)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HashMap
import Data.List (stripPrefix, nub)
import Data.Maybe (isJust, fromMaybe, maybe)
import Data.Monoid (mappend)
import Data.String (fromString)
import Data.Text (Text)
import Data.Typeable
import Hakyll
import System.FilePath (combine, splitExtension, takeBaseName, takeDirectory)

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
                    >>= compileSass  
                    >>= return . fmap compressCss

    match "templates/*" $ compile templateBodyCompiler

    -- Writing
    match "posts/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- Miscellaneous files associated with each post.
    match "posts/*/files/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Metadata for posts published elsewhere on the internet.
    matchMetadata "posts/**/*.md" hasSourceUrl $ do
        -- Posts which have a 'source' field are published somewhere else on the
        -- internet. As a result, they typically do not have a 'body', and
        -- should not be published as standalone posts on this website. Omitting
        -- a `route` statement from this `matchMetadata` block ensures that
        -- these posts do not produce standalone HTML output.
        compile $ postPreviewCompiler

    -- Full content and metadata of posts published on this website.
    matchMetadata "posts/**/*.md" (not . hasSourceUrl) $ do
        route $ setExtension "html"
        compile $ do
            postPreviewCompiler

            -- This must be the last statement in the do-block, since it is
            -- meant to produce the "final" state of each post (the full
            -- HTML for the standalone post).
            pandocCompiler
                >>= saveSnapshot "post-body"
                >>= loadAndApplyTemplate "templates/post.html" siteCtx
                >>= normalizeUrls siteCtx
                >>= saveSnapshot "post-full"
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

    create ["writing/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshotsMatchingMetadata "posts/**/*.md" "post-preview" (not . hasTag "tech")
            let archiveCtx =
                    listField "posts" siteCtx posts `mappend`
                    defaultContext

            makeItem ""
                >>= loadTemplateWithMetadataAndApply "templates/writing.html" archiveCtx
                <&> withNameInTitle
                >>= uncurry (loadAndApplyTemplate "templates/default.html")
                >>= relativizeUrls

    -- Topic-specific writing archive pages
    mapM_ createWritingArchiveByTag =<< getAllTags "posts/**/*.md"

    -- Talks
    match "talks/*.md" $ do
        -- Omit the `route` statement in order to ensure that talks appear
        -- only on the aggregation page, not as individual pages.
        compile $ pandocCompiler

    create ["talks/index.html"] $ do
        route idRoute
        compile $ do
            let talks = recentFirst =<< loadAll "talks/*.md"
            let talksCtx =
                    listField "talks" siteCtx talks `mappend`
                    defaultContext

            makeItem ""
                >>= loadTemplateWithMetadataAndApply "templates/talks.html" talksCtx
                <&> withNameInTitle
                >>= uncurry (loadAndApplyTemplate "templates/default.html")

    -- Art
    match "artworks/*/images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "artworks/**/*.md" $ do
        route $ setExtension "html"
        compile $ do
            pandocCompiler
                >>= loadAndApplyTemplate "templates/artwork_preview.html" siteCtx
                >>= normalizeUrls siteCtx
                >>= saveSnapshot "artwork-preview"

            pandocCompiler
                >>= loadAndApplyTemplate "templates/artwork.html" siteCtx
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls

    create ["art/index.html"] $ do
        route idRoute
        compile $ do
            let artworks = recentFirst =<< loadAllSnapshots "artworks/**/*.md" "artwork-preview"
            let galleryCtx =
                    listField "artworks" siteCtx artworks `mappend`
                    defaultContext

            makeItem ""
                >>= loadTemplateWithMetadataAndApply "templates/gallery.html" galleryCtx
                <&> withNameInTitle
                >>= uncurry (loadAndApplyTemplate "templates/default.html")
                >>= relativizeUrls

    -- Pages
    match "pages/*.md" $ do
        route pagesRoutes
        compile $ do
            let pageCtx = mapContextIf (== "title") withName siteCtx

            pandocCompiler
                >>= loadAndApplyTemplate "templates/page.html" siteCtx
                >>= loadAndApplyTemplate "templates/default.html" pageCtx

    -- Home
    create ["index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshots "posts/**/*.md" "post-preview"
            let homeCtx =
                    listField "posts" siteCtx posts `mappend`
                    defaultContext

            makeItem ""
                >>= loadTemplateWithMetadataAndApply "templates/home.html" homeCtx
                <&> withNameInTitle
                >>= uncurry (loadAndApplyTemplate "templates/default.html")
                >>= relativizeUrls

    create ["feed/atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = siteCtx `mappend`
                    bodyField "description"

            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshotsMatchingMetadata "posts/**/*.md" "post-body" (not . hasSourceUrl)
            renderAtom feedConfiguration feedCtx posts

-----------------------------------------------------------------------------

-- Helpers

hasMetadata :: Text -> Metadata -> Bool
hasMetadata key = isJust . HashMap.lookup key

hasSourceUrl :: Metadata -> Bool
hasSourceUrl = hasMetadata "source-url"

postPreviewCompiler :: Compiler (Item String)
postPreviewCompiler = pandocCompiler
    >>= loadAndApplyTemplate "templates/post_preview.html" siteCtx
    >>= normalizeUrls siteCtx
    >>= saveSnapshot "post-preview"

loadAllSnapshotsMatchingMetadata :: (Binary a, Typeable a) => Pattern -> Snapshot -> (Metadata -> Bool) -> Compiler [Item a]
loadAllSnapshotsMatchingMetadata pattern snapshot metadataPred = do
    matching <- map fst . filter (metadataPred . snd) <$> getAllMetadata pattern
    mapM (\i -> loadSnapshot i snapshot) matching

-- If this compiler fails, then you may need to install the `sass` binary (see
-- README for more details).
compileSass :: Item String -> Compiler (Item String)
compileSass = withItemBody (unixFilter "sass" ["css/style.scss"])

siteCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

-- This route performs the following mapping:
--    pages/x.md -> x/index.html
-- This allows such pages to be referenced as just x/ (rather than x.html).
-- That is, this route maps pages/talks.md to talks/index.html.
pagesRoutes :: Routes
pagesRoutes = customRoute $  addIndexHtml . fromMaybe "" . stripPrefix "pages/" . toFilePath where
    addIndexHtml :: FilePath -> FilePath
    addIndexHtml = flip combine "index.html" . fst . splitExtension

-- Extend `Hakyll.Web.Template.loadAndApplyTemplate` to read metadata from the *template* (rather than the
-- item included into the template), add that metadata into the Context, and pass the modified Context
-- on to the next template to use for processing. This is especially helpful for passing data from
-- single-page templates (like `Writing`, `Art`, `Talks`, etc) to the top-level (default) template.
loadTemplateWithMetadataAndApply :: Identifier -> Context a -> Item a -> Compiler (Context a, Item String)
loadTemplateWithMetadataAndApply templateIdentifier context item =
    loadTemplateWithMetadataAndApplyBy templateIdentifier context mappend item 

loadTemplateWithMetadataAndApplyBy :: Identifier
    -> Context a
    -> (Context a -> Context a -> Context a)
    -> Item a
    -> Compiler (Context a, Item String)
loadTemplateWithMetadataAndApplyBy templateIdentifier context mergeContexts item = do
    -- From `loadAndApplyTemplate`.
    tpl <- loadBody templateIdentifier

    -- From `Hakyll.Web.Template.Context.metadataField`.
    let templateMetadataField = Context $ \k _ i -> do
            value <- getMetadataField templateIdentifier k
            maybe empty (return . StringField) value
    let mergedContext = mergeContexts templateMetadataField context

    result <- applyTemplate tpl mergedContext item

    return (mergedContext, result)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

-- Extend `Hakyll.Web.Template.Context.mapContext` to take a predicate indicating
-- whether the map function should be applied to a value at a particular key.
mapContextIf :: (String -> Bool) -> (String -> String) -> Context a -> Context a
mapContextIf keyPredicate f (Context c) = Context $ \k a i -> do
    fld <- c k a i
    if keyPredicate k
    then
        case fld of
            StringField str -> return $ StringField (f str)
            ListField _ _   -> fail $
                "Hakyll.Web.Template.Context.mapContext: " ++
                "can't map over a ListField!"
    else return fld

withNameInTitle :: (Context String, a) -> (Context String, a)
withNameInTitle = mapFst (mapContextIf (== "title") withName)

getTagsFromMetadata :: Metadata -> [String]
getTagsFromMetadata metadata = fromMaybe [] $
    (lookupStringList "tags" metadata)
        <|> (map trim . splitAll "," <$> lookupString "tags" metadata)

getAllTags :: MonadMetadata m => Pattern -> m [String]
getAllTags pattern = do
    allTags <- (map $ getTagsFromMetadata . snd) <$> (getAllMetadata pattern)
    return $ (nub . concat $ allTags)

hasTag :: String -> Metadata -> Bool
hasTag tag metadata = tag `elem` getTagsFromMetadata metadata

-- Apply a compiler and pass along its context along with the compiled result.
-- Useful for constructing chains of template compilations.
withContext :: (Context a -> Item a -> Compiler b) -> Context a -> (Item a -> Compiler (Context a, b))
withContext f ctx item = do
    compiled <- f ctx item
    return (ctx, compiled)

createWritingArchiveByTag :: String -> Rules ()
createWritingArchiveByTag tag =
    create [fromString $ "writing/" ++ tag ++ "/index.html"] $ do
        route idRoute
        compile $ do
            let posts = recentFirst =<< loadAllSnapshotsMatchingMetadata "posts/**/*.md" "post-preview" (hasTag tag)
            let archiveCtx =
                    listField "posts" siteCtx posts `mappend`
                    constField "title" ("Writings on " ++ tag) `mappend`
                    defaultContext

            makeItem ""
                >>= withContext (loadAndApplyTemplate "templates/writing.html") archiveCtx
                <&> withNameInTitle
                >>= uncurry (loadAndApplyTemplate "templates/default.html")
                >>= relativizeUrls


-- Configuration

name :: String
name = "Justin Manley"

tagline :: String
tagline = "Writer, artist, and software artisan."

withName :: String -> String
withName title = name ++ " | " ++ title

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = name
    , feedDescription = withName tagline
    , feedAuthorName = name
    , feedAuthorEmail = "manleyjster@gmail.com"
    , feedRoot = "http://justinmanley.work"
    }
