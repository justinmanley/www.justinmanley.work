{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- Unlike the other projects, this module sits at the root of its own source
-- directory (declared in package.yaml) rather than being named after the
-- directory containing it, because 'photo-pairs' is not a legal Haskell
-- module name and the directory name determines the URL path of the pages
-- below.
module PhotoPairs (compile) where

import Data.List (stripPrefix)
import Hakyll
  ( Context,
    Routes,
    Rules,
    composeRoutes,
    constField,
    constRoute,
    copyFileCompiler,
    customRoute,
    fromFilePath,
    getMatches,
    idRoute,
    match,
    matchRoute,
    route,
    setExtension,
    toFilePath,
    (.||.),
  )
import Hakyll qualified
import Hakyll.Web.Redirect (createRedirects)
import Site.PageCompiler (markdownPageCompiler)
import System.FilePath (replaceExtension, takeDirectory, takeFileName, (</>))

compile :: Context String -> Rules ()
compile projectsContext = do
  -- Static assets for the project as a whole ('projects/photo-pairs/static')
  -- as well as those belonging to an individual page, such as the images and
  -- the recording for a single pair.
  match ("projects/photo-pairs/static/*" .||. "projects/photo-pairs/**/static/*") $ do
    -- A pair's own assets move with the pair, so that a page and everything it
    -- loads sit together under the pair's URL. The pages refer to them with
    -- page-relative links ('./static/...'), which resolve against that URL.
    route $ matchRoute pairAssets pairRoute <> idRoute
    Hakyll.compile copyFileCompiler

  match "projects/photo-pairs/**/index.md" $ do
    -- The book and its pairs are published under '/pairwise', a short URL
    -- which is easier to say out loud, to print, and to link to than the path
    -- to the page in the source tree. Every other page of the project is
    -- published at the path where it sits in the source tree.
    route $
      matchRoute bookPage (constRoute "pairwise/index.html")
        <> matchRoute pairPages (pairRoute `composeRoutes` setExtension "html")
        <> setExtension "html"
    Hakyll.compile $ markdownPageCompiler context

  -- These pages were published under 'projects/photo-pairs/book' before they
  -- moved to '/pairwise', so leave redirects behind for links which still
  -- point there.
  pairs <- getMatches pairPages
  createRedirects $
    ("projects/photo-pairs/book/index.html", "/pairwise")
      : [(publishedAt pair, pairUrl pair) | pair <- pairs]
  where
    bookPage = "projects/photo-pairs/book/index.md"
    pairPages = "projects/photo-pairs/book/pair/*/index.md"
    pairAssets = "projects/photo-pairs/book/pair/*/static/*"

    -- Where a page used to be published, which is where the redirect goes.
    publishedAt = fromFilePath . (`replaceExtension` "html") . toFilePath

    pairUrl pair = "/pairwise/" ++ takeFileName (takeDirectory (toFilePath pair))

    -- Every page of the project loads the script which sets up the player for
    -- a recording, rather than just those pages which have a recording,
    -- because the script does nothing on a page without one.
    context =
      constField "head" recordingScript <> projectsContext

    recordingScript =
      "<script defer src=\"/projects/photo-pairs/static/recording.js\"></script>"

-- Publish a pair, and everything belonging to it, under the pair's own name:
--
--    projects/photo-pairs/book/pair/<pair>/index.md       -> pairwise/<pair>/index.md
--    projects/photo-pairs/book/pair/<pair>/static/<file>  -> pairwise/<pair>/static/<file>
--
-- The extension of a page is set separately, by the route this one composes
-- with, so that this function has only one job.
pairRoute :: Routes
pairRoute = customRoute $ \identifier ->
  let path = toFilePath identifier
   in case stripPrefix pairSourceDirectory path of
        Just pairPath -> "pairwise" </> pairPath
        Nothing -> path

pairSourceDirectory :: FilePath
pairSourceDirectory = "projects/photo-pairs/book/pair/"
