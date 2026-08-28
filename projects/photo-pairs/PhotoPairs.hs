{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

-- Unlike the other projects, this module sits at the root of its own source
-- directory (declared in package.yaml) rather than being named after the
-- directory containing it, because 'photo-pairs' is not a legal Haskell
-- module name and the directory name determines the URL path of the pages
-- below.
module PhotoPairs (compile) where

import Hakyll
  ( Context,
    Rules,
    copyFileCompiler,
    idRoute,
    match,
    route,
    setExtension,
    (.||.),
  )
import Hakyll qualified
import Site.PageCompiler (markdownPageCompiler)

compile :: Context String -> Rules ()
compile projectsContext = do
  -- Static assets for the project as a whole ('projects/photo-pairs/static')
  -- as well as those belonging to an individual page, such as the images for a
  -- single pair ('projects/photo-pairs/book/pair/<pair>/static').
  match ("projects/photo-pairs/static/*" .||. "projects/photo-pairs/**/static/*") $ do
    route idRoute
    Hakyll.compile copyFileCompiler

  match "projects/photo-pairs/**/index.md" $ do
    route $ setExtension "html"
    Hakyll.compile $ markdownPageCompiler projectsContext
