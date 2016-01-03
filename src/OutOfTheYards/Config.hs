{-# LANGUAGE OverloadedStrings #-}

module OutOfTheYards.Config where

import Data.Text
import System.FilePath ((</>))

tokenCache :: Maybe FilePath
tokenCache = Just $ "_secret" </> "cached_auth_token"

-- | The name (title) of the Google Drive folder where files for this --   blog are located.
targetDir :: String
targetDir = "'Out of the Yards'"

-- | All files in the target directory with the 'Description' field
--   equal to this string will be downloaded and published.
publishedDescription :: Maybe Text
publishedDescription = Just "Published" 
