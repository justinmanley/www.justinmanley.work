{-# LANGUAGE OverloadedStrings #-}

module Site.Strings (removeString) where

import Data.Text as Text (Text, pack, replace, unpack)

replaceString :: Text -> Text -> String -> String
replaceString find replacement = Text.unpack . replace find replacement . Text.pack

removeString :: Text -> String -> String
removeString substring = replaceString substring ""