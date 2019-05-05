module Main (main) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Test.Hspec (hspec)
import System.FilePath (takeBaseName)
import System.FilePath.Find (find, always, extension, (==?))

import Screenshot.Test (validateScreenshots)
import Screenshot.Capture (NamedScreenshots, captureScreenshots, screenshotsDir, stripSuffix)

main :: IO ()
main = do
    currentScreenshots <- captureScreenshots
    savedScreenshots <- loadSavedScreenshots

    hspec $ do
        validateScreenshots currentScreenshots savedScreenshots
    

loadSavedScreenshots :: IO NamedScreenshots 
loadSavedScreenshots = do
    filepaths <- find always (extension ==? ".png") screenshotsDir
    Map.fromList <$> mapM loadSavedScreenshot filepaths

loadSavedScreenshot :: FilePath -> IO (String, ByteString)
loadSavedScreenshot filepath = do
    screenshotBytes <- ByteString.readFile filepath
    return $ (takeBaseName filepath, screenshotBytes)
