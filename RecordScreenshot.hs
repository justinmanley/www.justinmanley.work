module Main where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Screenshot.Capture (captureScreenshots, screenshotsDir, mapWithKeyM_)
import System.FilePath ((</>), (<.>))

main :: IO ()
main = do
    screenshots <- captureScreenshots
    mapWithKeyM_ saveScreenshot screenshots

saveScreenshot :: String -> ByteString -> IO ()
saveScreenshot pageName screenshotImage =
    ByteString.writeFile (screenshotsDir </> pageName <.> "png") screenshotImage
