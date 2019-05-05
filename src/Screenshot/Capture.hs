module Screenshot.Capture (NamedScreenshots, captureScreenshots, screenshotsDir, stripSuffix, mapWithKeyM_) where

import Data.ByteString.Lazy (ByteString)
import Data.Functor ((<&>))
import Data.List (stripPrefix, delete)
import qualified Data.Map as Map
import Data.Map (Map)
import Graphics.Image.Processing.Binary
import Graphics.Image (decode, Image, VS, RGB)
import Graphics.Image.IO.Formats (PNG(PNG))
import Maybes (orElse)
import System.FilePath ((</>))
import System.FilePath.Find (find, always, extension, (==?))
import System.FilePath.Posix(pathSeparator)
import Test.WebDriver (WDConfig, useBrowser, chrome, defaultConfig, wdHost, wdPort, wdHTTPRetryCount, chromeDriverVersion, runSession, runWD)
import Test.WebDriver.Commands (screenshot, openPage, closeSession)
import Test.WebDriver.Session (getSession)
import Test.WebDriver.Monad (WD)

type NamedScreenshots = Map String ByteString

captureScreenshots :: IO (Map String ByteString)
captureScreenshots = do
    pages <- find always (extension ==? ".html") siteDir

    session <- runSession chromeConfig $ getSession
    screenshots <- mapM (runWD session . captureScreenshot) pages

    runWD session closeSession

    return $ Map.fromList screenshots

    
captureScreenshot :: String -> WD (String, ByteString)
captureScreenshot filepath = do
    openPage $ "http://localhost:8000" </> page
    screenshotBytes <- screenshot 
    return (pageName, screenshotBytes)

    where
        stripIndexHtmlSuffix :: String -> Maybe String
        stripIndexHtmlSuffix = stripSuffix (pathSeparator : "index.html")

        slashToUnderscore :: Char -> Char
        slashToUnderscore char = if char == '/' then '_' else char

        page, pageName :: String
        page = delete pathSeparator . orIdentity (stripPrefix siteDir) . orIdentity stripIndexHtmlSuffix $ filepath
        pageName = map slashToUnderscore $ if page == "" then "home" else page

chromeConfig :: WDConfig
chromeConfig = useBrowser chromeBrowser defaultConfig
    { wdHost = "0.0.0.0"
    , wdPort = 4444
    , wdHTTPRetryCount = 50
    }

chromeBrowser = chrome
    { chromeDriverVersion = Just "73"
    }

screenshotsDir :: String
screenshotsDir = "tests/Screenshot/testdata/screenshots"

siteDir :: String
siteDir = "_site"

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix pattern = fmap reverse . stripPrefix (reverse pattern) . reverse

orIdentity :: (a -> Maybe a) -> a -> a
orIdentity f x = (f x) `orElse` x

mapWithKeyM_ :: Monad m => (k -> v1 -> m b) -> Map k v1 -> m ()
mapWithKeyM_ f m = mapM_ (uncurry f) $ Map.toList m
