module Main where

import Control.Lens ((<&>), (.~), (&), (^.))
import Control.Monad (filterM)
import qualified Data.ByteString.Char8 as BS (pack, empty)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Network.Google
import Network.Google.OAuth2 -- From google-oauth2 package, not gogol 
import Network.Google.Drive 
    ( filesList, flQ, flItems, fId
    , childrenList, clItems, crId
    , filesGet, fDescription, fKind
    )
import Network.Google.Drive.Types 
    ( FileList, ChildList, File
    , driveReadonlyScope
    )
import System.Environment (getEnv)
import System.IO (stdout)

import OutOfTheYards.Config 
    ( tokenCache
    , targetDir
    , publishedDescription
    )

main :: IO ()
main = do
    client <- OAuth2Client
        <$> getEnv "GOOGLE_CLIENT_ID"
        <*> getEnv "GOOGLE_CLIENT_SECRET"

    token <- OAuthToken . BS.pack
        <$> getAccessToken client scopes tokenCache

    logger <- newLogger Debug stdout
    env <- newEnv (FromToken token) <&> envLogger .~ logger 

    published <- runResourceT . runGoogle env $
        locatetarget >>= listFiles >>= filterM isPublished   
        
    putStrLn $ show published

    where
        scopes :: [OAuth2Scope]
        scopes = map (Text.unpack . scopeToText) [driveReadonlyScope]

type FileId = Text

locatetarget :: Google File
locatetarget = do
    q <- send (filesList & flQ .~ locateTargetQuery)

    case q ^. flItems of
        [f] -> 
            return f

        [] -> 
            fail $ "There is no folder called " ++ targetDir

        _ -> 
            fail $ "There are multiple folders called " ++ targetDir

    where
        locateTargetQuery :: Maybe Text
        locateTargetQuery = Just . Text.pack $ "title = " ++ targetDir
 
listFiles :: File -> Google [FileId]
listFiles folder = case folder ^. fId of
    Nothing -> 
        fail $ "Folder " ++ targetDir ++ " has no file id."

    Just folderId -> 
        fmap (catMaybes . map (^. crId) . (^. clItems))
            $ send (childrenList folderId) 

isPublished :: FileId -> Google Bool
isPublished fileId = do
    file <- send (filesGet fileId)
    
    let published = file ^. fDescription == publishedDescription
        doc = file ^. fKind == Text.pack "Document"

    return $ published && doc

