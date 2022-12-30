module Site.Url (normalizeUrls, normalizeUrlsWithRoute) where

import Data.List (stripPrefix)
import Hakyll
import qualified Network.URL as URL
import System.FilePath (dropFileName)
import Text.XML.Light

-- Map an attribute transformation function over all nodes in a document.
mapAttrs :: (QName -> Attr -> Attr) -> Content -> Content
mapAttrs f c = case c of
  Elem e ->
    Elem $
      Element
        { elName = elName e,
          elAttribs = map (f $ elName e) (elAttribs e),
          elContent = map (mapAttrs f) $ elContent e,
          elLine = elLine e
        }
  _ -> c

-- Transform a node's attribute based on the node's tag name
-- and the value of the attribute. This is applied to all
-- attributes / tags in the XML document.
normalizeImageSrc :: String -> QName -> Attr -> Attr
normalizeImageSrc prefix tagName attr =
  if qName tagName == "img" && (qName . attrKey) attr == "src"
    then
      let isRelative :: String -> Bool
          isRelative s = case URL.importURL s of
            Nothing -> False
            Just url -> case URL.url_type url of
              URL.PathRelative -> True
              _ -> False
          srcUrl = attrVal attr
          rootedUrl =
            if isRelative srcUrl
              then "/" ++ prefix ++ srcUrl
              else srcUrl
       in Attr
            { attrKey = attrKey attr,
              attrVal = rootedUrl
            }
    else attr

-- It is always safe to remove the suffix '/index.html' from link href's
-- because browsers will automatically service '/index.html' if such a file
-- is present in the target directory. Stripping '/index.html' is nice
-- because it makes URLs shorter and hides the implementation, allowing pages
-- to be served differently in the future without changing the URL path.
normalizeLinks :: QName -> Attr -> Attr
normalizeLinks tagName attr =
  if qName tagName == "a" && (qName . attrKey) attr == "href"
    then
      let -- TODO: Replace stripSuffix with a library implementation if one exists.
          stripSuffix :: Eq a => [a] -> [a] -> [a]
          stripSuffix prefix xs = case stripPrefix (reverse prefix) (reverse xs) of
            Just ys -> reverse ys
            Nothing -> xs
          hrefUrl = attrVal attr
       in Attr
            { attrKey = attrKey attr,
              attrVal = stripSuffix "/index.html" hrefUrl
            }
    else attr

-- Visible for testing
normalizeUrlsWithRoute maybeRoute item = case maybeRoute of
  Nothing -> item
  Just route ->
    let prefix = dropFileName route
        t =
          concatMap showContent
            . map (mapAttrs $ normalizeImageSrc prefix)
            . map (mapAttrs normalizeLinks)
            . parseXML
     in Item
          { itemBody = t $ itemBody item,
            itemIdentifier = itemIdentifier item
          }

-- Transform all relative URLs to be absolute rooted URLs.
normalizeUrls :: Context String -> Item String -> Compiler (Item String)
normalizeUrls ctx item = do
  maybeRoute <- getRoute (itemIdentifier item)
  return $ normalizeUrlsWithRoute maybeRoute item
