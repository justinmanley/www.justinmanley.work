module OutOfTheYards.Content.Normalize (normalizeUrls) where

import Hakyll
import Text.XML.Light
import System.FilePath (dropFileName)

-- Map an attribute transformation function over all nodes in a document.
mapAttrs :: (QName -> Attr -> Attr) -> Content -> Content
mapAttrs f c = case c of
    Elem e -> Elem $ Element 
        { elName = elName e
        , elAttribs = map (f $ elName e) (elAttribs e)
        , elContent = map (mapAttrs f) $ elContent e 
        , elLine = elLine e
        }
    _ -> c

-- Transform a node's attribute based on the node's tag name
-- and the value of the attribute. This is applied to all 
-- attributes / tags in the XML document.
normalizeImageSrc :: String -> QName -> Attr -> Attr
normalizeImageSrc prefix tagName attr =
    if qName tagName == "img" && (qName . attrKey) attr == "src"
    then Attr
        { attrKey = attrKey attr
        , attrVal = "/" ++ prefix ++ attrVal attr
        }
    else attr

-- Transform all relative URLs to be absolute rooted URLs.
normalizeUrls :: Context String -> Item String -> Compiler (Item String)
normalizeUrls ctx item =  do
    maybeRoute <- getRoute (itemIdentifier item)

    return $ case maybeRoute of 
        Nothing -> item
        Just route -> 
            let prefix = dropFileName route
                t = concatMap ppContent 
                    . map (mapAttrs $ normalizeImageSrc prefix) 
                    . parseXML
            in Item 
                { itemBody =  t $ itemBody item
                , itemIdentifier = itemIdentifier item 
                }

