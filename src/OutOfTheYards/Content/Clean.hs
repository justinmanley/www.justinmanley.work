module OutOfTheYards.Content.Clean where

import Text.XML.Light
import Data.List (find)
import Hakyll
import Text.Regex
import Data.Maybe (mapMaybe)

-- Clean artifacts from Google Docs which shouldn't be published,
-- such as footnotes and comments.
clean :: Item String -> Compiler (Item String)
clean (Item identifier body) = 
    let predicates = 
            [ isComment `atDepth` 2
            , isFootnote `atDepth` 2
            , isCommentReference `atDepth` 1
            , isFootnoteReference `atDepth` 1
            , isHr
            ]
    in return $ Item
        { itemBody = cleanWith ppContent predicates body 
        , itemIdentifier = identifier
        }

-- Remove content nodes and any of their children satisfying any of the
-- provided predicates.
-- The function from Content -> String allows us to control the output,
-- which is useful for testing.
cleanWith :: (Content -> String) -> [Content -> Bool] -> String -> String
cleanWith contentToString predicates = concatMap contentToString
    . (foldr (.) id $ map (mapMaybe . filterContent) predicates)
    . parseXML

-- Remove the content or any of its children which satify the predicate.
-- Note that this is the opposite of the way that Prelude.filter works
-- (Prelude.filter removes elements which fail the predicate).
filterContent :: (Content -> Bool)
    -> Content
    -> Maybe Content
filterContent predicate content@(Elem (Element name attrs contents line)) =
    if not . predicate $ content
    then Just . Elem $ Element 
        { elName = name
        , elAttribs = attrs
        , elContent = mapMaybe (filterContent predicate) contents
        , elLine = line
        }
    else Nothing
filterContent _ content = Just content

-- Predicates
isComment :: Content -> Bool
isComment (Elem element) = hasId "cmnt[\\d]*" $ elAttribs element
isComment _ = False

isCommentReference :: Content -> Bool
isCommentReference (Elem e) = hasId "cmnt_ref[\\d]*" $ elAttribs e
isCommentReference _ = False

isFootnote :: Content -> Bool
isFootnote (Elem e) = hasId "ftnt[\\d]*" $ elAttribs e
isFootnote _ = False

isFootnoteReference :: Content -> Bool
isFootnoteReference (Elem e) = hasId "ftnt_ref[\\d]*" $ elAttribs e
isFootnoteReference _ = False

-- There's typically an <hr /> separating the comments and footnotes
-- from the body of the document. Without the comments and footnotes,
-- it doesn't make any sense to keep the <hr />.
isHr :: Content -> Bool
isHr (Elem e) = (qName . elName $ e) == "hr"
isHr _ = False

-- When partially applied to its first two arguments, atDepth returns
-- a predicate which is true if the provided predicate is true for any 
-- children of the content at depth n.
atDepth :: (Content -> Bool) -> Int -> Content -> Bool
atDepth predicate 0 content = predicate content
atDepth predicate n (Elem element) = 
    any (predicate `atDepth` (n - 1)) $ elContent element
atDepth _ _ _ = False

hasId :: String -> [Attr] -> Bool
hasId = hasMatchingAttr "id"

-- Return True if the provided list of attributes has the specified attribute,
-- and the value of that attribute matches the provided regexp.
hasMatchingAttr :: String -> String -> [Attr] -> Bool
hasMatchingAttr attrName attrValPattern = 
        toBool . fmap attrMatches . attrValIfPresent
    where
        attrValIfPresent :: [Attr] -> Maybe String
        attrValIfPresent = fmap attrVal . find ((== attrName) . qName . attrKey)

        attrMatches :: String -> Maybe [String]
        attrMatches = matchRegex (mkRegex attrValPattern)
        
        toBool :: Maybe a -> Bool
        toBool = maybe False (const True)

