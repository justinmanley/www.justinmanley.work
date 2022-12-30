module Site.PostLength (minutesToReadPost, extractTextContentsAsForest, totalWords) where

import Data.Tree (Forest, Tree (Node), rootLabel, subForest)
import Text.XML.Light (Content (CRef, Elem, Text), Element (..), cdData, parseXML)

extractTextContentsAsForest :: [Content] -> Forest String
extractTextContentsAsForest c =
  let extractTextContentAsForest c = case c of
        Elem e -> concatMap extractTextContentAsForest (elContent e)
        Text cData ->
          [ Node
              { rootLabel = cdData cData,
                subForest = []
              }
          ]
        CRef s -> []
   in concatMap extractTextContentAsForest c

totalWords :: String -> Int
totalWords =
  sum
    . (map $ sum . fmap (length . words))
    . extractTextContentsAsForest
    . parseXML

-- Average human reading speed
wordsPerMinute :: Int
wordsPerMinute = 200

minutesToReadPost :: String -> Int
minutesToReadPost = (flip div $ wordsPerMinute) . totalWords
