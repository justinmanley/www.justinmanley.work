module Main (main) where

import Test.Hspec (hspec)

import Site.UrlTest (urlTests)
import Site.PostLengthTest (postLengthTests)

main :: IO ()
main = hspec $ do
    urlTests
    postLengthTests
