module Main (main) where

import Test.Hspec (hspec)

import Site.UrlTest (urlTests)

main :: IO ()
main = hspec $ do
    urlTests
