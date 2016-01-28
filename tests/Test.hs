module Main (main) where

import Test.Hspec (hspec)

import OutOfTheYards.Content.Clean.Tests as Clean

main :: IO ()
main = hspec $ Clean.tests
    
