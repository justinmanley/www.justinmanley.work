module Screenshot.Test where

import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Map.Merge.Strict (traverseMissing, zipWithAMatched, mergeA)
import Test.WebDriver
import Test.Hspec (Spec, describe, it, shouldBe, SpecWith, Expectation, Arg, expectationFailure)
import Test.Hspec.Core.Spec (SpecM)
import Screenshot.Capture (NamedScreenshots, captureScreenshots, screenshotsDir)

failOnUnpairedScreenshots :: String -> ByteString -> Spec
failOnUnpairedScreenshots pageName _ = do
    it "should have actual and expected screenshots" $ do
        expectationFailure $ pageName ++ " does not have a matching screenshot"

screenshotsAreSimilar :: String -> ByteString -> ByteString -> Spec 
screenshotsAreSimilar pageName actual expected = do
    it "should match" $ do
        actual `shouldBe` expected

validateScreenshots :: NamedScreenshots -> NamedScreenshots -> Spec
validateScreenshots actual expected = do
    const () <$>
        mergeA 
            (traverseMissing failOnUnpairedScreenshots)
            (traverseMissing failOnUnpairedScreenshots)
            (zipWithAMatched screenshotsAreSimilar)
            actual
            expected
