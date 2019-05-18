module Site.PostLengthTest (postLengthTests) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Site.PostLength (minutesToReadPost, totalWords)

postLengthTests :: Spec
postLengthTests = do
    describe "Site.PostLength" $ do
        describe "totalWords" $ do
            it "should count the number of words in sibling <p> elements" $
                totalWords "<body><p>One Two</p><p>Three</p></body>" `shouldBe` 3

