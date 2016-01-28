module OutOfTheYards.Content.Clean.Tests where

import Text.XML.Light 
    ( Content
    , useShortEmptyTags, useExtraWhiteSpace
    , defaultConfigPP, ppcContent
    )
import Test.Hspec (Spec, describe, it, shouldBe)

import OutOfTheYards.Content.Clean 

tests :: Spec
tests = describe "OutOfTheYards.Content.Clean" $ do
    it "should act as the identity when there are no predicates" $
        let exampleHtml = "<body><div>hi</div></body>"
        in cleanWith showContent [] exampleHtml `shouldBe` exampleHtml

    it "should act as the identity when there are no comments" $
        let exampleHtml = "<body><div>hi</div></body>"
        in cleanWith showContent [isComment] exampleHtml `shouldBe` exampleHtml

    it "should remove comments" $
        let cleanedHtml = cleanWith showContent [isComment] $
                "<body><div id=\"cmnt5\"></div></body>"
        in cleanedHtml `shouldBe` "<body></body>"

    it "should remove nested comments" $
        let cleanedHtml = cleanWith showContent [isComment] $
                "<body><p><p id=\"cmnt5\"></p></p></body>"
        in cleanedHtml `shouldBe` "<body><p></p></body>"

    it "should recognize a footnote class name" $
        let cleanedHtml = cleanWith showContent [isFootnote] $
                "<body><div id=\"ftnt10\"></div></body>"
        in cleanedHtml `shouldBe` "<body></body>"

    it "should remove the grandparent of a footnote" $
        let cleanedHtml = cleanWith showContent [isFootnote `atDepth` 2] $
                "<body><div><p><span id=\"ftnt10\"></span></p></div></body>"
        in cleanedHtml `shouldBe` "<body></body>"

showContent :: Content -> String
showContent = ppcContent 
    . useShortEmptyTags (const False) 
    . useExtraWhiteSpace False 
    $ defaultConfigPP
