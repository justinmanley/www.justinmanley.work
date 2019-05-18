module Site.UrlTest (urlTests) where

import Test.Hspec (Spec, describe, it, shouldBe)

import Data.Monoid (mempty)
import Hakyll (Item(..), fromFilePath)
import Site.Url (normalizeUrlsWithRoute)

urlTests :: Spec
urlTests = do
    describe "Site.Url" $ do
        describe "normalizeUrlsWithRoute" $ do
            it "should make src attributes of <img> tags absolute paths" $
                let item = Item
                        { itemIdentifier = fromFilePath "example.html"
                        , itemBody = "<body><img src=\"a/relative/path\"/></body>"
                        }
                    route = (Just "site_root/route.html")
                    normalizedBody = itemBody $ normalizeUrlsWithRoute route item
                    expectedNormalizedBody = "<body><img src=\"/site_root/a/relative/path\" /></body>"
                in normalizedBody `shouldBe` expectedNormalizedBody

            it "should remove /index.html suffix from href attributes of links" $
                let item = Item
                        { itemIdentifier = fromFilePath "example.html"
                        , itemBody = "<body><a href=\"/hello/index.html\">Hello</a></body>"
                        }
                    route = (Just "site_root/route.html")
                    normalizedBody = itemBody $ normalizeUrlsWithRoute route item
                    expectedNormalizedBody = "<body><a href=\"/hello\">Hello</a></body>"
                in normalizedBody `shouldBe` expectedNormalizedBody

            it "should leave href attributes without /index.html suffix untouched" $
                let body = "<body><a href=\"http://example.com/page.html\">Hello</a></body>"
                    item = Item
                        { itemIdentifier = fromFilePath "example.html"
                        , itemBody = body
                        }
                    route = (Just "site_root/route.html")
                    normalizedBody = itemBody $ normalizeUrlsWithRoute route item
                in normalizedBody `shouldBe` body 

                

