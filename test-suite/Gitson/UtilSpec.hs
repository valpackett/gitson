module Gitson.UtilSpec (spec) where

import           Test.Hspec
import           Gitson.Util

spec :: Spec
spec = describe "util" $ do
  it "forms paths" $ do
    entryPath "things" "entry" `shouldBe` "things/entry.json"
    entryPath "things/" "entry" `shouldBe` "things/entry.json"

  it "filters filenames as keys" $ do
    filterFilenamesAsKeys [".", "..", "k1.json"] `shouldBe` ["k1"]
