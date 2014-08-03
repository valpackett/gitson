module Gitson.UtilSpec (spec) where

import           Test.Hspec
import           Gitson.Util

spec :: Spec
spec = describe "util" $ do
  it "forms paths" $ do
    makePath "things" "entry" `shouldBe` "things/entry.json"
    makePath "things/" "entry" `shouldBe` "things/entry.json"

  it "filters filenames as keys" $ do
    filterFilenamesAsKeys [".", "..", "k1.json"] `shouldBe` ["k1"]
