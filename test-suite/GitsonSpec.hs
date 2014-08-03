{-# LANGUAGE TemplateHaskell #-}

module GitsonSpec (spec) where

import           Test.Hspec
import           System.Directory
import           Data.Aeson.TH
import           Gitson
import           Gitson.Util (insideDirectory, lastCommitText)

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

spec :: Spec
spec = before setup $ after cleanup $ describe "gitson" $ do
  it "saves entries" $ do
    saveToCollection "tmp/repo/things" "first-thing" Thing {val = 1}
    stored <- readFile "tmp/repo/things/first-thing.json"
    stored `shouldBe` "{\"val\":1}"
    insideDirectory "tmp/repo" $ do
      commitMsg <- lastCommitText
      commitMsg `shouldBe` "Update 'first-thing' in collection 'things'"

  it "reads entries" $ do
    createDirectoryIfMissing True "tmp/repo/things"
    _ <- writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
    content <- readFromCollection "tmp/repo/things" "second-thing" :: IO (Maybe Thing)
    content `shouldBe` Just Thing {val = 2}

  it "returns Nothing when reading by a nonexistent key" $ do
    content <- readFromCollection "tmp/repo/things" "totally-not-a-thing" :: IO (Maybe Thing)
    content `shouldBe` Nothing

  it "lists entries" $ do
    createDirectoryIfMissing True "tmp/repo/things"
    _ <- writeFile "tmp/repo/things/first-thing.json" "{}"
    _ <- writeFile "tmp/repo/things/second-thing.json" "{}"
    list <- listCollection "tmp/repo/things"
    list `shouldBe` Just ["first-thing", "second-thing"]

  it "returns Nothing when listing a nonexistent collection" $ do
    list <- listCollection "nonsense"
    list `shouldBe` Nothing

setup :: IO ()
setup = createRepo "tmp/repo"

cleanup :: IO ()
cleanup = removeDirectoryRecursive "tmp/repo"
