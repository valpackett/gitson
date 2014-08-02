{-# LANGUAGE TemplateHaskell #-}

module GitsonSpec (spec) where

import           Test.Hspec
import           System.Directory
import           System.Process (readProcess)
import           Data.Aeson.TH
import           Gitson

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

spec :: Spec
spec = before setup $ after cleanup $ describe "gitson" $ do
  it "forms paths" $ do
    makePath "things" "entry" `shouldBe` "things/entry.json"
    makePath "things/" "entry" `shouldBe` "things/entry.json"

  it "saves entries" $ do
    saveToCollection "tmp/repo/things" "first-thing" Thing {val = 1}
    stored <- readFile "tmp/repo/things/first-thing.json"
    stored `shouldBe` "{\"val\":1}"
    insideDirectory "tmp/repo" $ do
      commitMsg <- readProcess "git" ["log", "--max-count=1", "--pretty=format:%s"] []
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
