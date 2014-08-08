{-# LANGUAGE TemplateHaskell #-}

module GitsonSpec (spec) where

import           Test.Hspec
import           System.Directory
import           Data.Aeson.TH
import           Data.List (sort)
import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad (void)
import           Gitson
import           Gitson.Util (insideDirectory, lastCommitText)

data Thing = Thing { val :: Int } deriving (Eq, Show, Ord)
$(deriveJSON defaultOptions ''Thing)

spec :: Spec
spec = before setup $ after cleanup $ do
  context "transaction" $ do
    describe "saveEntry" $ do
      it "saves entries only after it's done" $ do
        transaction "tmp/repo" $ do
          saveEntry "things" "first-thing" Thing {val = 1}
          saveEntry "things" "second-thing" Thing {val = 2}
          liftIO $ (readFile "things/first-thing.json") `shouldThrow` anyIOException
          liftIO $ (readFile "things/second-thing.json") `shouldThrow` anyIOException
        insideDirectory "tmp/repo" $ do
          first <- readFile "things/first-thing.json"
          first `shouldBe` "{\n  \"val\": 1\n}"
          second <- readFile "things/second-thing.json"
          second `shouldBe` "{\n  \"val\": 2\n}"
          commitMsg <- lastCommitText
          commitMsg `shouldBe` "Gitson transaction"

    describe "saveNextEntry" $ do
      it "saves entries with next numeric ids" $ do
        transaction "tmp/repo" $ do
          saveNextEntry "things" "hello" Thing {val = 1}
          saveNextEntry "things" "world" Thing {val = 2}
        insideDirectory "tmp/repo" $ do
          first <- readFile "things/000001-hello.json"
          first `shouldBe` "{\n  \"val\": 1\n}"
          second <- readFile "things/000002-world.json"
          second `shouldBe` "{\n  \"val\": 2\n}"

  describe "readEntry" $ do
    it "returns Just the entry when reading an entry by key" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
      content <- readEntry "tmp/repo/things" "second-thing" :: IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 2}

    it "returns Nothing when reading by a nonexistent key" $ do
      content <- readEntry "tmp/repo/things" "totally-not-a-thing" :: IO (Maybe Thing)
      content `shouldBe` Nothing

  describe "readEntryById" $ do
    it "returns Just the entry when reading an entry by id" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/000004-second-thing.json" "{\"val\":1}"
      content <- readEntryById "tmp/repo/things" 4 :: IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 1}

    it "returns Nothing when reading by a nonexistent id" $ do
      content <- readEntryById "tmp/repo/things" 1 :: IO (Maybe Thing)
      content `shouldBe` Nothing

  describe "readEntryByName" $ do
    it "returns Just the entry when reading an entry by name" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/000098-second-thing.json" "{\"val\":1}"
      content <- readEntryByName "tmp/repo/things" "second-thing" :: IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 1}

    it "returns Nothing when reading by a nonexistent name" $ do
      content <- readEntryByName "tmp/repo/things" "yolo" :: IO (Maybe Thing)
      content `shouldBe` Nothing

  describe "listEntryKeys" $ do
    it "returns a list of entry keys when listing a collection" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/first-thing.json" "{}"
      _ <- writeFile "tmp/repo/things/second-thing.json" "{}"
      list <- listEntryKeys "tmp/repo/things"
      sort list `shouldBe` ["first-thing", "second-thing"]

    it "returns an empty when listing a nonexistent collection" $ do
      list <- listEntryKeys "nonsense"
      list `shouldBe` []

  describe "listEntries" $ do
    it "returns a list of entries when listing a collection" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/first-thing.json" "{\"val\":1}"
      _ <- writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
      list <- listEntries "tmp/repo/things" :: IO [Thing]
      sort list `shouldBe` [Thing {val = 1}, Thing {val = 2}]

    it "returns an empty list when listing a nonexistent collection" $ do
      list <- listEntries "nonsense" :: IO [Thing]
      list `shouldBe` []

  describe "listCollections" $ do
    it "returns a list of collections" $ do
      createDirectoryIfMissing True "tmp/repo/bits"
      createDirectoryIfMissing True "tmp/repo/pieces"
      insideDirectory "tmp/repo" $ do
        list <- listCollections
        sort list `shouldBe` ["bits", "pieces"]

    it "returns an empty list when listing an empty repo" $ do
      createDirectoryIfMissing True "tmp/repo"
      insideDirectory "tmp/repo" $ do
        list <- listCollections
        sort list `shouldBe` []

setup :: IO ()
setup = createRepo "tmp/repo"

cleanup :: IO ()
cleanup = void (try (removeDirectoryRecursive "tmp/repo") :: IO (Either IOException ()))
