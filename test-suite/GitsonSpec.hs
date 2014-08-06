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
  describe "transaction" $ do
    it "saves entries only after it's done" $ do
      transaction "tmp/repo" $ do
        saveEntry "things" "first-thing" Thing {val = 1}
        saveEntry "things" "second-thing" Thing {val = 2}
        liftIO $ (readFile "things/first-thing.json") `shouldThrow` anyIOException
        liftIO $ (readFile "things/second-thing.json") `shouldThrow` anyIOException
      insideDirectory "tmp/repo" $ do
        first <- readFile "things/first-thing.json"
        first `shouldBe` "{\"val\":1}"
        second <- readFile "things/second-thing.json"
        second `shouldBe` "{\"val\":2}"
        commitMsg <- lastCommitText
        commitMsg `shouldBe` "Gitson transaction"

  describe "readEntry" $ do
    it "returns Just the entry when reading an entry by key" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
      content <- readEntry "tmp/repo/things" "second-thing" :: IO (Maybe Thing)
      content `shouldBe` Just Thing {val = 2}

    it "returns Nothing when reading by a nonexistent key" $ do
      content <- readEntry "tmp/repo/things" "totally-not-a-thing" :: IO (Maybe Thing)
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
      list <- listEntries "tmp/repo/things" :: IO ([Thing])
      sort list `shouldBe` [Thing {val = 1}, Thing {val = 2}]

    it "returns an empty list when listing a nonexistent collection" $ do
      list <- listEntries "nonsense" :: IO ([Thing])
      list `shouldBe` []


setup :: IO ()
setup = createRepo "tmp/repo"

cleanup :: IO ()
cleanup = void (try (removeDirectoryRecursive "tmp/repo") :: IO (Either IOException ()))
