{-# LANGUAGE TemplateHaskell #-}

module GitsonSpec (spec) where

import           Test.Hspec
import           System.Directory
import           Data.Aeson.TH
import           Data.List (sort)
import           Control.Applicative
import           Control.Monad.IO.Class
import           Gitson
import           Gitson.Util (insideDirectory, lastCommitText)
import           System.Process

data Thing = Thing { val :: Int } deriving (Eq, Show)
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
        ls <- readProcess "ls" ["-la"] []
        putStrLn ls
        lsTh <- readProcess "ls" ["-la", "things"] []
        putStrLn lsTh
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

  describe "listEntries" $ do
    it "returns Just a list of entries when listing a collection" $ do
      createDirectoryIfMissing True "tmp/repo/things"
      _ <- writeFile "tmp/repo/things/first-thing.json" "{}"
      _ <- writeFile "tmp/repo/things/second-thing.json" "{}"
      list <- listEntries "tmp/repo/things"
      sort <$> list `shouldBe` Just ["first-thing", "second-thing"]

    it "returns Nothing when listing a nonexistent collection" $ do
      list <- listEntries "nonsense"
      list `shouldBe` Nothing

setup :: IO ()
setup = createRepo "tmp/repo"

cleanup :: IO ()
cleanup = removeDirectoryRecursive "tmp/repo"
