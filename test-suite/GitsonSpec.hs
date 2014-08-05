{-# LANGUAGE TemplateHaskell #-}

module GitsonSpec (spec) where

import           Test.Hspec
import           System.Directory
import           Data.Aeson.TH
import           Control.Monad.IO.Class
import           Gitson
import           Gitson.Util (insideDirectory, lastCommitText)

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

spec :: Spec
spec = before setup $ after cleanup $ describe "gitson" $ do
  it "saves entries in transactions" $ do
    transaction "tmp/repo" $ do
      saveEntry Thing {val = 1} "first-thing" "things"
      saveEntry Thing {val = 2} "second-thing" "things"
      liftIO $ (readFile "tmp/repo/things/first-thing.json") `shouldThrow` anyIOException
      liftIO $ (readFile "tmp/repo/things/second-thing.json") `shouldThrow` anyIOException
    insideDirectory "tmp/repo" $ do
      first <- readFile "things/first-thing.json"
      first `shouldBe` "{\"val\":1}"
      second <- readFile "things/second-thing.json"
      second `shouldBe` "{\"val\":2}"
      commitMsg <- lastCommitText
      commitMsg `shouldBe` "Gitson transaction"

  it "reads entries" $ do
    createDirectoryIfMissing True "tmp/repo/things"
    _ <- writeFile "tmp/repo/things/second-thing.json" "{\"val\":2}"
    content <- readEntry "second-thing" "tmp/repo/things" :: IO (Maybe Thing)
    content `shouldBe` Just Thing {val = 2}

  it "returns Nothing when reading by a nonexistent key" $ do
    content <- readEntry "totally-not-a-thing" "tmp/repo/things" :: IO (Maybe Thing)
    content `shouldBe` Nothing

  it "lists entries" $ do
    createDirectoryIfMissing True "tmp/repo/things"
    _ <- writeFile "tmp/repo/things/first-thing.json" "{}"
    _ <- writeFile "tmp/repo/things/second-thing.json" "{}"
    list <- listEntries "tmp/repo/things"
    list `shouldBe` Just ["first-thing", "second-thing"]

  it "returns Nothing when listing a nonexistent collection" $ do
    list <- listEntries "nonsense"
    list `shouldBe` Nothing

setup :: IO ()
setup = createRepo "tmp/repo"

cleanup :: IO ()
cleanup = removeDirectoryRecursive "tmp/repo"
