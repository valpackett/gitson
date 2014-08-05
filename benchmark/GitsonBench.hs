{-# LANGUAGE TemplateHaskell #-}

module GitsonBench (setup, benchmarks, cleanup) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (void)
import           System.Directory
import           System.Random
import           Data.Aeson.TH
import           Criterion
import           Gitson

data Thing = Thing { val :: Int } deriving (Eq, Show)
$(deriveJSON defaultOptions ''Thing)

benchRepoPath :: String
benchRepoPath = "tmp/bench-repo"

setup :: IO ()
setup = createRepo benchRepoPath >> void (transaction benchRepoPath $ do
  saveEntry "things" "thing-to-read" Thing {val = 1})

transactOneWrite :: IO ()
transactOneWrite = transaction benchRepoPath $ do
  rnum <- liftIO $ getStdRandom $ randomR (1,100)
  saveEntry "things" "first-thing" Thing {val = rnum}

readThing :: IO ()
readThing = void (readEntry "things" "thing-to-read" :: IO (Maybe Thing))

benchmarks :: [Benchmark]
benchmarks = [
    bench "transaction with 1 write" $ nfIO transactOneWrite
  , bench "1 read" $ nfIO $ readThing ]

cleanup :: IO ()
cleanup = removeDirectoryRecursive benchRepoPath
