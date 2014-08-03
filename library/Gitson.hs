{-# OPTIONS_GHC -fno-warn-dodgy-exports -fwarn-unused-imports #-}

-- | Gitson is a simple document store library for Git + JSON.
module Gitson (module Gitson) where

import           Prelude hiding (catch)
import           System.Directory
import           System.FilePath
import           Control.Exception
import           Data.Aeson (ToJSON, encode, FromJSON, decode)
import qualified Data.ByteString.Lazy as BL
import           Gitson.Util

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  shell "git" ["init", path]

-- | Writes an entry to a collection under a given key and commits the change to git.
saveToCollection :: ToJSON a => FilePath -> String -> a -> IO ()
saveToCollection collPath key content = do
  createDirectoryIfMissing True collPath
  let filePath = makePath collPath key
  BL.writeFile filePath (encode content)
  repoRoot <- findRepoRoot collPath
  repoRelPath <- makeRelativeToCurrentDirectory repoRoot
  insideDirectory repoRoot $ do
    let collName = makeRelative repoRelPath collPath
    let fileName = makeRelative repoRelPath filePath
    shell "git" ["add", fileName]
    shell "git" ["commit", "-m", "Update '" ++ key ++ "' in collection '" ++ collName ++ "'"]
    return ()

-- | Reads an entry from a collection by key.
readFromCollection :: FromJSON a => FilePath -> String -> IO (Maybe a)
readFromCollection collPath key = do
  jsonString <- try (BL.readFile $ makePath collPath key) :: IO (Either IOException BL.ByteString)
  case jsonString of
    Left e -> return Nothing
    Right val -> return $ decode val

-- | Lists entry keys in a collection.
listCollection :: FilePath -> IO (Maybe [String])
listCollection collPath = do
  contents <- try (getDirectoryContents collPath) :: IO (Either IOException [FilePath])
  case contents of
    Left e -> return Nothing
    Right val -> return $ Just $ filterFilenamesAsKeys val
