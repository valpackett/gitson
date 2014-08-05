-- | Gitson is a simple document store library for Git + JSON.
module Gitson (module Gitson) where

import           Prelude hiding (catch)
import           System.Directory
import           System.FilePath
import           System.Lock.FLock
import           Control.Exception
import           Control.Applicative
import           Control.Error.Util
import           Data.Aeson (ToJSON, encode, FromJSON, decode)
import qualified Data.ByteString.Lazy as BL
import           Gitson.Util

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  shell "git" ["init", path]
  lock <- lockPath path
  writeFile lock ""

-- | Writes an entry to a collection under a given key and commits the change to git.
-- Uses flock for thread safety.
saveToCollection :: ToJSON a => FilePath -> String -> a -> IO ()
saveToCollection collPath key content = do
  createDirectoryIfMissing True collPath
  lock <- lockPath collPath
  withLock lock Exclusive Block $ do
    let filePath = entryPath collPath key
    BL.writeFile filePath (encode content)
    repoRoot <- findRepoRoot collPath
    repoRelPath <- makeRelativeToCurrentDirectory repoRoot
    insideDirectory repoRoot $ do
      let collName = makeRelative repoRelPath collPath
      let fileName = makeRelative repoRelPath filePath
      shell "git" ["add", fileName]
      shell "git" ["commit", "-m", "Update '" ++ key ++ "' in collection '" ++ collName ++ "'"]

-- | Reads an entry from a collection by key.
readFromCollection :: FromJSON a => FilePath -> String -> IO (Maybe a)
readFromCollection collPath key = do
  jsonString <- try (BL.readFile $ entryPath collPath key) :: IO (Either IOException BL.ByteString)
  return $ decode =<< hush jsonString

-- | Lists entry keys in a collection.
listCollection :: FilePath -> IO (Maybe [String])
listCollection collPath = do
  contents <- try (getDirectoryContents collPath) :: IO (Either IOException [FilePath])
  return $ filterFilenamesAsKeys <$> hush contents
