{-# OPTIONS_GHC -fno-warn-dodgy-exports -fno-warn-unused-imports #-}

-- | Gitson is a simple document store library for Git + JSON.
module Gitson (module Gitson) where

import           Prelude hiding (catch)
import           System.FilePath
import           System.Directory
import           System.Cmd (rawSystem)
import           System.Process (readProcess)
import           System.IO.Error hiding (catch)
import           Control.Exception
import           Data.Char (isSpace)
import           Data.Aeson (ToJSON, encode, FromJSON, decode)
import qualified Data.ByteString.Lazy as BL

makePath :: FilePath -> String -> FilePath
makePath collPath key = collPath </> key <.> "json"

insideDirectory :: FilePath -> IO a -> IO a
insideDirectory path action = do
  prevPath <- getCurrentDirectory
  setCurrentDirectory path
  result <- action
  setCurrentDirectory prevPath
  return result

stripWhitespaceRight :: FilePath -> FilePath
stripWhitespaceRight = reverse . dropWhile isSpace . reverse

findRepoRoot :: FilePath -> IO FilePath
findRepoRoot path = (insideDirectory path $ readProcess "git" ["rev-parse", "--show-toplevel"] []) >>= return . stripWhitespaceRight

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  _ <- rawSystem "git" ["init", path]
  return ()

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
    _ <- rawSystem "git" ["add", fileName]
    _ <- rawSystem "git" ["commit", "-m", "Update '" ++ key ++ "' in collection '" ++ collName ++ "'"]
    return ()

-- | Reads an entry from a collection by key.
readFromCollection :: FromJSON a => FilePath -> String -> IO (Maybe a)
readFromCollection collPath key = do
  jsonString <- try (BL.readFile $ makePath collPath key) :: IO (Either IOException BL.ByteString)
  case jsonString of
    Left e -> return Nothing
    Right val -> return $ decode val
