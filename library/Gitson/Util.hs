-- | Various functions used inside Gitson.
module Gitson.Util (module Gitson.Util) where

import           Control.Monad (void, filterM)
import           Control.Applicative
import           Data.List (isSuffixOf, isPrefixOf)
import           Data.Maybe
import           System.FilePath
import           System.Directory
import           System.Process
import           System.IO

-- | Combines two paths and adds the .json extension.
--
-- >>> entryPath "things" "entry"
-- "things/entry.json"
--
-- >>> entryPath "things/" "entry"
-- "things/entry.json"
entryPath :: FilePath -> FilePath -> FilePath
entryPath collection key = collection </> key <.> "json"

-- | Path to the transaction lock file, relative to the repo root.
lockPath :: FilePath
lockPath = ".git" </> "gitson-lock"

-- | Turns a list of filenames into a list of keys, ignoring non-JSON files.
--
-- >>> filterFilenamesAsKeys [".", "..", "k1.json", "unrelated.file"]
-- ["k1"]
filterFilenamesAsKeys :: [String] -> [String]
filterFilenamesAsKeys = map dropExtension . filter (isSuffixOf ".json")

-- | Filters a list of file paths, leaving only paths to existing non-hidden directories.
filterDirs :: [FilePath] -> IO [FilePath]
filterDirs = (filterM doesDirectoryExist) . filter (not . isPrefixOf ".")

-- | Returns an IO action that switches the current directory to a given path,
-- executes the given IO action and switches the current directory back.
insideDirectory :: FilePath -> IO a -> IO a
insideDirectory path action = do
  prevPath <- getCurrentDirectory
  setCurrentDirectory path
  result <- action
  setCurrentDirectory prevPath
  return result

-- | Returns the message of the last git commit in the repo where the current directory is located.
lastCommitText :: IO String
lastCommitText = readProcess "git" ["log", "--max-count=1", "--pretty=format:%s"] []

-- | A \/dev/null handle.
devNull :: IO Handle
devNull = openFile "/dev/null" ReadWriteMode

-- | Runs a shell command with stdin, stdout and stderr set to /dev/null.
shell :: String -> [String] -> IO ()
shell cmd args = void $ do
  dnull <- devNull
  (_, _, _, pid) <- createProcess (proc cmd args){std_in = UseHandle dnull, std_out = UseHandle dnull, std_err = UseHandle dnull}
  waitForProcess pid

-- I can't believe there's nothing like this in Data.Maybe
-- | Adds a value to a Maybe.
intoMaybe :: Maybe a -> b -> Maybe (a, b)
intoMaybe (Just x) y = Just (x, y)
intoMaybe Nothing y = Nothing

-- | Tries to extract the first int out of a string.
--
-- >>> maybeReadIntString "0123-hell0w0rld"
-- Just (123,"-hell0w0rld")
--
-- >>> maybeReadIntString "1"
-- Just (1,"")
--
-- >>> maybeReadIntString "hello"
-- Nothing
maybeReadIntString :: String -> Maybe (Int, String)
maybeReadIntString x = listToMaybe (reads x :: [(Int, String)])

-- | Returns the next numeric id in a sequence of keys.
--
-- >>> nextKeyId []
-- 1
--
-- >>> nextKeyId ["aaaaarrrrrrrrrrr"]
-- 1
--
-- >>> nextKeyId ["1-hell0-w0rld-123456.json", "002-my-second-post.json"]
-- 3
nextKeyId :: [FilePath] -> Int
nextKeyId = (+1) . maxOrZero . catMaybes . map maybeReadInt
  where maybeReadInt x = fst <$> maybeReadIntString x
        maxOrZero [] = 0
        maxOrZero xs = maximum xs
