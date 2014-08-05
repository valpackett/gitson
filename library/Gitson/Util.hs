-- | Various functions used inside Gitson.
module Gitson.Util (module Gitson.Util) where

import           Data.Char (isSpace)
import           Control.Monad (void)
import           Control.Applicative
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

-- | Turns a list of filenames into a list of keys.
--
-- >>> filterFilenamesAsKeys [".", "..", "k1.json"]
-- ["k1"]
filterFilenamesAsKeys :: [String] -> [String]
filterFilenamesAsKeys = map dropExtension . filter (`notElem` [".", ".."])

-- | Returns an IO action that switches the current directory to a given path,
-- executes the given IO action and switches the current directory back.
insideDirectory :: FilePath -> IO a -> IO a
insideDirectory path action = do
  prevPath <- getCurrentDirectory
  setCurrentDirectory path
  result <- action
  setCurrentDirectory prevPath
  return result

-- | Removes trailing whitespace like the newline you get from executing commands.
--
-- >>> stripWhitespaceRight "/path/to/thingy \n\n\n"
-- "/path/to/thingy"
stripWhitespaceRight :: FilePath -> FilePath
stripWhitespaceRight = reverse . dropWhile isSpace . reverse

-- | Finds the path to the git repository a given path belongs to.
findRepoRoot :: FilePath -> IO FilePath
findRepoRoot path = stripWhitespaceRight <$> (insideDirectory path $ readProcess "git" ["rev-parse", "--show-toplevel"] [])

-- | Returns the message of the last git commit in the repo where the current directory is located.
lastCommitText :: IO String
lastCommitText = readProcess "git" ["log", "--max-count=1", "--pretty=format:%s"] []

-- | A \/dev/null handle.
devNull :: IO Handle
devNull = openFile "/dev/null" ReadWriteMode

-- | Runs a shell command with stdin, stdout and stderr set to /dev/null.
shell :: String -> [String] -> IO ()
shell cmd args = void $ do
  null <- devNull
  (_, _, _, pid) <- createProcess (proc cmd args){std_in = UseHandle null, std_out = UseHandle null, std_err = UseHandle null}
  waitForProcess pid
