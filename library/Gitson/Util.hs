-- | Various functions used inside Gitson.
module Gitson.Util (module Gitson.Util) where

import           Data.Char (isSpace)
import           Control.Monad (void)
import           System.FilePath
import           System.Directory
import           System.Process (readProcess)
import           System.Cmd (rawSystem)

-- | Combines two paths and adds the .json extension.
makePath :: FilePath -> String -> FilePath
makePath collPath key = collPath </> key <.> "json"

-- | Turns a list of filenames into a list of keys.
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
stripWhitespaceRight :: FilePath -> FilePath
stripWhitespaceRight = reverse . dropWhile isSpace . reverse

-- | Finds the path to the git repository a given path belongs to.
findRepoRoot :: FilePath -> IO FilePath
findRepoRoot path = (insideDirectory path $ readProcess "git" ["rev-parse", "--show-toplevel"] []) >>= return . stripWhitespaceRight

-- | Returns the message of the last git commit in the repo where the current directory is located.
lastCommitText :: IO String
lastCommitText = readProcess "git" ["log", "--max-count=1", "--pretty=format:%s"] []

-- | Runs a shell command.
shell :: String -> [String] -> IO ()
shell cmd args = void $ rawSystem cmd args

-- | Returns a lock file path.
lockPath :: FilePath -> IO FilePath
lockPath path = findRepoRoot path >>= return . (</> ".git" </> "gitson-lock") -- i can haz readability
