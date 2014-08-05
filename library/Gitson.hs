-- | Gitson is a simple document store library for Git + JSON.
module Gitson (module Gitson) where

import           System.Directory
import           System.Lock.FLock
import           Control.Exception
import           Control.Applicative
import           Control.Error.Util
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON, encode, FromJSON, decode)
import qualified Data.ByteString.Lazy as BL
import           Gitson.Util

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  insideDirectory path $ do
    shell "git" ["init"]
    writeFile lockPath ""

-- | A transaction monad.
type TransactionWriter = WriterT [IO ()] IO ()

-- | Adds a write action to a transaction.
saveEntry :: ToJSON a => FilePath -> FilePath -> a -> TransactionWriter
saveEntry collection key content = do
  liftIO $ createDirectoryIfMissing True collection
  tell [BL.writeFile (entryPath collection key) (encode content)]

-- | Executes a blocking transaction on a repository, committing the results to git.
transaction :: FilePath -> TransactionWriter -> IO ()
transaction repoPath action = do
  insideDirectory repoPath $ withLock lockPath Exclusive Block $ do
    shell "git" ["reset", "--hard", "HEAD"] -- Reset working tree just in case
    writeActions <- execWriterT action
    sequence_ writeActions
    shell "git" ["add", "--all"]
    shell "git" ["commit", "-m", "Gitson transaction"]

-- | Reads an entry from a collection by key.
readEntry :: FromJSON a => FilePath -> FilePath -> IO (Maybe a)
readEntry collection key = do
  jsonString <- try (BL.readFile $ entryPath collection key) :: IO (Either IOException BL.ByteString)
  return $ decode =<< hush jsonString

-- | Lists entry keys in a collection.
listEntries :: FilePath -> IO (Maybe [FilePath])
listEntries collection = do
  contents <- try (getDirectoryContents collection) :: IO (Either IOException [FilePath])
  return $ filterFilenamesAsKeys <$> hush contents
