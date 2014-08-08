-- | Gitson is a simple document store library for Git + JSON.
module Gitson (
  TransactionWriter,
  createRepo,
  transaction,
  saveEntry,
  saveNextEntry,
  listCollections,
  listEntryKeys,
  listEntries,
  readEntry,
  readEntryById,
  readEntryByName
) where

import           System.Directory
import           System.Lock.FLock
import           Control.Exception
import           Control.Error.Util
import           Control.Monad.Trans.Writer
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (ToJSON, FromJSON, decode)
import           Data.Aeson.Encode.Pretty
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.List (find, isSuffixOf)
import           Text.Printf (printf)
import qualified Data.ByteString.Lazy as BL
import           Gitson.Util

-- | A transaction monad.
type TransactionWriter = WriterT [IO ()] IO ()

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  insideDirectory path $ do
    shell "git" ["init"]
    writeFile lockPath ""

-- | Executes a blocking transaction on a repository, committing the results to git.
transaction :: FilePath -> TransactionWriter -> IO ()
transaction repoPath action = do
  insideDirectory repoPath $ withLock lockPath Exclusive Block $ do
    shell "git" ["reset", "--hard", "HEAD"] -- Reset working tree just in case
    writeActions <- execWriterT action
    sequence_ writeActions
    shell "git" ["add", "--all"]
    shell "git" ["commit", "-m", "Gitson transaction"]

combineKey :: (Int, String) -> String
combineKey (n, s) = zeroPad n ++ "-" ++ s
  where zeroPad :: Int -> String
        zeroPad x = printf "%06d" x

prettyConfig :: Config
prettyConfig = Config { confIndent = 2, confCompare = compare }

writeEntry :: ToJSON a => FilePath -> FilePath -> a -> IO ()
writeEntry collection key content = BL.writeFile (entryPath collection key) (encodePretty' prettyConfig content)

-- | Adds a write action to a transaction.
saveEntry :: ToJSON a => FilePath -> FilePath -> a -> TransactionWriter
saveEntry collection key content = do
  tell [createDirectoryIfMissing True collection,
        writeEntry collection key content]

-- | Adds a write action to a transaction.
-- The key will start with a numeric id, incremented from the last id in the collection.
saveNextEntry :: ToJSON a => FilePath -> FilePath -> a -> TransactionWriter
saveNextEntry collection key content = do
  tell [createDirectoryIfMissing True collection,
        listEntryKeys collection >>=
        return . nextKeyId >>=
        \nextId -> writeEntry collection (combineKey (nextId, key)) content]

-- | Lists collections in the current repository.
listCollections :: IO [FilePath]
listCollections = do
  contents <- try (getDirectoryContents =<< getCurrentDirectory) :: IO (Either IOException [FilePath])
  filterDirs $ fromMaybe [] $ hush contents

-- | Lists entry keys in a collection.
listEntryKeys :: FilePath -> IO [String]
listEntryKeys collection = do
  contents <- try (getDirectoryContents collection) :: IO (Either IOException [String])
  return $ filterFilenamesAsKeys $ fromMaybe [] $ hush contents

-- | Lists entries in a collection.
listEntries :: FromJSON a => FilePath -> IO [a]
listEntries collection = do
  ks <- listEntryKeys collection
  maybes <- mapM (readEntry collection) ks
  return $ fromMaybe [] $ sequence maybes

-- | Reads an entry from a collection by key.
readEntry :: FromJSON a => FilePath -> String -> IO (Maybe a)
readEntry collection key = do
  jsonString <- try (BL.readFile $ entryPath collection key) :: IO (Either IOException BL.ByteString)
  return $ decode =<< hush jsonString

splitFindAndReadEntry :: FromJSON a => FilePath -> ([((Int, String), String)] -> Maybe ((Int, String), String)) -> IO (Maybe a)
splitFindAndReadEntry collection finder = listEntryKeys collection >>=
  maybeReadEntry . finder . catMaybes . (map $ \x -> intoMaybe (maybeReadIntString x) x)
  where maybeReadEntry (Just x) = readEntry collection $ snd x
        maybeReadEntry Nothing = return Nothing

-- | Reads an entry from a collection by numeric id (for example, key "00001-hello" has id 1)..
readEntryById :: FromJSON a => FilePath -> Int -> IO (Maybe a)
readEntryById collection n = splitFindAndReadEntry collection $ find ((== n) . fst . fst)

-- | Reads an entry from a collection by name (for example, key "00001-hello" has name "hello").
readEntryByName :: FromJSON a => FilePath -> String -> IO (Maybe a)
readEntryByName collection n = splitFindAndReadEntry collection $ find ((isSuffixOf n) . snd . fst)
