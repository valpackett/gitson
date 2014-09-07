{-# LANGUAGE Safe #-}

-- | Gitson is a simple document store library for Git + JSON.
module Gitson (
  TransactionWriter
, createRepo
, transaction
, saveDocument
, saveNextDocument
, listCollections
, listDocumentKeys
, listEntries
, readDocument
, readDocumentById
, readDocumentByName
, documentIdFromName
, documentNameFromId
) where

import           System.Directory
import           System.Lock.FLock
import           Control.Applicative ((<$>))
import           Control.Exception (try, IOException)
import           Control.Error.Util (hush)
import           Control.Monad.Trans.Writer
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.List (find, isSuffixOf)
import           Text.Printf (printf)
import qualified Data.ByteString.Lazy as BL
import           Gitson.Util
import           Gitson.Json

-- | A transaction monad.
type TransactionWriter = WriterT [IO ()] IO ()

type IdAndName = (Int, String)
type FileName = String
type Finder = ([(IdAndName, FileName)] -> Maybe (IdAndName, FileName))

-- | Creates a git repository under a given path.
createRepo :: FilePath -> IO ()
createRepo path = do
  createDirectoryIfMissing True path
  insideDirectory path $ shell "git" ["init"]

-- | Executes a blocking transaction on a repository, committing the results to git.
transaction :: FilePath -> TransactionWriter -> IO ()
transaction repoPath action = do
  insideDirectory repoPath $ do
    writeFile lockPath ""
    withLock lockPath Exclusive Block $ do
      writeActions <- execWriterT action
      shell "git" ["stash"] -- it's totally ok to do this without changes
      sequence_ writeActions
      shell "git" ["add", "--all"]
      shell "git" ["commit", "-m", "Gitson transaction"]
      shell "git" ["stash", "pop"]

combineKey :: IdAndName -> FileName
combineKey (n, s) = zeroPad n ++ "-" ++ s
  where zeroPad :: Int -> String
        zeroPad x = printf "%06d" x

writeDocument :: ToJSON a => FilePath -> FileName -> a -> IO ()
writeDocument collection key content = BL.writeFile (documentPath collection key) (encode content)

-- | Adds a write action to a transaction.
saveDocument :: ToJSON a => FilePath -> FileName -> a -> TransactionWriter
saveDocument collection key content = do
  tell [createDirectoryIfMissing True collection,
        writeDocument collection key content]

-- | Adds a write action to a transaction.
-- The key will start with a numeric id, incremented from the last id in the collection.
saveNextDocument :: ToJSON a => FilePath -> FileName -> a -> TransactionWriter
saveNextDocument collection key content = do
  tell [createDirectoryIfMissing True collection,
        listDocumentKeys collection >>=
        return . nextKeyId >>=
        \nextId -> writeDocument collection (combineKey (nextId, key)) content]

-- | Lists collections in the current repository.
listCollections :: IO [FilePath]
listCollections = do
  contents <- try (getDirectoryContents =<< getCurrentDirectory) :: IO (Either IOException [FilePath])
  filterDirs $ fromMaybe [] $ hush contents

-- | Lists document keys in a collection.
listDocumentKeys :: FilePath -> IO [FileName]
listDocumentKeys collection = do
  contents <- try (getDirectoryContents collection) :: IO (Either IOException [String])
  return $ filterFilenamesAsKeys $ fromMaybe [] $ hush contents

-- | Lists entries in a collection.
listEntries :: FromJSON a => FilePath -> IO [a]
listEntries collection = do
  ks <- listDocumentKeys collection
  maybes <- mapM (readDocument collection) ks
  return $ fromMaybe [] $ sequence maybes

-- | Reads a document from a collection by key.
readDocument :: FromJSON a => FilePath -> FileName -> IO (Maybe a)
readDocument collection key = do
  jsonString <- try (BL.readFile $ documentPath collection key) :: IO (Either IOException BL.ByteString)
  return $ decode =<< hush jsonString

readDocument' :: FromJSON a => FilePath -> Maybe FileName -> IO (Maybe a)
readDocument' collection key = case key of
  Just key -> readDocument collection key
  Nothing -> return Nothing

splitFindDocument :: FilePath -> Finder -> IO (Maybe (IdAndName, FileName))
splitFindDocument collection finder = listDocumentKeys collection >>=
  return . finder . catMaybes . map (\x -> intoFunctor (maybeReadIntString x) x)

extractIdAndName :: Maybe (IdAndName, FileName) -> IO (Maybe IdAndName)
extractIdAndName m = case m of
                       Just (ian, _) -> return $ Just ian
                       _ -> return Nothing

extractFilename :: Maybe (IdAndName, FileName) -> IO (Maybe FileName)
extractFilename m = case m of
                     Just (_, fname) -> return $ Just fname
                     _ -> return Nothing

findById :: Int -> Finder
findById i = find $ (== i) . fst . fst

findByName :: String -> Finder
findByName n = find $ (isSuffixOf n) . snd . fst

-- | Reads a document from a collection by numeric id (for example, key "00001-hello" has id 1).
readDocumentById :: FromJSON a => FilePath -> Int -> IO (Maybe a)
readDocumentById collection i =
  splitFindDocument collection (findById i) >>=
  extractFilename >>=
  readDocument' collection

-- | Reads a document from a collection by name (for example, key "00001-hello" has name "hello").
readDocumentByName :: FromJSON a => FilePath -> String -> IO (Maybe a)
readDocumentByName collection n =
  splitFindDocument collection (findByName n) >>=
  extractFilename >>=
  readDocument' collection

-- | Returns a document's id by name (for example, "hello" will return 23 when key "00023-hello" exists).
-- Does not read the document!
documentIdFromName :: FilePath -> String -> IO (Maybe Int)
documentIdFromName collection n =
  splitFindDocument collection (findByName n) >>=
  extractIdAndName >>=
  return . (fst <$>)

-- | Returns a document's name by id (for example, 23 will return "hello" when key "00023-hello" exists).
-- Does not read the document!
documentNameFromId :: FilePath -> Int -> IO (Maybe String)
documentNameFromId collection i =
  splitFindDocument collection (findById i) >>=
  extractIdAndName >>=
  return . (drop 1 . snd <$>)
