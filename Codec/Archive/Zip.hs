{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  Codec.Archive.Zip
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides everything you may need to manipulate Zip archives.
-- There are three things that should be clarified right away, to avoid
-- confusion.
--
-- First, we use the 'EntrySelector' type that can be obtained from relative
-- 'FilePath's (paths to directories are not allowed). This method may seem
-- awkward at first, but it will protect you from the problems with
-- portability when your archive is unpacked on a different platform.
--
-- Second, there is no way to add directories, or to be precise, /empty
-- directories/ to your archive. This approach is used in Git, and I find it
-- sane.
--
-- Finally, the third feature of the library is that it does not modify
-- archive instantly, because doing so on every manipulation would often be
-- inefficient. Instead, we maintain a collection of pending actions that
-- can be turned into an optimized procedure that efficiently modifies the
-- archive in one pass. Normally, this should be of no concern to you,
-- because all actions are performed automatically when you leave the
-- 'ZipArchive' monad. If, however, you ever need to force an update, the
-- 'commit' function is your friend.
--
-- === Examples
--
-- An example of a program that prints a list of archive entries:
--
-- > import Codec.Archive.Zip
-- > import System.Environment (getArgs)
-- > import qualified Data.Map as M
-- >
-- > main :: IO ()
-- > main = do
-- >   [path]  <- getArgs
-- >   entries <- withArchive path (M.keys <$> getEntries)
-- >   mapM_ print entries
--
-- Create a Zip archive with a “Hello World” file:
--
-- > import Codec.Archive.Zip
-- > import System.Environment (getArgs)
-- >
-- > main :: IO ()
-- > main = do
-- >   [path] <- getArgs
-- >   s      <- mkEntrySelector "hello-world.txt"
-- >   createArchive path (addEntry Store "Hello, World!" s)
--
-- Extract contents of a file and print them:
--
-- > import Codec.Archive.Zip
-- > import System.Environment (getArgs)
-- > import qualified Data.ByteString.Char8 as B
-- >
-- > main :: IO ()
-- > main = do
-- >   [path,f] <- getArgs
-- >   s        <- mkEntrySelector f
-- >   bs       <- withArchive path (getEntry s)
-- >   B.putStrLn bs
module Codec.Archive.Zip
  ( -- * Types

    -- ** Entry selector
    EntrySelector,
    mkEntrySelector,
    unEntrySelector,
    getEntryName,
    EntrySelectorException (..),

    -- ** Entry description
    EntryDescription (..),
    CompressionMethod (..),

    -- ** Archive description
    ArchiveDescription (..),

    -- ** Exceptions
    ZipException (..),

    -- * Archive monad
    ZipArchive,
    ZipState,
    createArchive,
    withArchive,

    -- * Retrieving information
    getEntries,
    doesEntryExist,
    getEntryDesc,
    getEntry,
    getEntrySource,
    sourceEntry,
    saveEntry,
    checkEntry,
    unpackInto,
    getArchiveComment,
    getArchiveDescription,

    -- * Modifying archive

    -- ** Adding entries
    addEntry,
    sinkEntry,
    loadEntry,
    copyEntry,
    packDirRecur,
    packDirRecur',

    -- ** Modifying entries
    renameEntry,
    deleteEntry,
    recompress,
    setEntryComment,
    deleteEntryComment,
    setModTime,
    addExtraField,
    deleteExtraField,
    setExternalFileAttrs,
    forEntries,

    -- ** Operations on archive as a whole
    setArchiveComment,
    deleteArchiveComment,

    -- ** Control over editing
    undoEntryChanges,
    undoArchiveChanges,
    undoAll,
    commit,
  )
where

import Codec.Archive.Zip.Internal qualified as I
import Codec.Archive.Zip.Internal.Type
import Conduit (PrimMonad)
import Control.Monad
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, (.|))
import Data.Conduit qualified as C
import Data.Conduit.Binary qualified as CB
import Data.Conduit.List qualified as CL
import Data.DList qualified as DList
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as M
import Data.Sequence (Seq, (|>))
import Data.Sequence qualified as S
import Data.Set qualified as E
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Void
import Data.Word (Word16, Word32)
import System.Directory
import System.FilePath ((</>))
import System.FilePath qualified as FP
import System.IO.Error (isDoesNotExistError)

#ifndef mingw32_HOST_OS
import qualified Codec.Archive.Zip.Unix as Unix
import qualified System.Posix as Unix
#endif

----------------------------------------------------------------------------
-- Archive monad

-- | Monad that provides context necessary for performing operations on zip
-- archives. It's intentionally opaque and not a monad transformer to limit
-- the actions that can be performed in it to those provided by this module
-- and their combinations.
newtype ZipArchive a = ZipArchive
  { unZipArchive :: StateT ZipState IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

-- | @since 0.2.0
instance MonadBase IO ZipArchive where
  liftBase = liftIO

-- | @since 0.2.0
instance MonadBaseControl IO ZipArchive where
  type StM ZipArchive a = (a, ZipState)
  liftBaseWith f = ZipArchive . StateT $ \s ->
    (,s) <$> f (flip runStateT s . unZipArchive)
  {-# INLINEABLE liftBaseWith #-}
  restoreM = ZipArchive . StateT . const . return
  {-# INLINEABLE restoreM #-}

-- | The internal state record used by the 'ZipArchive' monad. This is only
-- exported for use with 'MonadBaseControl' methods, you can't look inside.
--
-- @since 0.2.0
data ZipState = ZipState
  { -- | Path to zip archive
    zsFilePath :: FilePath,
    -- | Actual collection of entries
    zsEntries :: Map EntrySelector EntryDescription,
    -- | Info about the whole archive
    zsArchive :: ArchiveDescription,
    -- | Pending actions
    zsActions :: Seq I.PendingAction
  }

-- | Create a new archive given its location and an action that describes
-- how to create contents of the archive. This will silently overwrite the
-- specified file if it already exists. See 'withArchive' if you want to
-- work with an existing archive.
createArchive ::
  (MonadIO m) =>
  -- | Location of the archive file to create
  FilePath ->
  -- | Actions that create the archive's content
  ZipArchive a ->
  m a
createArchive path m = liftIO $ do
  apath <- makeAbsolute path
  ignoringAbsence (removeFile apath)
  let st =
        ZipState
          { zsFilePath = apath,
            zsEntries = M.empty,
            zsArchive = ArchiveDescription Nothing 0 0,
            zsActions = S.empty
          }
      action = unZipArchive (m <* commit)
  evalStateT action st

-- | Work with an existing archive. See 'createArchive' if you want to
-- create a new archive instead.
--
-- This operation may fail with:
--
--     * @isAlreadyInUseError@ if the file is already open and cannot be
--     reopened;
--
--     * @isDoesNotExistError@ if the file does not exist;
--
--     * @isPermissionError@ if the user does not have permission to open
--     the file;
--
--     * 'ParsingFailed' when specified archive is something this library
--     cannot parse (this includes multi-disk archives, for example).
--
-- Please note that entries with invalid (non-portable) file names may be
-- missing in the list of entries. Files that are compressed with
-- unsupported compression methods are skipped as well. Also, if several
-- entries would collide on some operating systems (such as Windows, because
-- of its case-insensitivity), only one of them will be available, because
-- 'EntrySelector' is case-insensitive. These are the consequences of the
-- design decision to make it impossible to create non-portable archives
-- with this library.
withArchive ::
  (MonadIO m) =>
  -- | Location of the archive to work with
  FilePath ->
  -- | Actions on that archive
  ZipArchive a ->
  m a
withArchive path m = liftIO $ do
  apath <- canonicalizePath path
  (desc, entries) <- liftIO (I.scanArchive apath)
  let st =
        ZipState
          { zsFilePath = apath,
            zsEntries = entries,
            zsArchive = desc,
            zsActions = S.empty
          }
      action = unZipArchive (m <* commit)
  liftIO (evalStateT action st)

----------------------------------------------------------------------------
-- Retrieving information

-- | Retrieve a description of all archive entries. This is an efficient
-- operation that can be used for example to list all entries in the
-- archive. Do not hesitate to use the function frequently: scanning of the
-- archive happens only once.
--
-- Please note that the returned value only reflects the current contents of
-- the archive in file system, non-committed actions are not reflected, see
-- 'commit' for more information.
getEntries :: ZipArchive (Map EntrySelector EntryDescription)
getEntries = ZipArchive (gets zsEntries)

-- | Check whether the specified entry exists in the archive. This is a
-- simple shortcut defined as:
--
-- > doesEntryExist s = M.member s <$> getEntries
doesEntryExist :: EntrySelector -> ZipArchive Bool
doesEntryExist s = M.member s <$> getEntries

-- | Get 'EntryDescription' for a specified entry. This is a simple shortcut
-- defined as:
--
-- > getEntryDesc s = M.lookup s <$> getEntries
getEntryDesc :: EntrySelector -> ZipArchive (Maybe EntryDescription)
getEntryDesc s = M.lookup s <$> getEntries

-- | Get contents of a specific archive entry as a strict 'ByteString'. It's
-- not recommended to use this on big entries, because it will suck out a
-- lot of memory. For big entries, use conduits: 'sourceEntry'.
--
-- Throws: 'EntryDoesNotExist'.
getEntry ::
  -- | Selector that identifies archive entry
  EntrySelector ->
  -- | Contents of the entry
  ZipArchive ByteString
getEntry s = sourceEntry s (CL.foldMap id)

-- | Get an entry source.
--
-- Throws: 'EntryDoesNotExist'.
--
-- @since 0.1.3
getEntrySource ::
  (PrimMonad m, MonadThrow m, MonadResource m) =>
  -- | Selector that identifies archive entry
  EntrySelector ->
  ZipArchive (ConduitT () ByteString m ())
getEntrySource s = do
  path <- getFilePath
  mdesc <- M.lookup s <$> getEntries
  case mdesc of
    Nothing -> throwM (EntryDoesNotExist path s)
    Just desc -> return (I.sourceEntry path desc True)

-- | Stream contents of an archive entry to the given 'Sink'.
--
-- Throws: 'EntryDoesNotExist'.
sourceEntry ::
  -- | Selector that identifies the archive entry
  EntrySelector ->
  -- | Sink where to stream entry contents
  ConduitT ByteString Void (ResourceT IO) a ->
  -- | Contents of the entry (if found)
  ZipArchive a
sourceEntry s sink = do
  src <- getEntrySource s
  (liftIO . C.runConduitRes) (src .| sink)

-- | Save a specific archive entry as a file in the file system.
--
-- Throws: 'EntryDoesNotExist'.
saveEntry ::
  -- | Selector that identifies the archive entry
  EntrySelector ->
  -- | Where to save the file
  FilePath ->
  ZipArchive ()
saveEntry s path = do
  sourceEntry s (CB.sinkFile path)
  med <- getEntryDesc s
  forM_ med (liftIO . setModificationTime path . edModTime)

-- | Calculate CRC32 check sum and compare it with the value read from the
-- archive. The function returns 'True' when the check sums are the
-- same—that is, the data is not corrupted.
--
-- Throws: 'EntryDoesNotExist'.
checkEntry ::
  -- | Selector that identifies the archive entry
  EntrySelector ->
  -- | Is the entry intact?
  ZipArchive Bool
checkEntry s = do
  calculated <- sourceEntry s I.crc32Sink
  given <- edCRC32 . (! s) <$> getEntries
  -- NOTE We can assume that entry exists for sure because otherwise
  -- 'sourceEntry' would have thrown 'EntryDoesNotExist' already.
  return (calculated == given)

-- | Unpack the archive into the specified directory. The directory will be
-- created if it does not exist.
unpackInto :: FilePath -> ZipArchive ()
unpackInto dir' = do
  selectors <- M.keysSet <$> getEntries
  unless (null selectors) $ do
    dir <- liftIO (makeAbsolute dir')
    liftIO (createDirectoryIfMissing True dir)
    let dirs = E.map (FP.takeDirectory . (dir </>) . unEntrySelector) selectors
    forM_ dirs (liftIO . createDirectoryIfMissing True)
    forM_ selectors $ \s ->
      saveEntry s (dir </> unEntrySelector s)

-- | Get the archive comment.
getArchiveComment :: ZipArchive (Maybe Text)
getArchiveComment = adComment <$> getArchiveDescription

-- | Get the archive description record.
getArchiveDescription :: ZipArchive ArchiveDescription
getArchiveDescription = ZipArchive (gets zsArchive)

----------------------------------------------------------------------------
-- Modifying archive

-- | Add a new entry to the archive given its contents in binary form.
addEntry ::
  -- | The compression method to use
  CompressionMethod ->
  -- | Entry contents
  ByteString ->
  -- | Name of the entry to add
  EntrySelector ->
  ZipArchive ()
addEntry t b s = addPending (I.SinkEntry t (C.yield b) s)

-- | Stream data from the specified source to an archive entry.
sinkEntry ::
  -- | The compression method to use
  CompressionMethod ->
  -- | Source of entry contents
  ConduitT () ByteString (ResourceT IO) () ->
  -- | Name of the entry to add
  EntrySelector ->
  ZipArchive ()
sinkEntry t src s = addPending (I.SinkEntry t src s)

-- | Load an entry from a given file.
loadEntry ::
  -- | The compression method to use
  CompressionMethod ->
  -- | Name of the entry to add
  EntrySelector ->
  -- | Path to the file to add
  FilePath ->
  ZipArchive ()
loadEntry t s path = do
  apath <- liftIO (canonicalizePath path)
  modTime <- liftIO (getModificationTime path)
  let src = CB.sourceFile apath
  addPending (I.SinkEntry t src s)
  addPending (I.SetModTime modTime s)

#ifndef mingw32_HOST_OS
  status <- liftIO $ Unix.getFileStatus path
  setExternalFileAttrs (Unix.fromFileMode (Unix.fileMode status)) s
#endif

-- | Copy an entry “as is” from another zip archive. If the entry does not
-- exist in that archive, 'EntryDoesNotExist' will be thrown.
copyEntry ::
  -- | Path to the archive to copy from
  FilePath ->
  -- | Name of the entry (in the source archive) to copy
  EntrySelector ->
  -- | Name of the entry to insert (in current archive)
  EntrySelector ->
  ZipArchive ()
copyEntry path s' s = do
  apath <- liftIO (canonicalizePath path)
  addPending (I.CopyEntry apath s' s)

-- | Add an directory to the archive. Please note that due to the design of
-- the library, empty sub-directories will not be added.
--
-- The action can throw 'InvalidEntrySelector'.
packDirRecur ::
  -- | The compression method to use
  CompressionMethod ->
  -- | How to get the 'EntrySelector' from a path relative to the root of
  -- the directory we pack
  (FilePath -> ZipArchive EntrySelector) ->
  -- | Path to the directory to add
  FilePath ->
  ZipArchive ()
packDirRecur t f = packDirRecur' t f (const $ return ())

-- | The same as 'packDirRecur' but allows us to perform modifying actions
-- on the created entities as we go.
--
-- @since 1.5.0
packDirRecur' ::
  -- | The compression method to use
  CompressionMethod ->
  -- | How to get the 'EntrySelector' from a path relative to the root of
  -- the directory we pack
  (FilePath -> ZipArchive EntrySelector) ->
  -- | How to modify an entry after creation
  (EntrySelector -> ZipArchive ()) ->
  -- | Path to the directory to add
  FilePath ->
  ZipArchive ()
packDirRecur' t f patch path = do
  files <- liftIO (listDirRecur path)
  forM_ files $ \x -> do
    s <- f x
    loadEntry t s (path </> x)
    patch s

-- | Rename an entry in the archive. If the entry does not exist, nothing
-- will happen.
renameEntry ::
  -- | The original entry name
  EntrySelector ->
  -- | The new entry name
  EntrySelector ->
  ZipArchive ()
renameEntry old new = addPending (I.RenameEntry old new)

-- | Delete an entry from the archive, if it does not exist, nothing will
-- happen.
deleteEntry :: EntrySelector -> ZipArchive ()
deleteEntry s = addPending (I.DeleteEntry s)

-- | Change compression method of an entry, if it does not exist, nothing
-- will happen.
recompress ::
  -- | The new compression method
  CompressionMethod ->
  -- | Name of the entry to re-compress
  EntrySelector ->
  ZipArchive ()
recompress t s = addPending (I.Recompress t s)

-- | Set an entry comment, if that entry does not exist, nothing will
-- happen. Note that if binary representation of the comment is longer than
-- 65535 bytes, it will be truncated on writing.
setEntryComment ::
  -- | Text of the comment
  Text ->
  -- | Name of the entry to comment on
  EntrySelector ->
  ZipArchive ()
setEntryComment text s = addPending (I.SetEntryComment text s)

-- | Delete an entry's comment, if that entry does not exist, nothing will
-- happen.
deleteEntryComment :: EntrySelector -> ZipArchive ()
deleteEntryComment s = addPending (I.DeleteEntryComment s)

-- | Set the last modification date\/time. The specified entry may be
-- missing, in that case the action has no effect.
setModTime ::
  -- | New modification time
  UTCTime ->
  -- | Name of the entry to modify
  EntrySelector ->
  ZipArchive ()
setModTime time s = addPending (I.SetModTime time s)

-- | Add an extra field. The specified entry may be missing, in that case
-- this action has no effect.
addExtraField ::
  -- | Tag (header id) of the extra field to add
  Word16 ->
  -- | Body of the field
  ByteString ->
  -- | Name of the entry to modify
  EntrySelector ->
  ZipArchive ()
addExtraField n b s = addPending (I.AddExtraField n b s)

-- | Delete an extra field by its type (tag). The specified entry may be
-- missing, in that case this action has no effect.
deleteExtraField ::
  -- | Tag (header id) of the extra field to delete
  Word16 ->
  -- | Name of the entry to modify
  EntrySelector ->
  ZipArchive ()
deleteExtraField n s = addPending (I.DeleteExtraField n s)

-- | Set external file attributes. This function can be used to set file
-- permissions.
--
-- See also: "Codec.Archive.Zip.Unix".
--
-- @since 1.2.0
setExternalFileAttrs ::
  -- | External file attributes
  Word32 ->
  -- | Name of the entry to modify
  EntrySelector ->
  ZipArchive ()
setExternalFileAttrs attrs s =
  addPending (I.SetExternalFileAttributes attrs s)

-- | Perform an action on every entry in the archive.
forEntries ::
  -- | The action to perform
  (EntrySelector -> ZipArchive ()) ->
  ZipArchive ()
forEntries action = getEntries >>= mapM_ action . M.keysSet

-- | Set the comment of the entire archive.
setArchiveComment :: Text -> ZipArchive ()
setArchiveComment text = addPending (I.SetArchiveComment text)

-- | Delete the archive's comment if it's present.
deleteArchiveComment :: ZipArchive ()
deleteArchiveComment = addPending I.DeleteArchiveComment

-- | Undo the changes to a specific archive entry.
undoEntryChanges :: EntrySelector -> ZipArchive ()
undoEntryChanges s = modifyActions f
  where
    f = S.filter ((/= Just s) . I.targetEntry)

-- | Undo the changes to the archive as a whole (archive's comment).
undoArchiveChanges :: ZipArchive ()
undoArchiveChanges = modifyActions f
  where
    f = S.filter ((/= Nothing) . I.targetEntry)

-- | Undo all changes made in this editing session.
undoAll :: ZipArchive ()
undoAll = modifyActions (const S.empty)

-- | Archive contents are not modified instantly, but instead changes are
-- collected as “pending actions” that should be committed, in order to
-- efficiently modify the archive in one pass. The actions are committed
-- automatically when the program leaves the 'ZipArchive' monad (i.e. as
-- part of 'createArchive' or 'withArchive'), or can be forced explicitly
-- with the help of this function. Once committed, changes take place in the
-- file system and cannot be undone.
commit :: ZipArchive ()
commit = do
  file <- getFilePath
  odesc <- getArchiveDescription
  oentries <- getEntries
  actions <- getPending
  exists <- liftIO (doesFileExist file)
  unless (S.null actions && exists) $ do
    liftIO (I.commit file odesc oentries actions)
    -- NOTE The most robust way to update the internal description of the
    -- archive is to scan it again—manual manipulations with descriptions of
    -- entries are too error-prone. We also want to erase all pending
    -- actions because 'I.commit' executes them all by definition.
    (ndesc, nentries) <- liftIO (I.scanArchive file)
    ZipArchive . modify $ \st ->
      st
        { zsEntries = nentries,
          zsArchive = ndesc,
          zsActions = S.empty
        }

----------------------------------------------------------------------------
-- Helpers

-- | Get the path of the actual archive file from inside of 'ZipArchive'
-- monad.
getFilePath :: ZipArchive FilePath
getFilePath = ZipArchive (gets zsFilePath)

-- | Get the collection of pending actions.
getPending :: ZipArchive (Seq I.PendingAction)
getPending = ZipArchive (gets zsActions)

-- | Modify the collection of pending actions.
modifyActions :: (Seq I.PendingAction -> Seq I.PendingAction) -> ZipArchive ()
modifyActions f = ZipArchive (modify g)
  where
    g st = st {zsActions = f (zsActions st)}

-- | Add a new action to the list of pending actions.
addPending :: I.PendingAction -> ZipArchive ()
addPending a = modifyActions (|> a)

-- | Recursively list a directory. Do not return paths to empty directories.
listDirRecur :: FilePath -> IO [FilePath]
listDirRecur path = DList.toList <$> go ""
  where
    go adir = do
      let cdir = path </> adir
      raw <- listDirectory cdir
      fmap mconcat . forM raw $ \case
        "" -> return mempty
        "." -> return mempty
        ".." -> return mempty
        x -> do
          let fullx = cdir </> x
              adir' = adir </> x
          isFile <- doesFileExist fullx
          isDir <- doesDirectoryExist fullx
          if isFile
            then return (DList.singleton adir')
            else
              if isDir
                then go adir'
                else return mempty

-- | Perform an action ignoring IO exceptions it may throw.
ignoringAbsence :: IO () -> IO ()
ignoringAbsence io = catchJust select io handler
  where
    select e = if isDoesNotExistError e then Just e else Nothing
    handler = const (return ())
