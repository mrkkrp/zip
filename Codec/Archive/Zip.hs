-- |
-- Module      :  Codec.Archive.Zip
-- Copyright   :  © 2016–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides everything you may need to manipulate Zip archives.
-- There are three things that should be clarified right away, to avoid
-- confusion in the future.
--
-- First, we use the 'EntrySelector' type that can be obtained from 'Path'
-- 'Rel' 'File' paths. This method may seem awkward at first, but it will
-- protect you from the problems with portability when your archive is
-- unpacked on a different platform. Using well-typed paths is also
-- something you should consider doing in your projects anyway. Even if you
-- don't want to use the "Path" module in your project, it's easy to marshal
-- 'FilePath' to 'Path' just before using functions from the library.
--
-- The second thing, that is rather a consequence of the first, is that
-- there is no way to add directories, or to be precise, /empty directories/
-- to your archive. This approach is used in Git, and I find it quite sane.
--
-- Finally, the third feature of the library is that it does not modify
-- archive instantly, because doing so on every manipulation would often be
-- inefficient. Instead we maintain a collection of pending actions that can
-- be turned into an optimized procedure that efficiently modifies archive
-- in one pass. Normally this should be of no concern to you, because all
-- actions are performed automatically when you leave the realm of
-- 'ZipArchive' monad. If, however, you ever need to force an update, the
-- 'commit' function is your friend. There are even “undo” functions, by the
-- way.
--
-- === Examples
--
-- An example of a program that prints a list of archive entries:
--
-- > import Codec.Archive.Zip
-- > import Path.IO (resolveFile')
-- > import System.Environment (getArgs)
-- > import qualified Data.Map as M
-- >
-- > main :: IO ()
-- > main = do
-- >   [fp]    <- getArgs
-- >   path    <- resolveFile' fp
-- >   entries <- withArchive path (M.keys <$> getEntries)
-- >   mapM_ print entries
--
-- Create a Zip archive with a “Hello World” file:
--
-- > import Codec.Archive.Zip
-- > import Path (parseRelFile)
-- > import Path.IO (resolveFile')
-- > import System.Environment (getArgs)
-- >
-- > main :: IO ()
-- > main = do
-- >   [fp]    <- getArgs
-- >   path    <- resolveFile' fp
-- >   s       <- parseRelFile "hello-world.txt" >>= mkEntrySelector
-- >   createArchive path (addEntry Store "Hello, World!" s)
--
-- Extract contents of a specific file and print them:
--
-- > import Codec.Archive.Zip
-- > import Path (parseRelFile)
-- > import Path.IO (resolveFile')
-- > import System.Environment (getArgs)
-- > import qualified Data.ByteString.Char8 as B
-- >
-- > main :: IO ()
-- > main = do
-- >   [fp,f]  <- getArgs
-- >   path    <- resolveFile' fp
-- >   s       <- parseRelFile f >>= mkEntrySelector
-- >   bs      <- withArchive path (getEntry s)
-- >   B.putStrLn bs

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Codec.Archive.Zip
  ( -- * Types
    -- ** Entry selector
    EntrySelector
  , mkEntrySelector
  , unEntrySelector
  , getEntryName
  , EntrySelectorException (..)
    -- ** Entry description
  , EntryDescription (..)
  , CompressionMethod (..)
    -- ** Archive description
  , ArchiveDescription (..)
    -- ** Exceptions
  , ZipException (..)
    -- * Archive monad
  , ZipArchive
  , ZipState
  , createArchive
  , withArchive
    -- * Retrieving information
  , getEntries
  , doesEntryExist
  , getEntryDesc
  , getEntry
  , getEntrySource
  , sourceEntry
  , saveEntry
  , checkEntry
  , unpackInto
  , getArchiveComment
  , getArchiveDescription
    -- * Modifying archive
    -- ** Adding entries
  , addEntry
  , sinkEntry
  , loadEntry
  , copyEntry
  , packDirRecur
    -- ** Modifying entries
  , renameEntry
  , deleteEntry
  , recompress
  , setEntryComment
  , deleteEntryComment
  , setModTime
  , addExtraField
  , deleteExtraField
  , forEntries
    -- ** Operations on archive as a whole
  , setArchiveComment
  , deleteArchiveComment
    -- ** Control over editing
  , undoEntryChanges
  , undoArchiveChanges
  , undoAll
  , commit )
where

import Codec.Archive.Zip.Type
import Conduit (PrimMonad)
import Control.Monad
import Control.Monad.Base (MonadBase (..))
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Control (MonadBaseControl (..))
import Control.Monad.Trans.Resource (ResourceT, MonadResource)
import Data.ByteString (ByteString)
import Data.Conduit (ConduitT, (.|))
import Data.Map.Strict (Map, (!))
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Void
import Data.Word (Word16)
import Path
import Path.IO
import qualified Codec.Archive.Zip.Internal as I
import qualified Data.Conduit               as C
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import qualified Data.Map.Strict            as M
import qualified Data.Sequence              as S
import qualified Data.Set                   as E

----------------------------------------------------------------------------
-- Archive monad

-- | Monad that provides context necessary for performing operations on zip
-- archives. It's intentionally opaque and not a monad transformer to limit
-- the actions that can be performed in it to those provided by this module
-- and their combinations.

newtype ZipArchive a = ZipArchive
  { unZipArchive :: StateT ZipState IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadThrow
             , MonadCatch
             , MonadMask )

-- | @since 0.2.0

instance MonadBase IO ZipArchive where
  liftBase = liftIO

-- | @since 0.2.0

instance MonadBaseControl IO ZipArchive where
  type StM ZipArchive a = (a, ZipState)
  liftBaseWith f = ZipArchive . StateT $ \s ->
    (\x -> (x, s)) <$> f (flip runStateT s . unZipArchive)
  {-# INLINEABLE liftBaseWith #-}
  restoreM       = ZipArchive . StateT . const . return
  {-# INLINEABLE restoreM #-}

-- | Internal state record used by the 'ZipArchive' monad. This is only
-- exported for use with 'MonadBaseControl' methods, you can't look inside.
--
-- @since 0.2.0

data ZipState = ZipState
  { zsFilePath  :: Path Abs File
    -- ^ Absolute path to zip archive
  , zsEntries   :: Map EntrySelector EntryDescription
    -- ^ Actual collection of entries
  , zsArchive   :: ArchiveDescription
    -- ^ Info about the whole archive
  , zsActions   :: Seq I.PendingAction
    -- ^ Pending actions
  }

-- | Create a new archive given its location and an action that describes
-- how to create contents of the archive. This will silently overwrite the
-- specified file if it already exists. See 'withArchive' if you want to
-- work with an existing archive.

createArchive :: MonadIO m
  => Path b File       -- ^ Location of archive file to create
  -> ZipArchive a      -- ^ Actions that form archive's content
  -> m a
createArchive path m = liftIO $ do
  apath <- makeAbsolute path
  ignoringAbsence (removeFile apath)
  let st = ZipState
        { zsFilePath = apath
        , zsEntries  = M.empty
        , zsArchive  = ArchiveDescription Nothing 0 0
        , zsActions  = S.empty }
      action = unZipArchive (liftM2 const m commit)
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

withArchive :: MonadIO m
  => Path b File       -- ^ Location of archive to work with
  -> ZipArchive a      -- ^ Actions on that archive
  -> m a
withArchive path m = liftIO $ do
  apath           <- canonicalizePath path
  (desc, entries) <- liftIO (I.scanArchive apath)
  let st = ZipState
        { zsFilePath = apath
        , zsEntries  = entries
        , zsArchive  = desc
        , zsActions  = S.empty }
      action = unZipArchive (liftM2 const m commit)
  liftIO (evalStateT action st)

----------------------------------------------------------------------------
-- Retrieving information

-- | Retrieve description of all archive entries. This is an efficient
-- operation that can be used for example to list all entries in an archive.
-- Do not hesitate to use the function frequently: scanning of archive
-- happens only once anyway.
--
-- Please note that the returned value only reflects actual contents of the
-- archive in file system, non-committed actions do not influence the list
-- of entries, see 'commit' for more information.

getEntries :: ZipArchive (Map EntrySelector EntryDescription)
getEntries = ZipArchive (gets zsEntries)

-- | Check whether the specified entry exists in the archive. This is a
-- simple shortcut defined as:
--
-- > doesEntryExist s = M.member s <$> getEntries

doesEntryExist :: EntrySelector -> ZipArchive Bool
doesEntryExist s = M.member s <$> getEntries

-- | Get 'EntryDescription' for specified entry. This is a simple shortcut
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

getEntry
  :: EntrySelector     -- ^ Selector that identifies archive entry
  -> ZipArchive ByteString -- ^ Contents of the entry
getEntry s = sourceEntry s (CL.foldMap id)

-- | Get an entry source.
--
-- Throws: 'EntryDoesNotExist'.
--
-- @since 0.1.3

getEntrySource
  :: (PrimMonad m, MonadThrow m, MonadResource m)
  => EntrySelector
  -> ZipArchive (ConduitT () ByteString m ())
getEntrySource s = do
  path  <- getFilePath
  mdesc <- M.lookup s <$> getEntries
  case mdesc of
    Nothing   -> throwM (EntryDoesNotExist path s)
    Just desc -> return (I.sourceEntry path desc True)

-- | Stream contents of an archive entry to the given 'Sink'.
--
-- Throws: 'EntryDoesNotExist'.

sourceEntry
  :: EntrySelector
     -- ^ Selector that identifies archive entry
  -> ConduitT ByteString Void (ResourceT IO) a
     -- ^ Sink where to stream entry contents
  -> ZipArchive a
     -- ^ Contents of the entry (if found)
sourceEntry s sink = do
  src <- getEntrySource s
  (liftIO . C.runConduitRes) (src .| sink)

-- | Save a specific archive entry as a file in the file system.
--
-- Throws: 'EntryDoesNotExist'.

saveEntry
  :: EntrySelector     -- ^ Selector that identifies archive entry
  -> Path b File       -- ^ Where to save the file
  -> ZipArchive ()
saveEntry s path = sourceEntry s (CB.sinkFile (toFilePath path))

-- | Calculate CRC32 check sum and compare it with the value read from the
-- archive. The function returns 'True' when the check sums are the
-- same—that is, the data is not corrupted.
--
-- Throws: 'EntryDoesNotExist'.

checkEntry
  :: EntrySelector     -- ^ Selector that identifies archive entry
  -> ZipArchive Bool   -- ^ Is the entry intact?
checkEntry s = do
  calculated <- sourceEntry s I.crc32Sink
  given      <- edCRC32 . (! s) <$> getEntries
  -- ↑ NOTE We can assume that entry exists for sure because otherwise
  -- 'sourceEntry' would have thrown 'EntryDoesNotExist' already.
  return (calculated == given)

-- | Unpack the entire archive into the specified directory. The directory
-- will be created if it does not exist.

unpackInto :: Path b Dir -> ZipArchive ()
unpackInto dir' = do
  selectors <- M.keysSet <$> getEntries
  unless (null selectors) $ do
    dir <- liftIO (makeAbsolute dir')
    liftIO (ensureDir dir)
    let dirs = E.map (parent . (dir </>) . unEntrySelector) selectors
    forM_ dirs (liftIO . ensureDir)
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

addEntry
  :: CompressionMethod -- ^ Compression method to use
  -> ByteString        -- ^ Entry contents
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive ()
addEntry t b s = addPending (I.SinkEntry t (C.yield b) s)

-- | Stream data from the specified source to an archive entry.

sinkEntry
  :: CompressionMethod -- ^ Compression method to use
  -> ConduitT () ByteString (ResourceT IO) () -- ^ Source of entry contents
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive ()
sinkEntry t src s = addPending (I.SinkEntry t src s)

-- | Load an entry from a given file.

loadEntry
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> ZipArchive EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b File       -- ^ Path to file to add
  -> ZipArchive ()
loadEntry t f path = do
  apath   <- liftIO (canonicalizePath path)
  s       <- f apath
  modTime <- liftIO (getModificationTime path)
  let src = CB.sourceFile (toFilePath apath)
  addPending (I.SinkEntry t src s)
  addPending (I.SetModTime modTime s)

-- | Copy an entry “as is” from another zip archive. If the entry does not
-- exist in that archive, 'EntryDoesNotExist' will be eventually thrown.

copyEntry
  :: Path b File       -- ^ Path to archive to copy from
  -> EntrySelector     -- ^ Name of entry (in source archive) to copy
  -> EntrySelector     -- ^ Name of entry to insert (in actual archive)
  -> ZipArchive ()
copyEntry path s' s = do
  apath <- liftIO (canonicalizePath path)
  addPending (I.CopyEntry apath s' s)

-- | Add an entire directory to the archive. Please note that due to the
-- design of the library, empty sub-directories won't be added.
--
-- The action can throw the same exceptions as 'listDirRecur' and
-- 'InvalidEntrySelector'.

packDirRecur
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> ZipArchive EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b Dir        -- ^ Path to directory to add
  -> ZipArchive ()
packDirRecur t f path = do
  files <- snd <$> liftIO (listDirRecur path)
  mapM_ (loadEntry t f) files

-- | Rename an entry in the archive. If the entry does not exist, nothing
-- will happen.

renameEntry
  :: EntrySelector     -- ^ Original entry name
  -> EntrySelector     -- ^ New entry name
  -> ZipArchive ()
renameEntry old new = addPending (I.RenameEntry old new)

-- | Delete an entry from the archive, if it does not exist, nothing will
-- happen.

deleteEntry :: EntrySelector -> ZipArchive ()
deleteEntry s = addPending (I.DeleteEntry s)

-- | Change compression method of an entry, if it does not exist, nothing
-- will happen.

recompress
  :: CompressionMethod -- ^ New compression method
  -> EntrySelector     -- ^ Name of entry to re-compress
  -> ZipArchive ()
recompress t s = addPending (I.Recompress t s)

-- | Set an entry comment, if that entry does not exist, nothing will
-- happen. Note that if binary representation of the comment is longer than
-- 65535 bytes, it will be truncated on writing.

setEntryComment
  :: Text              -- ^ Text of the comment
  -> EntrySelector     -- ^ Name of entry to comment on
  -> ZipArchive ()
setEntryComment text s = addPending (I.SetEntryComment text s)

-- | Delete an entry's comment, if that entry does not exist, nothing will
-- happen.

deleteEntryComment :: EntrySelector -> ZipArchive ()
deleteEntryComment s = addPending (I.DeleteEntryComment s)

-- | Set the “last modification” date\/time. The specified entry may be
-- missing, in that case the action has no effect.

setModTime
  :: UTCTime           -- ^ New modification time
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
setModTime time s = addPending (I.SetModTime time s)

-- | Add an extra field. The specified entry may be missing, in that case
-- this action has no effect.

addExtraField
  :: Word16            -- ^ Tag (header id) of extra field to add
  -> ByteString        -- ^ Body of the field
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
addExtraField n b s = addPending (I.AddExtraField n b s)

-- | Delete an extra field by its type (tag). The specified entry may be
-- missing, in that case this action has no effect.

deleteExtraField
  :: Word16            -- ^ Tag (header id) of extra field to delete
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
deleteExtraField n s = addPending (I.DeleteExtraField n s)

-- | Perform an action on every entry in the archive.

forEntries
  :: (EntrySelector -> ZipArchive ()) -- ^ Action to perform
  -> ZipArchive ()
forEntries action = getEntries >>= mapM_ action . M.keysSet

-- | Set comment of the entire archive.

setArchiveComment :: Text -> ZipArchive ()
setArchiveComment text = addPending (I.SetArchiveComment text)

-- | Delete the archive comment if it's present.

deleteArchiveComment :: ZipArchive ()
deleteArchiveComment = addPending I.DeleteArchiveComment

-- | Undo changes to a specific archive entry.

undoEntryChanges :: EntrySelector -> ZipArchive ()
undoEntryChanges s = modifyActions f
  where f = S.filter ((/= Just s) . I.targetEntry)

-- | Undo changes to the archive as a whole (archive's comment).

undoArchiveChanges :: ZipArchive ()
undoArchiveChanges = modifyActions f
  where f = S.filter ((/= Nothing) . I.targetEntry)

-- | Undo all changes made in this editing session.

undoAll :: ZipArchive ()
undoAll = modifyActions (const S.empty)

-- | Archive contents are not modified instantly, but instead changes are
-- collected as “pending actions” that should be committed, in order to
-- efficiently modify the archive in one pass. The actions are committed
-- automatically when the program leaves the realm of 'ZipArchive' monad
-- (i.e. as part of 'createArchive' or 'withArchive'), or can be forced
-- explicitly with the help of this function. Once committed, changes take
-- place in the file system and cannot be undone.

commit :: ZipArchive ()
commit = do
  file     <- getFilePath
  odesc    <- getArchiveDescription
  oentries <- getEntries
  actions  <- getPending
  exists   <- doesFileExist file
  unless (S.null actions && exists) $ do
    liftIO (I.commit file odesc oentries actions)
    -- NOTE The most robust way to update internal description of the
    -- archive is to scan it again—manual manipulations with descriptions of
    -- entries are too error-prone. We also want to erase all pending
    -- actions because 'I.commit' executes them all by definition.
    (ndesc, nentries) <- liftIO (I.scanArchive file)
    ZipArchive . modify $ \st -> st
      { zsEntries = nentries
      , zsArchive = ndesc
      , zsActions = S.empty }

----------------------------------------------------------------------------
-- Helpers

-- | Get the path of the actual archive file from inside of 'ZipArchive'
-- monad.

getFilePath :: ZipArchive (Path Abs File)
getFilePath = ZipArchive (gets zsFilePath)

-- | Get the collection of pending actions.

getPending :: ZipArchive (Seq I.PendingAction)
getPending = ZipArchive (gets zsActions)

-- | Modify the collection of pending actions in some way.

modifyActions :: (Seq I.PendingAction -> Seq I.PendingAction) -> ZipArchive ()
modifyActions f = ZipArchive (modify g)
  where g st = st { zsActions = f (zsActions st) }

-- | Add a new action to the list of pending actions.

addPending :: I.PendingAction -> ZipArchive ()
addPending a = modifyActions (|> a)
