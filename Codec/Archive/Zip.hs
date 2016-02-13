-- |
-- Module      :  Codec.Archive.Zip
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides everything you need to manipulate Zip archives. The
-- library covers all functionality that may be of interest of most users,
-- however, there are three things that should clarified right away, to
-- avoid confusion in the future.
--
-- First, we use 'EntrySelector' type that can be obtained from 'Path' 'Rel'
-- 'File' things. This method may seem awkward at first, but it will protect
-- you from problems with portability when your archive is unpacked on a
-- different platform. Using of well-typed paths is also something you
-- should consider doing in your projects anyway.
--
-- The second thing, that is rather a consequence of the first, is that
-- there is no way to add directories, or to be precise, /empty directories/
-- to your archive. This approach is used in Git, and I find it quite sane.
--
-- Finally, the third feature of the library is that it does not modify
-- archive instantly, because doing so on every manipulation would often be
-- inefficient. Instead we maintain collection of pending actions that can
-- be turned into optimized procedure that efficiently modifies archive in
-- one pass. Normally this should be of no concern to you, because all
-- actions are performed automatically when you leave the realm of
-- 'ZipArchive' monad. If, however, you ever need to force update, 'commit'
-- function is your friend. There are even “undo” functions, by the way.
--
-- Let's take a look at some examples that show how to accomplish most
-- typical tasks with help of the library.
--
-- TODO Add examples here.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codec.Archive.Zip
  ( -- * Types
    -- ** Entry selector
    EntrySelector
  , mkEntrySelector
  , unEntrySelector
  , getEntryName
  , EntrySelectorException
    -- ** Entry description
  , EntryDescription (..)
  , CompressionMethod (..)
    -- ** Archive description
  , ArchiveDescription (..)
    -- ** Exceptions
  , ZipException (..)
    -- * Archive monad
  , ZipArchive
  , createArchive
  , withArchive
    -- * Retrieving information
  , getEntries
  , getEntry
  , sourceEntry
  , saveEntry
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
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State.Strict
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (Source, Sink)
import Data.Map.Strict (Map)
import Data.Sequence (Seq, (<|))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Numeric.Natural
import Path
import Path.IO
import qualified Codec.Archive.Zip.Internal as I
import qualified Data.Conduit.Binary        as CB
import qualified Data.Conduit.List          as CL
import qualified Data.Map.Strict            as M
import qualified Data.Sequence              as S
import qualified Data.Set                   as E

----------------------------------------------------------------------------
-- Archive monad

-- | Monad that provides context necessary for performing operations on
-- archives. It's intentionally opaque and not a monad transformer to limit
-- number of actions that can be performed in it to those provided by this
-- module and their combinations.
--
-- Note: you cannot perform 'IO' actions in this monad, although provided
-- primitives can do it internally.

newtype ZipArchive a = ZipArchive
  { unZipArchive :: StateT ZipState IO a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadThrow
             , MonadCatch
             , MonadMask )

-- | Internal state record used by the 'ZipArchive' monad.

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

-- | Create new archive given its location and action that describes how to
-- create content in the archive. This will silently overwrite specified
-- file if it already exists. See 'withArchive' if you want to work with
-- existing archive.

createArchive :: (MonadIO m, MonadCatch m)
  => Path b File       -- ^ Location of archive file to create
  -> ZipArchive a      -- ^ Actions that form archive's content
  -> m a
createArchive path m = do
  apath <- canonicalizePath path
  ignoringAbsence (removeFile apath)
  let st = ZipState
        { zsFilePath = apath
        , zsEntries  = M.empty
        , zsArchive  = ArchiveDescription Nothing 0 0
        , zsActions  = S.empty }
      action = unZipArchive (liftM2 const m commit)
  liftIO (evalStateT action st)

-- | Work with already existing archive. See 'createArchive' if you want to
-- create new archive instead.
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
-- missing in list of entries. Files that are compressed with unsupported
-- compression methods are skipped as well. Also, if several entries would
-- collide on some operating systems (such as Windows, because of its
-- case-insensitivity), only one of them will be available, because
-- 'EntrySelector' is case-insensitive. These are consequences of the design
-- decision to make it impossible to create non-portable archives with this
-- library.

withArchive :: (MonadIO m, MonadThrow m)
  => Path b File       -- ^ Location of archive to work with
  -> ZipArchive a      -- ^ Actions on that archive
  -> m a
withArchive path m = do
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
-- operation that can be used for example to list all entries in archive.

getEntries :: ZipArchive (Map EntrySelector EntryDescription)
getEntries = ZipArchive (gets zsEntries)

-- | Get contents of specific archive entry as a lazy 'BL.ByteString'. It's
-- not recommended to use this on big entries, because it will suck out a
-- lot of memory. For big entries, use conduits: 'sourceEntry'.
--
-- Throws: 'EntryDoesNotExist'.

getEntry
  :: EntrySelector
     -- ^ Selector that identifies archive entry
  -> ZipArchive ByteString
     -- ^ Contents of the entry (if found)
getEntry s = sourceEntry s (CL.foldMap id)

-- | Stream contents of archive entry to specified 'Sink'.
--
-- Throws: 'EntryDoesNotExist'.

sourceEntry
  :: EntrySelector
     -- ^ Selector that identifies archive entry
  -> Sink ByteString (ResourceT IO) a
     -- ^ Sink where to stream entry contents
  -> ZipArchive a
     -- ^ Contents of the entry (if found)
sourceEntry s sink = do
  path  <- getFilePath
  mdesc <- M.lookup s <$> getEntries
  case mdesc of
    Nothing   -> throwM (EntryDoesNotExist path s)
    Just desc -> liftIO' (I.sourceEntry path desc sink)

-- | Save specific archive entry as a file in file system.
--
-- Throws: 'EntryDoesNotExist'.

saveEntry
  :: EntrySelector     -- ^ Selector that identifies archive entry
  -> Path b File       -- ^ Where to save the file
  -> ZipArchive ()     -- ^ Was the file found in the archive?
saveEntry s path = sourceEntry s (CB.sinkFile (toFilePath path))

-- | Unpack entire archive into specified directory. The directory will be
-- created if it does not exist.

unpackInto :: Path b Dir -> ZipArchive ()
unpackInto dir' = do
  selectors <- M.keysSet <$> getEntries
  unless (null selectors) $ do
    dir <- liftIO' (makeAbsolute dir')
    liftIO' (ensureDir dir)
    let dirs = E.map (parent . (dir </>) . unEntrySelector) selectors
    forM_ dirs (liftIO' . ensureDir)
    forM_ selectors $ \s ->
      saveEntry s (dir </> unEntrySelector s)

-- | Get archive comment.

getArchiveComment :: ZipArchive (Maybe Text)
getArchiveComment = adComment <$> getArchiveDescription

-- | Get archive description record.

getArchiveDescription :: ZipArchive ArchiveDescription
getArchiveDescription = ZipArchive (gets zsArchive)

----------------------------------------------------------------------------
-- Modifying archive

-- | Add a new entry to archive given its contents in binary form.

addEntry
  :: CompressionMethod -- ^ Compression method to use
  -> ByteString        -- ^ Entry contents
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive ()
addEntry t b s = addPending (I.AddEntry t b s)

-- | Stream data from the specified source to an archive entry.

sinkEntry
  :: CompressionMethod -- ^ Compression method to use
  -> Source (ResourceT IO) ByteString -- ^ Source of entry content
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive ()
sinkEntry t src s = addPending (I.SinkEntry t src s)

-- | Load entry from given file.

loadEntry
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> ZipArchive EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b File       -- ^ Path to file to add
  -> ZipArchive ()
loadEntry t f path = do
  apath <- liftIO' (canonicalizePath path)
  s     <- f apath
  addPending (I.LoadEntry t apath s)

-- | Copy entry “as is” from another .ZIP archive.

copyEntry
  :: Path b File       -- ^ Path to archive to copy from
  -> EntrySelector     -- ^ Name of entry to copy
  -> ZipArchive ()
copyEntry path s = do
  apath <- liftIO' (canonicalizePath path)
  addPending (I.CopyEntry apath s)

-- | Add entire directory to archive. Please note that due to design of the
-- library, empty directories won't be added to archive.
--
-- The action can throw the same exceptions as 'listDir' and
-- 'InvalidEntrySelector'.

packDirRecur
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> ZipArchive EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b Dir        -- ^ Path to directory to add
  -> ZipArchive ()
packDirRecur t f path = do
  files <- snd <$> liftIO' (listDirRecur path)
  forM_ files $ \file -> do
    s   <- f file
    addPending (I.LoadEntry t file s)

-- | Rename entry in archive.

renameEntry
  :: EntrySelector     -- ^ Old entry name
  -> EntrySelector     -- ^ New entry name
  -> ZipArchive ()
renameEntry old new = addPending (I.RenameEntry old new)

-- | Delete entry from archive.

deleteEntry :: EntrySelector -> ZipArchive ()
deleteEntry s = addPending (I.DeleteEntry s)

-- | Change compression method of an entry.

recompress
  :: CompressionMethod -- ^ New compression method
  -> EntrySelector     -- ^ Name of entry to re-compress
  -> ZipArchive ()
recompress t s = addPending (I.Recompress t s)

-- | Set entry comment.

setEntryComment
  :: Text              -- ^ Text of the comment
  -> EntrySelector     -- ^ Name of entry to comment upon
  -> ZipArchive ()
setEntryComment text s = addPending (I.SetEntryComment text s)

-- | Delete entry's comment.

deleteEntryComment :: EntrySelector -> ZipArchive ()
deleteEntryComment s = addPending (I.DeleteEntryComment s)

-- | Set “last modification” date\/time.

setModTime
  :: UTCTime           -- ^ New modification time
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
setModTime time s = addPending (I.SetModTime time s)

-- | Add an extra field.

addExtraField
  :: Natural           -- ^ Extra field to add
  -> ByteString        -- ^ Body of the field
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()     -- ^ 'True' on success
addExtraField n b s = addPending (I.AddExtraField n b s)

-- | Delete an extra field by its type.

deleteExtraField
  :: Natural           -- ^ Type of extra field to delete
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
deleteExtraField n s = addPending (I.DeleteExtraField n s)

-- | Set comment of entire archive.

setArchiveComment :: Text -> ZipArchive ()
setArchiveComment text = addPending (I.SetArchiveComment text)

-- | Delete archive comment if it's present.

deleteArchiveComment :: ZipArchive ()
deleteArchiveComment = addPending I.DeleteArchiveComment

-- | Undo changes to specific archive entry.

undoEntryChanges :: EntrySelector -> ZipArchive ()
undoEntryChanges s = modifyActions f
  where f = S.filter ((== Just s) . I.targetEntry)

-- | Undo changes to archive as a whole (archive's comment).

undoArchiveChanges :: ZipArchive ()
undoArchiveChanges = modifyActions f
  where f = S.filter ((== Nothing) . I.targetEntry)

-- | Undo all changes made in this editing session.

undoAll :: ZipArchive ()
undoAll = modifyActions (const S.empty)

-- | Archive contents are not modified instantly, but instead changes are
-- collected as “pending actions” that should be committed. The actions are
-- committed automatically when program leaves the realm of 'ZipArchive'
-- monad (i.e. as part of 'createArchive' or 'withArchive'), or can be
-- forced explicitly with help of this function. Once committed, changes
-- take place in the file system and cannot be undone.

commit :: ZipArchive ()
commit = do
  file    <- getFilePath
  odesc   <- getArchiveDescription
  actions <- getPending
  unless (S.null actions) $ do
    liftIO' (I.withOptimizedActions file odesc actions I.commit)
    -- NOTE The most robust way to update internal description of the
    -- archive is to scan it again — manual manipulations with descriptions
    -- of entries are too error-prone. We also want to erase all pending
    -- actions because 'I.commit' executes them all by definition.
    (ndesc, entries) <- liftIO' (I.scanArchive file)
    ZipArchive . modify $ \st -> st
      { zsEntries = entries
      , zsArchive = ndesc
      , zsActions = S.empty }

----------------------------------------------------------------------------
-- Helpers

-- | Get path of actual archive file from inside of 'ZipArchive' monad.

getFilePath :: ZipArchive (Path Abs File)
getFilePath = ZipArchive (gets zsFilePath)

-- | Get collection of pending actions.

getPending :: ZipArchive (Seq I.PendingAction)
getPending = ZipArchive (gets zsActions)

-- | We do not permit arbitrary 'IO' inside 'ZipArchive' for users of the
-- library, but we can cheat and do it ourselves.

liftIO' :: IO a -> ZipArchive a
liftIO' = ZipArchive . liftIO

-- | Modify collection of pending actions in some way.

modifyActions :: (Seq I.PendingAction -> Seq I.PendingAction) -> ZipArchive ()
modifyActions f = ZipArchive (modify g)
  where g st = st { zsActions = f (zsActions st) }

-- | Add new action to the list of pending actions.

addPending :: I.PendingAction -> ZipArchive ()
addPending a = modifyActions (a <|)
