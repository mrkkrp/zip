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
-- be turned into optimized action that efficiently modifies archive in one
-- pass. Normally this should not be of any concern for you, because all
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
  , ExtraField (..)
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
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Numeric.Natural
import Path
import Path.IO
import qualified Codec.Archive.Zip.Internal as I
import qualified Data.Conduit.Binary        as CB
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
  apath <- makeAbsolute path
  ignoringAbsence (removeFile apath)
  let st = ZipState
        { zsFilePath = apath
        , zsEntries  = M.empty
        , zsArchive  = ArchiveDescription Nothing
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
--     * @isDoesNotExistError@ if the file does not exist; or
--
--     * @isPermissionError@ if the user does not have permission to open
--     the file.

withArchive :: (MonadIO m, MonadThrow m)
  => Path b File       -- ^ Location of archive to work with
  -> ZipArchive a      -- ^ Actions on that archive
  -> m a
withArchive path m = do
  apath           <- makeAbsolute path
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
getEntry s = do
  path  <- getFilePath
  mdesc <- M.lookup s <$> getEntries
  case mdesc of
    Nothing   -> throwM (EntryDoesNotExist path s)
    Just desc -> liftIO' (I.getEntry path desc)

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
getArchiveComment = ZipArchive (gets (adComment . zsArchive))

----------------------------------------------------------------------------
-- Modifying archive

-- | Add a new entry to archive given its contents in binary form.

addEntry
  :: CompressionMethod -- ^ Compression method to use
  -> ByteString        -- ^ Entry contents
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive ()
addEntry = undefined

-- | Stream data from the specified source to an archive entry.

sinkEntry
  :: CompressionMethod -- ^ Compression method to use
  -> Source (ResourceT ZipArchive) ByteString -- ^ Source of entry content
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive ()
sinkEntry = undefined

-- | Load entry from given file.

loadEntry
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b File       -- ^ Path to file to add
  -> ZipArchive ()
loadEntry = undefined

-- | Copy entry “as is” from another .ZIP archive.

copyEntry
  :: Path b File       -- ^ Path to archive to copy from
  -> EntrySelector     -- ^ Name of entry to copy
  -> ZipArchive ()
copyEntry = undefined

-- | Add entire directory to archive. Please note that due to design of the
-- library, empty directories cannot be added to archive, just like in Git.
--
-- The action can throw the same exceptions as 'listDir'.

packDirRecur
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b Dir        -- ^ Path to directory to add
  -> ZipArchive ()
packDirRecur = undefined

-- | Rename entry in archive.

renameEntry
  :: EntrySelector     -- ^ Old entry name
  -> EntrySelector     -- ^ New entry name
  -> ZipArchive ()
renameEntry = undefined

-- | Delete entry from archive.

deleteEntry :: EntrySelector -> ZipArchive ()
deleteEntry = undefined

-- | Change compression method of an entry.

recompress
  :: CompressionMethod
  -> EntrySelector
  -> ZipArchive ()
recompress = undefined

-- | Set entry comment.

setEntryComment
  :: Text              -- ^ Text of the comment
  -> EntrySelector     -- ^ Name of entry to comment upon
  -> ZipArchive ()
setEntryComment = undefined

-- | Delete entry's comment.

deleteEntryComment :: EntrySelector -> ZipArchive ()
deleteEntryComment = undefined

-- | Set “last modification” date\/time.

setModTime
  :: UTCTime           -- ^ New modification time
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
setModTime = undefined

-- | Add an extra field.

addExtraField
  :: ExtraField        -- ^ Extra field to add
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()     -- ^ 'True' on success
addExtraField = undefined

-- | Delete an extra field by its type.

deleteExtraField
  :: Natural           -- ^ Type of extra field to delete
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive ()
deleteExtraField = undefined

-- | Set comment of entire archive.

setArchiveComment :: Text -> ZipArchive ()
setArchiveComment = undefined

-- | Delete archive comment if it's present.

deleteArchiveComment :: ZipArchive ()
deleteArchiveComment = undefined

-- | Undo changes to specific archive entry.

undoEntryChanges :: EntrySelector -> ZipArchive ()
undoEntryChanges = undefined

-- | Undo changes to archive as a whole (archive's comment).

undoArchiveChanges :: ZipArchive ()
undoArchiveChanges = undefined

-- | Undo all changes made in this editing session.

undoAll :: ZipArchive ()
undoAll = ZipArchive . modify $ \s -> s { zsActions = S.empty }

-- | Archive contents are not modified instantly, but instead changes are
-- collected as sort of “pending actions” that should be committed. The
-- actions are committed automatically when program leaves the realm of
-- 'ZipArchive' monad (i.e. as part of 'createArchive' or 'withArchive'), or
-- can be forced explicitly with help of this function. Once committed,
-- changes take place on file system and cannot be undone.

commit :: ZipArchive ()
commit = undefined

----------------------------------------------------------------------------
-- Helpers

-- | Get path of actual archive file from inside of 'ZipArchive' monad.

getFilePath :: ZipArchive (Path Abs File)
getFilePath = ZipArchive (gets zsFilePath)

-- | We do not permit arbitrary 'IO' inside 'ZipArchive' for users of the
-- library, but we can cheat and do it ourselves.

liftIO' :: IO a -> ZipArchive a
liftIO' = ZipArchive . liftIO
