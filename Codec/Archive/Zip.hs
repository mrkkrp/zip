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
  , getArchiveDescription
    -- * Modifying archive
    -- ** Adding entries
  , addEntry
  , sinkEntry
  , loadEntry
  , packDirRecur
    -- ** Modifying entries
  , renameEntry
  , deleteEntry
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

import Codec.Archive.Zip.Internal
import Codec.Archive.Zip.Type
import Control.Monad
import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString (ByteString)
import Data.Conduit (Source, Sink)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Numeric.Natural
import Path
import Path.IO
import qualified Data.ByteString as B

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
  { zsFilePath  :: Path Abs File      -- ^ Absolute path to zip archive
  , zsEntries   :: [EntryDescription] -- ^ Actual collection of entries
  , zsArchive   :: ArchiveDescription -- ^ Info about the whole archive
  , zsActions   :: [PendingAction]    -- ^ Pending actions
  }

-- | Create new archive given its location and action that describes how to
-- create content in the archive. This will silently overwrite specified
-- file if it already exists. See 'withArchive' if you want to work with
-- existing archive.

createArchive :: MonadIO m
  => Path b File       -- ^ Location of archive file to create
  -> ZipArchive a      -- ^ Actions that form archive's content
  -> m a
createArchive = undefined

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

withArchive :: MonadIO m
  => Path b File       -- ^ Location of archive to work with
  -> ZipArchive a      -- ^ Actions on that archive
  -> m a
withArchive = undefined

----------------------------------------------------------------------------
-- Retrieving information

-- | Retrieve description of all archive entries. This is an efficient
-- operation that can be used for example to list all entries in archive.

getEntries :: ZipArchive [EntryDescription]
getEntries = undefined

-- | Get contents of specific archive entry as a lazy 'BL.ByteString'. It's
-- not recommended to use this on big entries, because it will suck out a
-- lot of memory. For big entries, use conduits: 'sourceEntry'.

getEntry
  :: EntrySelector
     -- ^ Selector that identifies archive entry
  -> ZipArchive (Maybe ByteString)
     -- ^ Contents of the entry (if found)
getEntry = undefined

-- | Stream contents of archive entry to specified 'Sink'.

sourceEntry
  :: EntrySelector
     -- ^ Selector that identifies archive entry
  -> Sink B.ByteString (ResourceT ZipArchive) a
     -- ^ Sink where to stream entry contents
  -> ZipArchive (Maybe a)
     -- ^ Contents of the entry (if found)
sourceEntry = undefined

-- | Save specific archive entry as a file in file system. This tries to
-- restore file attributes if possible.

saveEntry
  :: EntrySelector     -- ^ Selector that identifies archive entry
  -> Path b File       -- ^ Where to save the file
  -> ZipArchive Bool   -- ^ Was the file found in the archive?
saveEntry = undefined

-- | Unpack entire archive into specified directory. The directory will be
-- created if it does not exist.

unpackInto :: Path b Dir -> ZipArchive ()
unpackInto dir = do
  entries <- getEntries
  unless (null entries) $ do
    liftIO' (ensureDir dir)
    forM_ entries $ \EntryDescription {..} ->
      -- TODO Create sub-directories as needed
      saveEntry edSelector (dir </> unEntrySelector edSelector)

-- | Get archive comment.

getArchiveDescription :: ZipArchive ArchiveDescription
getArchiveDescription = undefined

----------------------------------------------------------------------------
-- Modifying archive

-- | Add a new entry to archive given its contents in binary form.

addEntry
  :: CompressionMethod -- ^ Compression method to use
  -> ByteString        -- ^ Entry contents
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive Bool   -- ^ 'True' if successfully added
addEntry = undefined

-- | Stream data from the specified source to an archive entry.

sinkEntry
  :: CompressionMethod -- ^ Compression method to use
  -> Source (ResourceT ZipArchive) ByteString -- ^ Source of entry content
  -> EntrySelector     -- ^ Name of entry to add
  -> ZipArchive Bool   -- ^ 'True' if successfully added
sinkEntry = undefined

-- | Load entry from given file.

loadEntry
  :: CompressionMethod -- ^ Compression method to use
  -> (Path Abs File -> EntrySelector) -- ^ How to get 'EntrySelector'
  -> Path b File       -- ^ Path to file to add
  -> ZipArchive Bool   -- ^ 'True' if successfully added
loadEntry = undefined

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
  -> ZipArchive Bool   -- ^ 'True' if the entry has been renamed
renameEntry = undefined

-- | Delete entry from archive.

deleteEntry
  :: EntrySelector     -- ^ Name of entry to delete
  -> ZipArchive Bool   -- ^ 'True' if the entry has been deleted
deleteEntry = undefined

-- | Set entry comment.

setEntryComment
  :: Text              -- ^ Text of the comment
  -> EntrySelector     -- ^ Name of entry to comment upon
  -> ZipArchive Bool   -- ^ 'True' on success
setEntryComment = undefined

-- | Delete entry's comment.

deleteEntryComment
  :: EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive Bool   -- ^ 'True' on success
deleteEntryComment = undefined

-- | Set “last modification” date\/time.

setModTime
  :: UTCTime           -- ^ New modification time
  -> EntrySelector     -- ^ Name of entry to modify
  -> ZipArchive Bool   -- ^ 'True' on success
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
  -> ZipArchive Bool   -- ^ 'True' in success
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
undoAll = do
  undoArchiveChanges
  entries <- getEntries
  mapM_ undoEntryChanges (edSelector <$> entries)

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

-- | We do not permit arbitrary 'IO' inside 'ZipArchive' for users of the
-- library, but we can cheat and do it ourselves.

liftIO' :: IO a -> ZipArchive a
liftIO' = ZipArchive . liftIO
