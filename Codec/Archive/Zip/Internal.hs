-- |
-- Module      :  Codec.Archive.Zip.Internal
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level, non-public concepts and operations.

module Codec.Archive.Zip.Internal
  ( PendingAction (..)
  , targetEntry
  , scanArchive
  , getEntry
  , sourceEntry
  , withOptimizedActions
  , commit
  )
where

import Codec.Archive.Zip.Type
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString
import Data.Char (ord)
import Data.Conduit (Source, Sink)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Numeric.Natural (Natural)
import Path
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | The sum type describes all possible actions that can be performed on
-- archive.

data PendingAction
  = AddEntry CompressionMethod ByteString EntrySelector
    -- ^ Add entry given its binary content as strict 'ByteString'
  | SinkEntry CompressionMethod (Source (ResourceT IO) ByteString) EntrySelector
    -- ^ Add entry given its 'Source'
  | LoadEntry CompressionMethod (Path Abs File) EntrySelector
    -- ^ Load entry from file on disk
  | CopyEntry (Path Abs File) EntrySelector
    -- ^ Copy an entry form another archive without re-compression
  | RenameEntry EntrySelector EntrySelector
    -- ^ Change name the entry inside archive
  | DeleteEntry EntrySelector
    -- ^ Delete entry from archive
  | Recompress CompressionMethod EntrySelector
    -- ^ Change compression method on an entry
  | SetEntryComment Text EntrySelector
    -- ^ Set comment for a particular entry
  | DeleteEntryComment EntrySelector
    -- ^ Delete comment of particular entry
  | SetModTime UTCTime EntrySelector
    -- ^ Set modification time of particular entry
  | AddExtraField ExtraField EntrySelector
    -- ^ Add an extra field to specified entry
  | DeleteExtraField Natural EntrySelector
    -- ^ Delete an extra filed of specified entry
  | SetArchiveComment Text
    -- ^ Set comment for entire archive
  | DeleteArchiveComment
    -- ^ Delete comment of entire archive

-- | Determine target entry of action.

targetEntry :: PendingAction -> Maybe EntrySelector
targetEntry (AddEntry       _ _ s) = Just s
targetEntry (SinkEntry      _ _ s) = Just s
targetEntry (LoadEntry      _ _ s) = Just s
targetEntry (CopyEntry        _ s) = Just s
targetEntry (RenameEntry      s _) = Just s
targetEntry (DeleteEntry        s) = Just s
targetEntry (Recompress       _ s) = Just s
targetEntry (SetEntryComment  _ s) = Just s
targetEntry (DeleteEntryComment s) = Just s
targetEntry (SetModTime       _ s) = Just s
targetEntry (AddExtraField    _ s) = Just s
targetEntry (DeleteExtraField _ s) = Just s
targetEntry (SetArchiveComment  _) = Nothing
targetEntry DeleteArchiveComment   = Nothing

-- | Scan central directory of an archive and return its description
-- 'ArchiveDescription' as well as collection of its entries.
--
-- Throws 'MultiDiskArchive'.

scanArchive
  :: Path Abs File     -- ^ Path to archive to scan
  -> IO (ArchiveDescription, Map EntrySelector EntryDescription)
scanArchive = undefined

-- | Given location of archive and information about specific archive entry
-- 'EntryDescription', return its contents as strict 'ByteString'. Returned
-- binary data is decompressed.

getEntry
  :: Path Abs File     -- ^ Path to archive that contains the entry
  -> EntryDescription  -- ^ Information needed to extract entry of interest
  -> IO ByteString     -- ^ Decompressed binary data
getEntry = undefined

-- | The same as 'getEntry', but archive contents are not returned directly,
-- but instead are streamed to given 'Sink'.

sourceEntry
  :: Path Abs File     -- ^ Path to archive that contains the entry
  -> EntryDescription  -- ^ Information needed to extract entry of interest
  -> Sink ByteString (ResourceT IO) a -- ^ Where to stream decompressed data
  -> IO a
sourceEntry = undefined

-- | Transform given pending actions so they can be performed efficiently in
-- one pass. The action also prepares environment when necessary
-- (e.g. creates temporary files).

withOptimizedActions
  :: Path Abs File     -- ^ Location of archive file to edit or create
  -> ArchiveDescription -- ^ Archive description
  -> Seq PendingAction -- ^ Collection of pending actions
  -> (Path Abs File -> Seq PendingAction -> IO ())
     -- ^ Given name of file where to write archive and optimized actions,
     -- do it
  -> IO ()
withOptimizedActions = undefined

-- | Undertake /all/ actions specified in the first argument of the
-- function.

commit
  :: Path Abs File     -- ^ Where to write the new archive
  -> Seq PendingAction -- ^ Collection of actions (should be optimized)
  -> IO ()
commit = undefined

----------------------------------------------------------------------------
-- Helpers

-- | Detect if the given text needs newer Unicode-aware features to be
-- properly encoded in archive.

needsUnicode :: Text -> Bool
needsUnicode = not . T.all validCP437
  where validCP437 x = let y = ord x in y >= 32 && y <= 126
