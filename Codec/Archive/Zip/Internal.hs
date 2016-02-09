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
  = DeleteArchive (Path Abs File)
    -- ^ Delete archive file if it exists
  | AddEntry CompressionMethod ByteString EntrySelector
  | SinkEntry CompressionMethod (Source (ResourceT IO) ByteString) EntrySelector
  | LoadEntry CompressionMethod (Path Abs File) EntrySelector
  | CopyEntry (Path Abs File) EntrySelector
  | RenameEntry EntrySelector EntrySelector
  | DeleteEntry EntrySelector
  | Recompress CompressionMethod EntrySelector
  | SetEntryComment Text EntrySelector
  | DeleteEntryComment EntrySelector
  | SetModTime UTCTime EntrySelector
  | AddExtraField ExtraField EntrySelector
  | DeleteExtraField Natural EntrySelector
  | SetArchiveComment Text
  | DeleteArchiveComment

-- | Determine target entry of action.

targetEntry :: PendingAction -> Maybe EntrySelector
targetEntry = undefined

-- | Throws 'MultiDiskArchive'.

scanArchive
  :: Path Abs File
  -> IO (ArchiveDescription, Map EntrySelector EntryDescription)
scanArchive = undefined

getEntry
  :: Path Abs File
  -> EntryDescription
  -> IO ByteString
getEntry = undefined

sourceEntry
  :: Path Abs File
  -> EntryDescription
  -> Sink ByteString (ResourceT IO) a
  -> IO a
sourceEntry = undefined

withOptimizedActions
  :: Path Abs File
  -> ArchiveDescription -- ???
  -> Seq PendingAction
  -> (Seq PendingAction -> IO ())
  -> IO ()
withOptimizedActions = undefined

commit :: Seq PendingAction -> IO ()
commit = undefined

----------------------------------------------------------------------------
-- Helpers

-- | Detect if the given text needs newer Unicode-aware features to be
-- properly encoded in archive.

needsUnicode :: Text -> Bool
needsUnicode = not . T.all validCP437
  where validCP437 x = let y = ord x in y >= 32 && y <= 126
