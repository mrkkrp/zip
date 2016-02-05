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
  , scanArchive
  , getEntry
  , sourceEntry
  )
where

import Codec.Archive.Zip.Type
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString
import Data.Char (ord)
import Data.Conduit (Source, Sink)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Path
import qualified Data.Map.Strict    as M
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

-- | The sum type describes all possible actions that can be performed on
-- archive.

data PendingAction
  = DeleteArchive (Path Abs File)
    -- ^ Delete archive file if it exists
  | AddEntry
  | RemoveEntry

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

----------------------------------------------------------------------------
-- Helpers

-- | Detect if the given text needs newer Unicode-aware features to be
-- properly encoded in archive.

needsUnicode :: Text -> Bool
needsUnicode = not . T.all validCP437
  where validCP437 x = let y = ord x in y >= 32 && y <= 126
