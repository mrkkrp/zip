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
import Control.Monad.Catch
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.ByteString
import Data.Char (ord)
import Data.Conduit (Source, Sink, ($=), ($$))
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Serialize
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Numeric.Natural (Natural)
import Path
import System.IO
import qualified Data.ByteString     as B
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.BZlib  as BZ
import qualified Data.Conduit.Zlib   as Z
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T

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

scanArchive
  :: Path Abs File     -- ^ Path to archive to scan
  -> IO (ArchiveDescription, Map EntrySelector EntryDescription)
scanArchive path = withFile (toFilePath path) ReadMode $ \h ->
  case locateECD h of
    Just ecdOffset -> do
      hSeek h AbsoluteSeek ecdOffset
      ecdRaw <- B.hGetContents h
      case runGet getECD ecdRaw of
        Left  msg -> throwM (ParsingFailed path msg)
        Right ecd -> do
          hSeek h AbsoluteSeek $ fromIntegral (adCDOffset ecd)
          cdRaw <- B.hGet h $ fromIntegral (adCDSize ecd)
          case runGet getCD cdRaw of
            Left  msg -> throwM (ParsingFailed path msg)
            Right cd  -> return (ecd, cd)
    Nothing ->
      throwM (ParsingFailed path "Cannot locate end of central directory")

-- | Given location of archive and information about specific archive entry
-- 'EntryDescription', return its contents as strict 'ByteString'. Returned
-- binary data is decompressed.

getEntry
  :: Path Abs File     -- ^ Path to archive that contains the entry
  -> EntryDescription  -- ^ Information needed to extract entry of interest
  -> IO ByteString     -- ^ Decompressed binary data
getEntry path desc = sourceEntry path desc (CL.foldMap id)

-- | The same as 'getEntry', but archive contents are not returned directly,
-- but instead are streamed to given 'Sink'.

sourceEntry
  :: Path Abs File     -- ^ Path to archive that contains the entry
  -> EntryDescription  -- ^ Information needed to extract entry of interest
  -> Sink ByteString (ResourceT IO) a -- ^ Where to stream decompressed data
  -> IO a
sourceEntry path ed@EntryDescription {..} sink = runResourceT $
  source $= CB.isolate (fromIntegral edCompressedSize) $= decompress $$ sink
  where
    source = CB.sourceIOHandle $ do
      h      <- openFile (toFilePath path) ReadMode
      offset <- fileDataOffset h ed
      hSeek h AbsoluteSeek offset
      return h
    decompress = case edCompression of
      Store   -> CL.map id
      Deflate -> Z.decompress Z.defaultWindowBits
      BZip2   -> BZ.bunzip2

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

-- | Undertake /all/ actions specified in the second argument of the
-- function.

commit
  :: Path Abs File     -- ^ Where to write the new archive
  -> Seq PendingAction -- ^ Collection of actions (should be optimized)
  -> IO ()
commit = undefined

----------------------------------------------------------------------------
-- Binary serialization

-- | Parse end of central directory record or Zip64 end of central directory
-- record depending on signature binary data begins with.

getECD :: Get ArchiveDescription
getECD = undefined

-- | Parse central directory file headers and put them into 'Map'.

getCD :: Get (Map EntrySelector EntryDescription)
getCD = undefined

-- | Find absolute offset of end of central directory record or, if present,
-- Zip64 end of central directory record.

locateECD :: Handle -> Maybe Integer
locateECD = undefined

----------------------------------------------------------------------------
-- Helpers

-- | Calculate file data offset from its 'EntryDescription'.

fileDataOffset :: Handle -> EntryDescription -> IO Integer
fileDataOffset = undefined

-- | Detect if the given text needs newer Unicode-aware features to be
-- properly encoded in archive.

needsUnicode :: Text -> Bool
needsUnicode = not . T.all validCP437
  where validCP437 x = let y = ord x in y >= 32 && y <= 126
