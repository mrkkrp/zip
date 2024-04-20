{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      :  Codec.Archive.Zip.Internal
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level, non-public types and operations.
module Codec.Archive.Zip.Internal
  ( PendingAction (..),
    targetEntry,
    scanArchive,
    sourceEntry,
    crc32Sink,
    commit,
  )
where

import Codec.Archive.Zip.CP437 (decodeCP437)
import Codec.Archive.Zip.Type
import Conduit (PrimMonad)
import Control.Applicative (many, (<|>))
import Control.Exception (bracketOnError, catchJust)
import Control.Monad
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource (MonadResource, ResourceT)
import Data.Bits
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char (ord)
import Data.Conduit (ConduitT, ZipSink (..), (.|))
import Data.Conduit qualified as C
import Data.Conduit.Binary qualified as CB
import Data.Conduit.List qualified as CL
import Data.Conduit.Zlib qualified as Z
import Data.Digest.CRC32 (crc32Update)
import Data.Fixed (Fixed (..))
import Data.Foldable (foldl')
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromJust, isNothing)
import Data.Sequence (Seq, (><), (|>))
import Data.Sequence qualified as S
import Data.Serialize
import Data.Set qualified as E
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Version
import Data.Void
import Data.Word (Word16, Word32)
import Numeric.Natural (Natural)
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)

#ifndef mingw32_HOST_OS
import qualified Codec.Archive.Zip.Unix as Unix
#endif

#ifdef ENABLE_BZIP2
import qualified Data.Conduit.BZlib as BZ
#endif

#ifdef ENABLE_ZSTD
import qualified Data.Conduit.Zstd as Zstandard
#endif

----------------------------------------------------------------------------
-- Data types

-- | The sum type describes all possible actions that can be performed on an
-- archive.
data PendingAction
  = -- | Add an entry given its 'Source'
    SinkEntry
      CompressionMethod
      (ConduitT () ByteString (ResourceT IO) ())
      EntrySelector
  | -- | Copy an entry form another archive without re-compression
    CopyEntry FilePath EntrySelector EntrySelector
  | -- | Change the name of the entry inside archive
    RenameEntry EntrySelector EntrySelector
  | -- | Delete an entry from archive
    DeleteEntry EntrySelector
  | -- | Change the compression method on an entry
    Recompress CompressionMethod EntrySelector
  | -- | Set the comment for a particular entry
    SetEntryComment Text EntrySelector
  | -- | Delete theh comment of a particular entry
    DeleteEntryComment EntrySelector
  | -- | Set the modification time of a particular entry
    SetModTime UTCTime EntrySelector
  | -- | Add an extra field to the specified entry
    AddExtraField Word16 ByteString EntrySelector
  | -- | Delete an extra filed of the specified entry
    DeleteExtraField Word16 EntrySelector
  | -- | Set the comment for the entire archive
    SetArchiveComment Text
  | -- | Delete the comment of the entire archive
    DeleteArchiveComment
  | -- | Set an external file attribute for the specified entry
    SetExternalFileAttributes Word32 EntrySelector

-- | A collection of maps describing how to produce entries in the resulting
-- archive.
data ProducingActions = ProducingActions
  { paCopyEntry :: Map FilePath (Map EntrySelector EntrySelector),
    paSinkEntry :: Map EntrySelector (ConduitT () ByteString (ResourceT IO) ())
  }

-- | A collection of editing actions, that is, actions that modify already
-- existing entries.
data EditingActions = EditingActions
  { eaCompression :: Map EntrySelector CompressionMethod,
    eaEntryComment :: Map EntrySelector Text,
    eaDeleteComment :: Map EntrySelector (),
    eaModTime :: Map EntrySelector UTCTime,
    eaExtraField :: Map EntrySelector (Map Word16 ByteString),
    eaDeleteField :: Map EntrySelector (Map Word16 ()),
    eaExtFileAttr :: Map EntrySelector Word32
  }

-- | The origin of entries that can be streamed into archive.
data EntryOrigin
  = GenericOrigin
  | Borrowed EntryDescription

-- | The type of the file header: local or central directory.
data HeaderType
  = LocalHeader
  | CentralDirHeader
  deriving (Eq)

-- | The data descriptor representation.
data DataDescriptor = DataDescriptor
  { ddCRC32 :: Word32,
    ddCompressedSize :: Natural,
    ddUncompressedSize :: Natural
  }

-- | A temporary data structure to hold Zip64 extra data field information.
data Zip64ExtraField = Zip64ExtraField
  { z64efUncompressedSize :: Natural,
    z64efCompressedSize :: Natural,
    z64efOffset :: Natural
  }

-- | MS-DOS date-time: a pair of 'Word16' (date, time) with the following
-- structure:
--
-- > DATE bit     0 - 4           5 - 8           9 - 15
-- >      value   day (1 - 31)    month (1 - 12)  years from 1980
-- > TIME bit     0 - 4           5 - 10          11 - 15
-- >      value   seconds*        minute          hour
-- >              *stored in two-second increments
data MsDosTime = MsDosTime
  { msDosDate :: Word16,
    msDosTime :: Word16
  }

----------------------------------------------------------------------------
-- Constants

-- | “Version created by” to specify when writing archive data.
zipVersion :: Version
zipVersion = Version [6, 3] []

----------------------------------------------------------------------------
-- Higher-level operations

-- | Scan the central directory of an archive and return its description
-- 'ArchiveDescription' as well as a collection of its entries.
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
scanArchive ::
  -- | Path to archive to scan
  FilePath ->
  IO (ArchiveDescription, Map EntrySelector EntryDescription)
scanArchive path = withBinaryFile path ReadMode $ \h -> do
  mecdOffset <- locateECD path h
  case mecdOffset of
    Just ecdOffset -> do
      hSeek h AbsoluteSeek ecdOffset
      ecdSize <- subtract ecdOffset <$> hFileSize h
      ecdRaw <- B.hGet h (fromIntegral ecdSize)
      case runGet getECD ecdRaw of
        Left msg -> throwM (ParsingFailed path msg)
        Right ecd -> do
          hSeek h AbsoluteSeek $ fromIntegral (adCDOffset ecd)
          cdRaw <- B.hGet h $ fromIntegral (adCDSize ecd)
          case runGet getCD cdRaw of
            Left msg -> throwM (ParsingFailed path msg)
            Right cd -> return (ecd, cd)
    Nothing ->
      throwM (ParsingFailed path "Cannot locate end of central directory")

-- | Given location of the archive and information about a specific archive
-- entry 'EntryDescription', return 'Source' of its data. The actual data
-- can be compressed or uncompressed depending on the third argument.
sourceEntry ::
  (PrimMonad m, MonadThrow m, MonadResource m) =>
  -- | Path to archive that contains the entry
  FilePath ->
  -- | Information needed to extract entry of interest
  EntryDescription ->
  -- | Should we stream uncompressed data?
  Bool ->
  -- | Source of uncompressed data
  ConduitT () ByteString m ()
sourceEntry path EntryDescription {..} d =
  source .| CB.isolate (fromIntegral edCompressedSize) .| decompress
  where
    source = CB.sourceIOHandle $ do
      h <- openFile path ReadMode
      hSeek h AbsoluteSeek (fromIntegral edOffset)
      localHeader <- B.hGet h 30
      case runGet getLocalHeaderGap localHeader of
        Left msg -> throwM (ParsingFailed path msg)
        Right gap -> do
          hSeek h RelativeSeek gap
          return h
    decompress =
      if d
        then decompressingPipe edCompression
        else C.awaitForever C.yield

-- | Undertake /all/ actions specified as the fourth argument of the
-- function. This transforms the given pending actions so they can be
-- performed in one pass, and then they are applied in the most efficient
-- way.
commit ::
  -- | Location of archive file to edit or create
  FilePath ->
  -- | Archive description
  ArchiveDescription ->
  -- | Current list of entires
  Map EntrySelector EntryDescription ->
  -- | Collection of pending actions
  Seq PendingAction ->
  IO ()
commit path ArchiveDescription {..} entries xs =
  withNewFile path $ \h -> do
    let (ProducingActions coping sinking, editing) =
          optimize (toRecreatingActions path entries >< xs)
        comment = predictComment adComment xs
    copiedCD <-
      M.unions
        <$> forM
          (M.keys coping)
          ( \srcPath ->
              copyEntries h srcPath (coping ! srcPath) editing
          )
    let sinkingKeys = M.keys $ sinking `M.difference` copiedCD
    sunkCD <-
      M.fromList
        <$> forM
          sinkingKeys
          ( \selector ->
              sinkEntry h selector GenericOrigin (sinking ! selector) editing
          )
    writeCD h comment (copiedCD `M.union` sunkCD)

-- | Create a new file with the guarantee that in the case of an exception
-- the old file will be intact. The file is only updated\/replaced if the
-- second argument finishes without exceptions.
withNewFile ::
  -- | Name of file to create
  FilePath ->
  -- | Action that writes to given 'Handle'
  (Handle -> IO ()) ->
  IO ()
withNewFile fpath action =
  bracketOnError allocate release $ \(path, h) -> do
    action h
    hClose h
    renameFile path fpath
  where
    allocate = openBinaryTempFile (takeDirectory fpath) ".zip"
    release (path, h) = do
      hClose h
      -- Despite using `bracketOnError` the file is not guaranteed to exist
      -- here since we could be interrupted with an async exception after
      -- the file has been renamed. Therefore, we silentely ignore
      -- `DoesNotExistError`.
      catchJust (guard . isDoesNotExistError) (removeFile path) (const $ pure ())

-- | Determine what comment in new archive will look like given its original
-- value and a collection of pending actions.
predictComment :: Maybe Text -> Seq PendingAction -> Maybe Text
predictComment original xs =
  case S.index xs <$> S.findIndexR (isNothing . targetEntry) xs of
    Nothing -> original
    Just DeleteArchiveComment -> Nothing
    Just (SetArchiveComment txt) -> Just txt
    Just _ -> Nothing

-- | Transform a map representing existing entries into a collection of
-- actions that re-create those entires.
toRecreatingActions ::
  -- | Name of the archive file where entires are found
  FilePath ->
  -- | Actual list of entires
  Map EntrySelector EntryDescription ->
  -- | Actions that recreate the archive entries
  Seq PendingAction
toRecreatingActions path entries = E.foldl' f S.empty (M.keysSet entries)
  where
    f s e = s |> CopyEntry path e e

-- | Transform a collection of 'PendingAction's into 'ProducingActions' and
-- 'EditingActions'—data that describes how to create resulting archive.
optimize ::
  -- | Collection of pending actions
  Seq PendingAction ->
  -- | Optimized data
  (ProducingActions, EditingActions)
optimize =
  foldl'
    f
    ( ProducingActions M.empty M.empty,
      EditingActions M.empty M.empty M.empty M.empty M.empty M.empty M.empty
    )
  where
    f (pa, ea) a = case a of
      SinkEntry m src s ->
        ( pa
            { paSinkEntry = M.insert s src (paSinkEntry pa),
              paCopyEntry = M.map (M.filter (/= s)) (paCopyEntry pa)
            },
          (clearEditingFor s ea)
            { eaCompression = M.insert s m (eaCompression ea)
            }
        )
      CopyEntry path os ns ->
        ( pa
            { paSinkEntry = M.delete ns (paSinkEntry pa),
              paCopyEntry = M.alter (ef os ns) path (paCopyEntry pa)
            },
          clearEditingFor ns ea
        )
      RenameEntry os ns ->
        ( pa
            { paCopyEntry = M.map (M.map $ re os ns) (paCopyEntry pa),
              paSinkEntry = renameKey os ns (paSinkEntry pa)
            },
          ea
            { eaCompression = renameKey os ns (eaCompression ea),
              eaEntryComment = renameKey os ns (eaEntryComment ea),
              eaDeleteComment = renameKey os ns (eaDeleteComment ea),
              eaModTime = renameKey os ns (eaModTime ea),
              eaExtraField = renameKey os ns (eaExtraField ea),
              eaDeleteField = renameKey os ns (eaDeleteField ea)
            }
        )
      DeleteEntry s ->
        ( pa
            { paSinkEntry = M.delete s (paSinkEntry pa),
              paCopyEntry = M.map (M.delete s) (paCopyEntry pa)
            },
          clearEditingFor s ea
        )
      Recompress m s ->
        (pa, ea {eaCompression = M.insert s m (eaCompression ea)})
      SetEntryComment txt s ->
        ( pa,
          ea
            { eaEntryComment = M.insert s txt (eaEntryComment ea),
              eaDeleteComment = M.delete s (eaDeleteComment ea)
            }
        )
      DeleteEntryComment s ->
        ( pa,
          ea
            { eaEntryComment = M.delete s (eaEntryComment ea),
              eaDeleteComment = M.insert s () (eaDeleteComment ea)
            }
        )
      SetModTime time s ->
        (pa, ea {eaModTime = M.insert s time (eaModTime ea)})
      AddExtraField n b s ->
        ( pa,
          ea
            { eaExtraField = M.alter (ef n b) s (eaExtraField ea),
              eaDeleteField = M.delete s (eaDeleteField ea)
            }
        )
      DeleteExtraField n s ->
        ( pa,
          ea
            { eaExtraField = M.alter (er n) s (eaExtraField ea),
              eaDeleteField = M.alter (ef n ()) s (eaDeleteField ea)
            }
        )
      SetExternalFileAttributes b s ->
        ( pa,
          ea {eaExtFileAttr = M.insert s b (eaExtFileAttr ea)}
        )
      _ -> (pa, ea)
    clearEditingFor s ea =
      ea
        { eaCompression = M.delete s (eaCompression ea),
          eaEntryComment = M.delete s (eaEntryComment ea),
          eaDeleteComment = M.delete s (eaDeleteComment ea),
          eaModTime = M.delete s (eaModTime ea),
          eaExtraField = M.delete s (eaExtraField ea),
          eaDeleteField = M.delete s (eaDeleteField ea),
          eaExtFileAttr = M.delete s (eaExtFileAttr ea)
        }
    re o n x = if x == o then n else x
    ef k v (Just m) = Just (M.insert k v m)
    ef k v Nothing = Just (M.singleton k v)
    er k (Just m) =
      let n = M.delete k m
       in if M.null n then Nothing else Just n
    er _ Nothing = Nothing

-- | Copy entries from another archive and write them into the file
-- associated with the given handle. This can throw 'EntryDoesNotExist' if
-- there is no such entry in that archive.
copyEntries ::
  -- | Opened 'Handle' of zip archive file
  Handle ->
  -- | Path to the file to copy the entries from
  FilePath ->
  -- | 'Map' from original name to name to use in new archive
  Map EntrySelector EntrySelector ->
  -- | Additional info that can influence result
  EditingActions ->
  -- | Info to generate central directory file headers later
  IO (Map EntrySelector EntryDescription)
copyEntries h path m e = do
  entries <- snd <$> scanArchive path
  done <- forM (M.keys m) $ \s ->
    case s `M.lookup` entries of
      Nothing -> throwM (EntryDoesNotExist path s)
      Just desc ->
        sinkEntry
          h
          (m ! s)
          (Borrowed desc)
          (sourceEntry path desc False)
          e
  return (M.fromList done)

-- | Sink an entry from the given stream into the file associated with the
-- given 'Handle'.
sinkEntry ::
  -- | Opened 'Handle' of zip archive file
  Handle ->
  -- | Name of the entry to add
  EntrySelector ->
  -- | Origin of the entry (can contain additional info)
  EntryOrigin ->
  -- | Source of the entry contents
  ConduitT () ByteString (ResourceT IO) () ->
  -- | Additional info that can influence result
  EditingActions ->
  -- | Info to generate the central directory file headers later
  IO (EntrySelector, EntryDescription)
sinkEntry h s o src EditingActions {..} = do
  currentTime <- getCurrentTime
  offset <- hTell h
  let compressed = case o of
        GenericOrigin -> Store
        Borrowed ed -> edCompression ed
      compression = M.findWithDefault compressed s eaCompression
      recompression = compression /= compressed
      modTime = case o of
        GenericOrigin -> currentTime
        Borrowed ed -> edModTime ed
      extFileAttr = case o of
        GenericOrigin -> M.findWithDefault defaultFileMode s eaExtFileAttr
        Borrowed _ -> M.findWithDefault defaultFileMode s eaExtFileAttr
      oldExtraFields = case o of
        GenericOrigin -> M.empty
        Borrowed ed -> edExtraField ed
      extraField =
        (M.findWithDefault M.empty s eaExtraField `M.union` oldExtraFields)
          `M.difference` M.findWithDefault M.empty s eaDeleteField
      oldComment = case (o, M.lookup s eaDeleteComment) of
        (GenericOrigin, _) -> Nothing
        (Borrowed ed, Nothing) -> edComment ed
        (Borrowed _, Just ()) -> Nothing
      desc0 =
        EntryDescription -- to write in local header
          { edVersionMadeBy = zipVersion,
            edVersionNeeded = zipVersion,
            edCompression = compression,
            edModTime = M.findWithDefault modTime s eaModTime,
            edCRC32 = 0, -- to be overwritten after streaming
            edCompressedSize = 0, -- ↑
            edUncompressedSize = 0, -- ↑
            edOffset = fromIntegral offset,
            edComment = M.lookup s eaEntryComment <|> oldComment,
            edExtraField = extraField,
            edExternalFileAttrs = extFileAttr
          }
  B.hPut h (runPut (putHeader LocalHeader s desc0))
  DataDescriptor {..} <-
    C.runConduitRes $
      if recompression
        then
          if compressed == Store
            then src .| sinkData h compression
            else src .| decompressingPipe compressed .| sinkData h compression
        else src .| sinkData h Store
  afterStreaming <- hTell h
  let desc1 = case o of
        GenericOrigin ->
          desc0
            { edCRC32 = ddCRC32,
              edCompressedSize = ddCompressedSize,
              edUncompressedSize = ddUncompressedSize
            }
        Borrowed ed ->
          desc0
            { edCRC32 =
                bool (edCRC32 ed) ddCRC32 recompression,
              edCompressedSize =
                bool (edCompressedSize ed) ddCompressedSize recompression,
              edUncompressedSize =
                bool (edUncompressedSize ed) ddUncompressedSize recompression
            }
      desc2 =
        desc1
          { edVersionNeeded =
              getZipVersion (needsZip64 desc1) (Just compression)
          }
  hSeek h AbsoluteSeek offset
  B.hPut h (runPut (putHeader LocalHeader s desc2))
  hSeek h AbsoluteSeek afterStreaming
  return (s, desc2)

{- ORMOLU_DISABLE -}

-- | Create a 'Sink' to stream data there. Once streaming is finished,
-- return 'DataDescriptor' for the streamed data. The action /does not/
-- close the given 'Handle'.
sinkData ::
  -- | Opened 'Handle' of zip archive file
  Handle ->
  -- | Compression method to apply
  CompressionMethod ->
  -- | 'Sink' where to stream data
  ConduitT ByteString Void (ResourceT IO) DataDescriptor
sinkData h compression = do
  let sizeSink = CL.fold (\acc input -> fromIntegral (B.length input) + acc) 0
      dataSink =
        getZipSink $
          ZipSink sizeSink <* ZipSink (CB.sinkHandle h)
      withCompression sink =
        getZipSink $
          (,,) <$> ZipSink sizeSink
            <*> ZipSink crc32Sink
            <*> ZipSink sink
  (uncompressedSize, crc32, compressedSize) <-
    case compression of
      Store ->
        withCompression
          dataSink
      Deflate ->
        withCompression $
          Z.compress 9 (Z.WindowBits (-15)) .| dataSink
#ifdef ENABLE_BZIP2
      BZip2 ->
        withCompression $
          BZ.bzip2 .| dataSink
#else
      BZip2 -> throwM (UnsupportedCompressionMethod BZip2)
#endif
#ifdef ENABLE_ZSTD
      Zstd ->
        withCompression $
          Zstandard.compress 1 .| dataSink
#else
      Zstd -> throwM (UnsupportedCompressionMethod Zstd)
#endif
  return
    DataDescriptor
      { ddCRC32 = fromIntegral crc32,
        ddCompressedSize = compressedSize,
        ddUncompressedSize = uncompressedSize
      }

{- ORMOLU_ENABLE -}

-- | Append central directory entries and the end of central directory
-- record to the file that given 'Handle' is associated with. Note that this
-- automatically writes Zip64 end of central directory record and Zip64 end
-- of central directory locator when necessary.
writeCD ::
  -- | Opened handle of zip archive file
  Handle ->
  -- | Commentary to the entire archive
  Maybe Text ->
  -- | Info about already written local headers and entry data
  Map EntrySelector EntryDescription ->
  IO ()
writeCD h comment m = do
  let cd = runPut (putCD m)
  cdOffset <- fromIntegral <$> hTell h
  B.hPut h cd -- write central directory
  let totalCount = fromIntegral (M.size m)
      cdSize = fromIntegral (B.length cd)
      needZip64 =
        totalCount >= ffff
          || cdSize >= ffffffff
          || cdOffset >= ffffffff
  when needZip64 $ do
    zip64ecdOffset <- fromIntegral <$> hTell h
    (B.hPut h . runPut) (putZip64ECD totalCount cdSize cdOffset)
    (B.hPut h . runPut) (putZip64ECDLocator zip64ecdOffset)
  (B.hPut h . runPut) (putECD totalCount cdSize cdOffset comment)

----------------------------------------------------------------------------
-- Binary serialization

-- | Extract the number of bytes between the start of file name in local
-- header and the start of actual data.
getLocalHeaderGap :: Get Integer
getLocalHeaderGap = do
  getSignature 0x04034b50
  skip 2 -- version needed to extract
  skip 2 -- general purpose bit flag
  skip 2 -- compression method
  skip 2 -- last mod file time
  skip 2 -- last mod file date
  skip 4 -- crc-32 check sum
  skip 4 -- compressed size
  skip 4 -- uncompressed size
  fileNameSize <- fromIntegral <$> getWord16le -- file name length
  extraFieldSize <- fromIntegral <$> getWord16le -- extra field length
  return (fileNameSize + extraFieldSize)

-- | Parse central directory file headers and put them into a 'Map'.
getCD :: Get (Map EntrySelector EntryDescription)
getCD = M.fromList . catMaybes <$> many getCDHeader

-- | Parse a single central directory file header. If it's a directory or
-- file compressed with unsupported compression method, 'Nothing' is
-- returned.
getCDHeader :: Get (Maybe (EntrySelector, EntryDescription))
getCDHeader = do
  getSignature 0x02014b50 -- central file header signature
  versionMadeBy <- toVersion <$> getWord16le -- version made by
  versionNeeded <- toVersion <$> getWord16le -- version needed to extract
  when (versionNeeded > zipVersion) . fail $
    "Version required to extract the archive is "
      ++ showVersion versionNeeded
      ++ " (can do "
      ++ showVersion zipVersion
      ++ ")"
  bitFlag <- getWord16le -- general purpose bit flag
  when (any (testBit bitFlag) [0, 6, 13]) . fail $
    "Encrypted archives are not supported"
  let needUnicode = testBit bitFlag 11
  mcompression <- toCompressionMethod <$> getWord16le -- compression method
  modTime <- getWord16le -- last mod file time
  modDate <- getWord16le -- last mod file date
  crc32 <- getWord32le -- CRC32 check sum
  compressed <- fromIntegral <$> getWord32le -- compressed size
  uncompressed <- fromIntegral <$> getWord32le -- uncompressed size
  fileNameSize <- getWord16le -- file name length
  extraFieldSize <- getWord16le -- extra field length
  commentSize <- getWord16le -- file comment size
  skip 4 -- disk number start, internal file attributes
  externalFileAttrs <- getWord32le -- external file attributes
  offset <- fromIntegral <$> getWord32le -- offset of local header
  fileName <-
    decodeText needUnicode
      <$> getBytes (fromIntegral fileNameSize) -- file name
  extraField <-
    M.fromList
      <$> isolate (fromIntegral extraFieldSize) (many getExtraField)
  -- ↑ extra fields in their raw form
  comment <- decodeText needUnicode <$> getBytes (fromIntegral commentSize)
  -- ↑ file comment
  let dfltZip64 =
        Zip64ExtraField
          { z64efUncompressedSize = uncompressed,
            z64efCompressedSize = compressed,
            z64efOffset = offset
          }
      z64ef = case M.lookup 1 extraField of
        Nothing -> dfltZip64
        Just b -> parseZip64ExtraField dfltZip64 b
  case mcompression of
    Nothing -> return Nothing
    Just compression ->
      let desc =
            EntryDescription
              { edVersionMadeBy = versionMadeBy,
                edVersionNeeded = versionNeeded,
                edCompression = compression,
                edModTime = fromMsDosTime (MsDosTime modDate modTime),
                edCRC32 = crc32,
                edCompressedSize = z64efCompressedSize z64ef,
                edUncompressedSize = z64efUncompressedSize z64ef,
                edOffset = z64efOffset z64ef,
                edComment = if commentSize == 0 then Nothing else comment,
                edExtraField = extraField,
                edExternalFileAttrs = externalFileAttrs
              }
       in return $ (,desc) <$> (fileName >>= mkEntrySelector . T.unpack)

-- | Parse an extra-field.
getExtraField :: Get (Word16, ByteString)
getExtraField = do
  header <- getWord16le -- header id
  size <- getWord16le -- data size
  body <- getBytes (fromIntegral size) -- content
  return (header, body)

-- | Get signature. If the extracted data is not equal to the provided
-- signature, fail.
getSignature :: Word32 -> Get ()
getSignature sig = do
  x <- getWord32le -- grab 4-byte signature
  unless (x == sig) . fail $
    "Expected signature " ++ show sig ++ ", but got: " ++ show x

-- | Parse 'Zip64ExtraField' from its binary representation.
parseZip64ExtraField ::
  -- | What is read from central directory file header
  Zip64ExtraField ->
  -- | Actual binary representation
  ByteString ->
  -- | Result
  Zip64ExtraField
parseZip64ExtraField dflt@Zip64ExtraField {..} b =
  either (const dflt) id . flip runGet b $ do
    let ifsat v =
          if v >= ffffffff
            then fromIntegral <$> getWord64le
            else return v
    uncompressed <- ifsat z64efUncompressedSize -- uncompressed size
    compressed <- ifsat z64efCompressedSize -- compressed size
    offset <- ifsat z64efOffset -- offset of local file header
    return (Zip64ExtraField uncompressed compressed offset)

-- | Produce binary representation of 'Zip64ExtraField'.
makeZip64ExtraField ::
  -- | Is this for local or central directory header?
  HeaderType ->
  -- | Zip64 extra field's data
  Zip64ExtraField ->
  -- | Resulting representation
  ByteString
makeZip64ExtraField headerType Zip64ExtraField {..} = runPut $ do
  case headerType of
    LocalHeader -> do
      putWord64le (fromIntegral z64efUncompressedSize) -- uncompressed size
      putWord64le (fromIntegral z64efCompressedSize) -- compressed size
    CentralDirHeader -> do
      when (z64efUncompressedSize >= ffffffff) $
        putWord64le (fromIntegral z64efUncompressedSize) -- uncompressed size
      when (z64efCompressedSize >= ffffffff) $
        putWord64le (fromIntegral z64efCompressedSize) -- compressed size
  when (z64efOffset >= ffffffff) $
    putWord64le (fromIntegral z64efOffset) -- offset of local file header

-- | Create 'ByteString' representing an extra field.
putExtraField :: Map Word16 ByteString -> Put
putExtraField m = forM_ (M.keys m) $ \headerId -> do
  let b = B.take 0xffff (m ! headerId)
  putWord16le headerId
  putWord16le (fromIntegral $ B.length b)
  putByteString b

-- | Create 'ByteString' representing the entire central directory.
putCD :: Map EntrySelector EntryDescription -> Put
putCD m = forM_ (M.keys m) $ \s ->
  putHeader CentralDirHeader s (m ! s)

-- | Create 'ByteString' representing either a local file header or a
-- central directory file header.
putHeader ::
  -- | Type of header to generate
  HeaderType ->
  -- | Name of entry to write
  EntrySelector ->
  -- | Description of entry
  EntryDescription ->
  Put
putHeader headerType s entry@EntryDescription {..} = do
  let isLocalHeader = headerType == LocalHeader
      isCentralDirHeader = headerType == CentralDirHeader
  putWord32le (bool 0x04034b50 0x02014b50 isCentralDirHeader)
  -- ↑ local/central file header signature
  when isCentralDirHeader $
    putWord16le (fromVersion edVersionMadeBy) -- version made by
  putWord16le (fromVersion edVersionNeeded) -- version needed to extract
  let entryName = getEntryName s
      rawName = T.encodeUtf8 entryName
      comment = B.take 0xffff (maybe B.empty T.encodeUtf8 edComment)
      unicode =
        needsUnicode entryName
          || maybe False needsUnicode edComment
      modTime = toMsDosTime edModTime
  putWord16le (if unicode then setBit 0 11 else 0)
  -- ↑ general purpose bit-flag
  putWord16le (fromCompressionMethod edCompression) -- compression method
  putWord16le (msDosTime modTime) -- last mod file time
  putWord16le (msDosDate modTime) -- last mod file date
  putWord32le edCRC32 -- CRC-32 checksum
  putWord32le (withSaturation edCompressedSize) -- compressed size
  putWord32le (withSaturation edUncompressedSize) -- uncompressed size
  putWord16le (fromIntegral $ B.length rawName) -- file name length
  let zip64ef =
        makeZip64ExtraField
          headerType
          Zip64ExtraField
            { z64efUncompressedSize = edUncompressedSize,
              z64efCompressedSize = edCompressedSize,
              z64efOffset = edOffset
            }
      extraField =
        B.take 0xffff . runPut . putExtraField $
          if needsZip64 entry || isLocalHeader
            then M.insert 1 zip64ef edExtraField
            else edExtraField
  putWord16le (fromIntegral $ B.length extraField) -- extra field length
  when isCentralDirHeader $ do
    putWord16le (fromIntegral $ B.length comment) -- file comment length
    putWord16le 0 -- disk number start
    putWord16le 0 -- internal file attributes
    putWord32le edExternalFileAttrs -- external file attributes
    putWord32le (withSaturation edOffset) -- relative offset of local header
  putByteString rawName -- file name (variable size)
  putByteString extraField -- extra field (variable size)
  when isCentralDirHeader (putByteString comment) -- file comment (variable size)

-- | Create 'ByteString' representing Zip64 end of central directory record.
putZip64ECD ::
  -- | Total number of entries
  Natural ->
  -- | Size of the central directory
  Natural ->
  -- | Offset of central directory record
  Natural ->
  Put
putZip64ECD totalCount cdSize cdOffset = do
  putWord32le 0x06064b50 -- zip64 end of central dir signature
  putWord64le 44 -- size of zip64 end of central dir record
  putWord16le (fromVersion zipVersion) -- version made by
  putWord16le (fromVersion $ getZipVersion True Nothing)
  -- ↑ version needed to extract
  putWord32le 0 -- number of this disk
  putWord32le 0 -- number of the disk with the start of the central directory
  putWord64le (fromIntegral totalCount) -- total number of entries (this disk)
  putWord64le (fromIntegral totalCount) -- total number of entries
  putWord64le (fromIntegral cdSize) -- size of the central directory
  putWord64le (fromIntegral cdOffset) -- offset of central directory

-- | Create 'ByteString' representing Zip64 end of the central directory
-- locator.
putZip64ECDLocator ::
  -- | Offset of Zip64 end of central directory
  Natural ->
  Put
putZip64ECDLocator ecdOffset = do
  putWord32le 0x07064b50 -- zip64 end of central dir locator signature
  putWord32le 0 -- number of the disk with the start of the zip64 end of
  -- central directory
  putWord64le (fromIntegral ecdOffset) -- relative offset of the zip64 end
  -- of central directory record
  putWord32le 1 -- total number of disks

-- | Parse end of the central directory record or Zip64 end of the central
-- directory record depending on signature binary data begins with.
getECD :: Get ArchiveDescription
getECD = do
  sig <- getWord32le -- end of central directory signature
  let zip64 = sig == 0x06064b50
  unless (sig == 0x06054b50 || sig == 0x06064b50) $
    fail "Cannot locate end of central directory"
  zip64size <-
    if zip64
      then do
        x <- getWord64le -- size of zip64 end of central directory record
        skip 2 -- version made by
        skip 2 -- version needed to extract
        return (Just x)
      else return Nothing
  thisDisk <- bool (fromIntegral <$> getWord16le) getWord32le zip64
  -- ↑ number of this disk
  cdDisk <- bool (fromIntegral <$> getWord16le) getWord32le zip64
  -- ↑ number of the disk with the start of the central directory
  unless (thisDisk == 0 && cdDisk == 0) $
    fail "No support for multi-disk archives"
  skip (bool 2 8 zip64)
  -- ↑ total number of entries in the central directory on this disk
  skip (bool 2 8 zip64)
  -- ↑ total number of entries in the central directory
  cdSize <- bool (fromIntegral <$> getWord32le) getWord64le zip64
  -- ↑ size of the central directory
  cdOffset <- bool (fromIntegral <$> getWord32le) getWord64le zip64
  -- ↑ offset of start of central directory with respect to the starting
  -- disk number
  when zip64 . skip . fromIntegral $ fromJust zip64size - 4 -- obviously
  commentSize <- getWord16le -- .ZIP file comment length
  comment <- decodeText True <$> getBytes (fromIntegral commentSize)
  -- ↑ archive comment, it's uncertain how we should decide on encoding here
  return
    ArchiveDescription
      { adComment = if commentSize == 0 then Nothing else comment,
        adCDOffset = fromIntegral cdOffset,
        adCDSize = fromIntegral cdSize
      }

-- | Create a 'ByteString' representing the end of central directory record.
putECD ::
  -- | Total number of entries
  Natural ->
  -- | Size of the central directory
  Natural ->
  -- | Offset of central directory record
  Natural ->
  -- | Zip file comment
  Maybe Text ->
  Put
putECD totalCount cdSize cdOffset mcomment = do
  putWord32le 0x06054b50 -- end of central dir signature
  putWord16le 0 -- number of this disk
  putWord16le 0 -- number of the disk with the start of the central directory
  putWord16le (withSaturation totalCount)
  -- ↑ total number of entries on this disk
  putWord16le (withSaturation totalCount) -- total number of entries
  putWord32le (withSaturation cdSize) -- size of central directory
  putWord32le (withSaturation cdOffset) -- offset of start of central directory
  let comment = maybe B.empty T.encodeUtf8 mcomment
  putWord16le (fromIntegral $ B.length comment)
  putByteString comment

-- | Find the absolute offset of the end of central directory record or, if
-- present, Zip64 end of central directory record.
locateECD :: FilePath -> Handle -> IO (Maybe Integer)
locateECD path h = sizeCheck
  where
    sizeCheck = do
      fsize <- hFileSize h
      let limit = max 0 (fsize - 0xffff - 22)
      if fsize < 22
        then return Nothing
        else hSeek h SeekFromEnd (-22) >> loop limit
    loop limit = do
      sig <- getNum getWord32le 4
      pos <- subtract 4 <$> hTell h
      let again = hSeek h AbsoluteSeek (pos - 1) >> loop limit
          done = pos <= limit
      if sig == 0x06054b50
        then do
          result <-
            runMaybeT $
              MaybeT (checkComment pos)
                >>= MaybeT . checkCDSig
                >>= MaybeT . checkZip64
          case result of
            Nothing -> bool again (return Nothing) done
            Just ecd -> return (Just ecd)
        else bool again (return Nothing) done
    checkComment pos = do
      size <- hFileSize h
      hSeek h AbsoluteSeek (pos + 20)
      l <- fromIntegral <$> getNum getWord16le 2
      return $
        if l + 22 == size - pos
          then Just pos
          else Nothing
    checkCDSig pos = do
      hSeek h AbsoluteSeek (pos + 16)
      sigPos <- fromIntegral <$> getNum getWord32le 4
      if sigPos == 0xffffffff -- Zip64 is probably used
        then return (Just pos)
        else do
          hSeek h AbsoluteSeek sigPos
          cdSig <- getNum getWord32le 4
          return $
            if cdSig == 0x02014b50
              ||
              -- ↑ normal case: central directory file header signature
              cdSig == 0x06064b50
              ||
              -- ↑ happens when zip 64 archive is empty
              cdSig == 0x06054b50
              then -- ↑ happens when vanilla archive is empty
                Just pos
              else Nothing
    checkZip64 pos =
      if pos < 20
        then return (Just pos)
        else do
          hSeek h AbsoluteSeek (pos - 20)
          zip64locatorSig <- getNum getWord32le 4
          if zip64locatorSig == 0x07064b50
            then do
              hSeek h AbsoluteSeek (pos - 12)
              Just . fromIntegral <$> getNum getWord64le 8
            else return (Just pos)
    getNum f n = do
      result <- runGet f <$> B.hGet h n
      case result of
        Left msg -> throwM (ParsingFailed path msg)
        Right val -> return val

----------------------------------------------------------------------------
-- Helpers

-- | Rename an entry (key) in a 'Map'.
renameKey :: (Ord k) => k -> k -> Map k a -> Map k a
renameKey ok nk m = case M.lookup ok m of
  Nothing -> m
  Just e -> M.insert nk e (M.delete ok m)

-- | Like 'fromIntegral', but with saturation when converting to bounded
-- types.
withSaturation :: forall a b. (Integral a, Integral b, Bounded b) => a -> b
withSaturation x =
  if (fromIntegral x :: Integer) > (fromIntegral bound :: Integer)
    then bound
    else fromIntegral x
  where
    bound = maxBound :: b

-- | Determine the target entry of an action.
targetEntry :: PendingAction -> Maybe EntrySelector
targetEntry (SinkEntry _ _ s) = Just s
targetEntry (CopyEntry _ _ s) = Just s
targetEntry (RenameEntry s _) = Just s
targetEntry (DeleteEntry s) = Just s
targetEntry (Recompress _ s) = Just s
targetEntry (SetEntryComment _ s) = Just s
targetEntry (DeleteEntryComment s) = Just s
targetEntry (SetModTime _ s) = Just s
targetEntry (AddExtraField _ _ s) = Just s
targetEntry (DeleteExtraField _ s) = Just s
targetEntry (SetExternalFileAttributes _ s) = Just s
targetEntry (SetArchiveComment _) = Nothing
targetEntry DeleteArchiveComment = Nothing

-- | Decode a 'ByteString'. The first argument indicates whether we should
-- treat it as UTF-8 (in case bit 11 of general-purpose bit flag is set),
-- otherwise the function assumes CP437. Note that since not every stream of
-- bytes constitutes valid UTF-8 text, this function can fail. In that case
-- 'Nothing' is returned.
decodeText ::
  -- | Whether bit 11 of general-purpose bit flag is set
  Bool ->
  -- | Binary data to decode
  ByteString ->
  -- | Decoded 'Text' in case of success
  Maybe Text
decodeText False = Just . decodeCP437
decodeText True = either (const Nothing) Just . T.decodeUtf8'

-- | Detect if the given text needs newer Unicode-aware features to be
-- properly encoded in the archive.
needsUnicode :: Text -> Bool
needsUnicode = not . T.all validCP437
  where
    validCP437 x = ord x <= 127

-- | Convert numeric representation (as per the .ZIP specification) of
-- version into 'Version'.
toVersion :: Word16 -> Version
toVersion x = makeVersion [major, minor]
  where
    (major, minor) = quotRem (fromIntegral $ x .&. 0x00ff) 10

-- | Covert 'Version' to its numeric representation as per the .ZIP
-- specification.
fromVersion :: Version -> Word16
fromVersion v = fromIntegral ((ZIP_OS `shiftL` 8) .|. (major * 10 + minor))
  where
    (major, minor) =
      case versionBranch v of
        v0 : v1 : _ -> (v0, v1)
        v0 : _ -> (v0, 0)
        [] -> (0, 0)

-- | Get the compression method form its numeric representation.
toCompressionMethod :: Word16 -> Maybe CompressionMethod
toCompressionMethod 0 = Just Store
toCompressionMethod 8 = Just Deflate
toCompressionMethod 12 = Just BZip2
toCompressionMethod 93 = Just Zstd
toCompressionMethod _ = Nothing

-- | Convert 'CompressionMethod' to its numeric representation as per the
-- .ZIP specification.
fromCompressionMethod :: CompressionMethod -> Word16
fromCompressionMethod Store = 0
fromCompressionMethod Deflate = 8
fromCompressionMethod BZip2 = 12
fromCompressionMethod Zstd = 93

-- | Check if an entry with these parameters needs the Zip64 extension.
needsZip64 :: EntryDescription -> Bool
needsZip64 EntryDescription {..} =
  any
    (>= ffffffff)
    [edOffset, edCompressedSize, edUncompressedSize]

-- | Determine “version needed to extract” that should be written to the
-- headers given the need of the Zip64 feature and the compression method.
getZipVersion :: Bool -> Maybe CompressionMethod -> Version
getZipVersion zip64 m = max zip64ver mver
  where
    zip64ver = makeVersion (if zip64 then [4, 5] else [2, 0])
    mver = makeVersion $ case m of
      Nothing -> [2, 0]
      Just Store -> [2, 0]
      Just Deflate -> [2, 0]
      Just BZip2 -> [4, 6]
      Just Zstd -> [6, 3]

-- | Return a decompressing 'Conduit' corresponding to the given compression
-- method.
decompressingPipe ::
  (PrimMonad m, MonadThrow m, MonadResource m) =>
  CompressionMethod ->
  ConduitT ByteString ByteString m ()
decompressingPipe Store = C.awaitForever C.yield
decompressingPipe Deflate = Z.decompress $ Z.WindowBits (-15)

#ifdef ENABLE_BZIP2
decompressingPipe BZip2 = BZ.bunzip2
#else
decompressingPipe BZip2 = throwM (UnsupportedCompressionMethod BZip2)
#endif

#ifdef ENABLE_ZSTD
decompressingPipe Zstd = Zstandard.decompress
#else
decompressingPipe Zstd = throwM (UnsupportedCompressionMethod Zstd)
#endif

-- | A sink that calculates the CRC32 check sum for an incoming stream.
crc32Sink :: ConduitT ByteString Void (ResourceT IO) Word32
crc32Sink = CL.fold crc32Update 0

-- | Convert 'UTCTime' to the MS-DOS time format.
toMsDosTime :: UTCTime -> MsDosTime
toMsDosTime UTCTime {..} = MsDosTime dosDate dosTime
  where
    dosTime = fromIntegral (seconds + shiftL minutes 5 + shiftL hours 11)
    dosDate = fromIntegral (day + shiftL month 5 + shiftL year 9)
    seconds =
      let (MkFixed x) = todSec tod
       in fromIntegral (x `quot` 2000000000000)
    minutes = todMin tod
    hours = todHour tod
    tod = timeToTimeOfDay utctDayTime
    year = fromIntegral year' - 1980
    (year', month, day) = toGregorian utctDay

-- | Convert MS-DOS date-time to 'UTCTime'.
fromMsDosTime :: MsDosTime -> UTCTime
fromMsDosTime MsDosTime {..} =
  UTCTime
    (fromGregorian year month day)
    (secondsToDiffTime $ hours * 3600 + minutes * 60 + seconds)
  where
    seconds = fromIntegral $ 2 * (msDosTime .&. 0x1f)
    minutes = fromIntegral (shiftR msDosTime 5 .&. 0x3f)
    hours = fromIntegral (shiftR msDosTime 11 .&. 0x1f)
    day = fromIntegral (msDosDate .&. 0x1f)
    month = fromIntegral $ shiftR msDosDate 5 .&. 0x0f
    year = 1980 + fromIntegral (shiftR msDosDate 9)

-- We use the constants of the type 'Natural' instead of literals to protect
-- ourselves from overflows on 32 bit systems.
--
-- If we're in the development mode, use lower values so the tests get a
-- chance to check all cases (otherwise we would need to generate way too
-- big archives on CI).

ffff, ffffffff :: Natural

#ifdef HASKELL_ZIP_DEV_MODE
ffff     = 25
ffffffff = 250
#else
ffff     = 0xffff
ffffffff = 0xffffffff
#endif

-- | The default permissions for the files, permissions not set on Windows,
-- and are set to rw on Unix. This mimics the behavior of the zip utility.
defaultFileMode :: Word32

#ifdef mingw32_HOST_OS
defaultFileMode = 0

#else
defaultFileMode = Unix.fromFileMode 0o600
#endif
