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
  , sourceEntry
  , withOptimizedActions
  , commit
  )
where

import Codec.Archive.Zip.Type
import Control.Applicative (many)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Bits
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Conduit (Source, Sink, ($=), ($$))
import Data.Map.Strict (Map)
import Data.Maybe (fromJust, catMaybes)
import Data.Sequence (Seq)
import Data.Serialize
import Data.Text (Text)
import Data.Time
import Data.Version
import Data.Word (Word16)
import Numeric.Natural (Natural)
import Path
import System.IO
import qualified Data.ByteString     as B
import qualified Data.Conduit.BZlib  as BZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.Zlib   as Z
import qualified Data.Map.Strict     as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T

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
  | AddExtraField Natural ByteString EntrySelector
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
targetEntry (AddExtraField  _ _ s) = Just s
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
scanArchive path = withFile (toFilePath path) ReadMode $ \h -> do
  mecdOffset <- locateECD h
  case mecdOffset of
    Just ecdOffset -> do
      hSeek h AbsoluteSeek ecdOffset
      ecdSize <- subtract ecdOffset <$> hFileSize h
      ecdRaw  <- B.hGet h (fromIntegral ecdSize)
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
-- 'EntryDescription', stream its contents to given 'Sink'. Returned
-- data is uncompressed.

sourceEntry
  :: Path Abs File     -- ^ Path to archive that contains the entry
  -> EntryDescription  -- ^ Information needed to extract entry of interest
  -> Sink ByteString (ResourceT IO) a -- ^ Where to stream uncompressed data
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
withOptimizedActions = undefined -- TODO

-- | Undertake /all/ actions specified in the second argument of the
-- function.

commit
  :: Path Abs File     -- ^ Where to write the new archive
  -> Seq PendingAction -- ^ Collection of actions (should be optimized)
  -> IO ()
commit = undefined -- TODO

----------------------------------------------------------------------------
-- Binary serialization

-- | Parse end of central directory record or Zip64 end of central directory
-- record depending on signature binary data begins with.

getECD :: Get ArchiveDescription
getECD = do
  sig <- getWord32le -- end of central directory signature
  let zip64 = sig == 0x06064b50
  unless (sig == 0x06054b50 || sig == 0x06064b50) $
    fail "Cannot locate end of central directory"
  zip64size <- if zip64 then do
    x <- getWord64le -- size of zip64 end of central directory record
    skip 2 -- version made by
    skip 2 -- version needed to extract
    return (Just x)
    else return Nothing
  thisDisk <- bool (fromIntegral <$> getWord16le) getWord32le zip64
  -- ↑ number of this disk
  cdDisk   <- bool (fromIntegral <$> getWord16le) getWord32le zip64
  -- ↑ number of the disk with the start of the central directory
  unless (thisDisk == 0 && cdDisk == 0) $
    fail "No support for multi-disk archives"
  skip (bool 2 8 zip64)
  -- ↑ total number of entries in the central directory on this disk
  skip (bool 2 8 zip64)
  -- ↑ total number of entries in the central directory
  cdSize   <- bool (fromIntegral <$> getWord32le) getWord64le zip64
  -- ↑ size of the central directory
  cdOffset <- bool (fromIntegral <$> getWord32le) getWord64le zip64
  -- ↑ offset of start of central directory with respect to the starting
  -- disk number
  when zip64 . skip . fromIntegral $ fromJust zip64size - 4 -- obviously
  commentSize <- getWord16le -- .ZIP file comment length
  comment <- T.decodeUtf8 <$> getBytes (fromIntegral commentSize)
  -- ↑ .ZIP file comment
  return ArchiveDescription
    { adComment  = if commentSize == 0 then Nothing else Just comment
    , adCDOffset = fromIntegral cdOffset
    , adCDSize   = fromIntegral cdSize }

-- | Parse central directory file headers and put them into 'Map'.

getCD :: Get (Map EntrySelector EntryDescription)
getCD = M.fromList . catMaybes <$> many getCDEntry

-- | Parse single central directory file header. If it's a directory or file
-- compressed with unsupported compression method, 'Nothing' is returned.

getCDEntry :: Get (Maybe (EntrySelector, EntryDescription))
getCDEntry = do
  sig <- getWord32le -- central file header signature
  unless (sig == 0x02014b50) $
    fail "Expected central directory file header signature"
  versionMadeBy  <- toVersion <$> getWord16le -- version made by
  versionNeeded  <- toVersion <$> getWord16le -- version needed to extract
  bitFlag        <- getWord16le -- general purpose bit flag
  when (any (testBit bitFlag) [0,6,13]) $
    fail "Encrypted archives are not supported"
  mcompression   <- toCompressionMethod <$> getWord16le -- compression method
  modTime        <- getWord16le -- last mod file time
  modDate        <- getWord16le -- last mod file date
  crc32          <- getWord32le -- CRC32 check sum
  compressed     <- fromIntegral <$> getWord32le -- compressed size
  uncompressed   <- fromIntegral <$> getWord32le -- uncompressed size
  fileNameSize   <- getWord16le -- file name length
  extraFieldSize <- getWord16le -- extra field length
  commentSize    <- getWord16le -- file comment size
  skip 2 -- disk number start
  skip 2 -- internal file attributes
  skip 4 -- external file attributes
  offset         <- fromIntegral <$> getWord32le -- offset of local header
  fileName       <- T.unpack . T.decodeUtf8 <$>
    getBytes (fromIntegral fileNameSize) -- file name
  extraFields    <- M.fromList <$>
    isolate (fromIntegral extraFieldSize) (many getExtraField)
  -- ↑ extra fields in their raw form
  comment <- T.decodeUtf8 <$> getBytes (fromIntegral commentSize)
  -- ↑ file comment
  let mz64ef = M.lookup 1 extraFields >>= parseZip64ExtraFiled
      with64opt x f =
        case mz64ef of
          Nothing -> x
          Just z64ef -> bool x (f z64ef) (x == 0xffffffff)
  case mcompression of
    Nothing -> return Nothing
    Just compression ->
      let desc = EntryDescription
            { edVersionMadeBy    = versionMadeBy
            , edVersionNeeded    = versionNeeded
            , edCompression      = compression
            , edModified         = fromMsDosTime (MsDosTime modDate modTime)
            , edCRC32            = fromIntegral crc32
            , edCompressedSize   = with64opt compressed   z64efCompressedSize
            , edUncompressedSize = with64opt uncompressed z64efUncompressedSize
            , edOffset           = with64opt offset       z64efOffset
            , edComment = if commentSize == 0 then Nothing else Just comment
            , edExtraFields      = extraFields }
      in return $ (,desc) <$> (parseRelFile fileName >>= mkEntrySelector)

-- | Parse an extra-field.

getExtraField :: Get (Natural, ByteString)
getExtraField = do
  header <- getWord16le -- header id
  size   <- getWord16le -- data size
  body   <- getBytes (fromIntegral size) -- content
  return (fromIntegral header, body)

-- | Find absolute offset of end of central directory record or, if present,
-- Zip64 end of central directory record.

locateECD :: Handle -> IO (Maybe Integer)
locateECD = fmap (Just . subtract 22) . hFileSize -- FIXME

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

-- | Convert numeric representation (as per .ZIP specification) of version
-- into 'Version'.

toVersion :: Word16 -> Version
toVersion x = makeVersion [major, minor]
  where (major, minor) = quotRem (fromIntegral $ x .&. 0x00ff) 10

-- | Covert 'Version' to its numeric representation as per .ZIP
-- specification.

fromVersion :: Version -> Word16
fromVersion v = fromIntegral (major * 10 + minor)
  where (major:minor:_) = versionBranch v ++ repeat 0

-- | Get compression method form its numeric representation.

toCompressionMethod :: Word16 -> Maybe CompressionMethod
toCompressionMethod 0  = Just Store
toCompressionMethod 8  = Just Deflate
toCompressionMethod 12 = Just BZip2
toCompressionMethod _  = Nothing

-- | A temporary data structure to hold Zip64 extra data field information.

data Zip64ExtraField = Zip64ExtraField
  { z64efUncompressedSize :: Natural
  , z64efCompressedSize   :: Natural
  , z64efOffset           :: Natural }

-- | Parse 'Zip64ExtraField' from its binary representation.

parseZip64ExtraFiled :: ByteString -> Maybe Zip64ExtraField
parseZip64ExtraFiled b = either (const Nothing) Just . flip runGet b $ do
  uncompressed <- fromIntegral <$> getWord64le -- uncompressed size
  compressed   <- fromIntegral <$> getWord64le -- compressed size
  offset       <- fromIntegral <$> getWord64le -- offset of local file header
  return (Zip64ExtraField uncompressed compressed offset)

-- | MS-DOS date-time: a pair of Word16s (date, time) with the following
-- structure:
--
-- > DATE bit     0 - 4           5 - 8           9 - 15
-- >      value   day (1 - 31)    month (1 - 12)  years from 1980
-- > TIME bit     0 - 4           5 - 10          11 - 15
-- >      value   seconds*        minute          hour
-- >              *stored in two-second increments

data MsDosTime = MsDosTime
  { msDosDate :: Word16
  , msDosTime :: Word16
  } deriving (Read, Show, Eq)

-- | Convert 'UTCTime' to MS-DOS time format.

toMsDosTime :: UTCTime -> MsDosTime
toMsDosTime UTCTime {..} = MsDosTime dosDate dosTime
  where
    dosTime = fromIntegral (seconds + shiftL minutes 5 + shiftL hours 11)
    dosDate = fromIntegral (day + shiftL month 5 + shiftL year 9)

    seconds = fromEnum (todSec tod) `div` 2
    minutes = todMin tod
    hours   = todHour tod
    tod     = timeToTimeOfDay utctDayTime

    year    = fromIntegral year' - 1980
    (year', month, day) = toGregorian utctDay

-- | Convert MS-DOS date-time to 'UTCTime'.

fromMsDosTime :: MsDosTime -> UTCTime
fromMsDosTime MsDosTime {..} = UTCTime
  (fromGregorian year month day)
  (secondsToDiffTime $ hours * 60 * 60 + minutes * 60 + seconds)
  where
    seconds = fromIntegral $ 2 * (msDosTime .&. 0x1F)
    minutes = fromIntegral $ shiftR msDosTime 5 .&. 0x3F
    hours   = fromIntegral (shiftR msDosTime 11)

    day     = fromIntegral (msDosDate .&. 0x1F)
    month   = fromIntegral $ shiftR msDosDate 5 .&. 0xF
    year    = 1980 + fromIntegral (shiftR msDosDate 9)
