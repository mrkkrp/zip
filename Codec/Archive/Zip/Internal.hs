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
  , commit )
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
import Data.Conduit (Source, Sink, (=$=), ($$), awaitForever, yield)
import Data.Conduit.Internal (zipSinks)
import Data.Digest.CRC32 (crc32Update)
import Data.Foldable (foldl')
import Data.Map.Strict (Map, (!))
import Data.Maybe (fromJust, catMaybes, isNothing)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (><), (|>))
import Data.Serialize
import Data.Text (Text)
import Data.Time
import Data.Version
import Data.Word (Word16, Word32)
import Numeric.Natural (Natural)
import Path
import System.IO
import System.PlanB
import qualified Data.ByteString     as B
import qualified Data.Conduit.BZlib  as BZ
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List   as CL
import qualified Data.Conduit.Zlib   as Z
import qualified Data.Map.Strict     as M
import qualified Data.Sequence       as S
import qualified Data.Set            as E
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T

----------------------------------------------------------------------------
-- Data types

-- | The sum type describes all possible actions that can be performed on
-- archive.

data PendingAction
  = SinkEntry CompressionMethod (Source (ResourceT IO) ByteString) EntrySelector
    -- ^ Add entry given its 'Source'
  | CopyEntry (Path Abs File) EntrySelector EntrySelector
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

-- | Collection of maps describing how to produce entries in resulting
-- archive.

data ProducingActions = ProducingActions
  { paCopyEntry :: Map (Path Abs File) (Map EntrySelector EntrySelector)
  , paSinkEntry :: Map EntrySelector (Source (ResourceT IO) ByteString) }

-- | Collection of editing actions, that is, actions that modify already
-- existing entries.

data EditingActions = EditingActions
  { eaCompression  :: Map EntrySelector CompressionMethod
  , eaEntryComment :: Map EntrySelector Text
  , eaModTime      :: Map EntrySelector UTCTime
  , eaExtraFields  :: Map EntrySelector (Map Natural ByteString) }

-- | Data descriptor representation.

data DataDescriptor = DataDescriptor
  { ddCRC32            :: Natural
  , ddCompressedSize   :: Natural
  , ddUncompressedSize :: Natural }

-- | A temporary data structure to hold Zip64 extra data field information.

data Zip64ExtraField = Zip64ExtraField
  { z64efUncompressedSize :: Natural
  , z64efCompressedSize   :: Natural
  , z64efOffset           :: Natural }

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
  , msDosTime :: Word16 }

----------------------------------------------------------------------------
-- Constants

-- | Version to specify when writing archive data. For now it's a constant.

zipVersion :: Version
zipVersion = Version [4,6] []

----------------------------------------------------------------------------
-- Higher-level operations

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
  mecdOffset <- locateECD path h
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
-- 'EntryDescription', return 'Source' of its uncompressed data.

sourceEntry
  :: Path Abs File     -- ^ Path to archive that contains the entry
  -> EntryDescription  -- ^ Information needed to extract entry of interest
  -> Source (ResourceT IO) ByteString -- ^ Source of uncompressed data
sourceEntry path EntryDescription {..} =
  source =$= CB.isolate (fromIntegral edCompressedSize) =$= decompress
  where
    source = CB.sourceIOHandle $ do
      h <- openFile (toFilePath path) ReadMode
      hSeek h AbsoluteSeek (fromIntegral edOffset)
      localHeader <- B.hGet h 30
      case runGet getLocalHeaderGap localHeader of
        Left msg -> throwM (ParsingFailed path msg)
        Right gap -> do
          hSeek h RelativeSeek gap
          return h
    decompress = case edCompression of
      Store   -> awaitForever yield
      Deflate -> Z.decompress $ Z.WindowBits (-15)
      BZip2   -> BZ.bunzip2

-- | Undertake /all/ actions specified as the fourth argument of the
-- function. This transforms given pending actions so they can be performed
-- in one pass, and then they are performed in the most efficient way.

commit
  :: Path Abs File     -- ^ Location of archive file to edit or create
  -> ArchiveDescription -- ^ Archive description
  -> Map EntrySelector EntryDescription -- ^ Current list of entires
  -> Seq PendingAction -- ^ Collection of pending actions
  -> IO ()
commit path ArchiveDescription {..} entries xs =
  withNewFile (overrideIfExists <> nameTemplate "zip") path $ \temp -> do
    let (ProducingActions coping sinking, editing) =
          optimize (toRecreatingActions path entries >< xs)
        comment = predictComment adComment xs
    withFile (toFilePath temp) WriteMode $ \h -> do
      copiedCD <- M.unions <$> forM (M.keys coping) (\srcPath ->
        copyEntries h srcPath (coping ! srcPath) editing)
      let sinkingKeys = M.keys $ sinking `M.difference` copiedCD
      sunkCD   <- M.fromList <$> forM sinkingKeys (\selector ->
        sinkEntry h selector (sinking ! selector) editing)
      writeCD h comment (copiedCD `M.union` sunkCD)

-- | Determine what comment in new archive will look like given its original
-- value and collection of pending actions.

predictComment :: Maybe Text -> Seq PendingAction -> Maybe Text
predictComment original xs =
  case S.index xs <$> S.findIndexR (isNothing . targetEntry) xs of
    Nothing                      -> original
    Just DeleteArchiveComment    -> Nothing
    Just (SetArchiveComment txt) -> Just txt
    Just _                       -> Nothing

-- | Transform map representing existing entries into collection of actions
-- that re-create those entires.

toRecreatingActions
  :: Path Abs File     -- ^ Name of archive file where entires are found
  -> Map EntrySelector EntryDescription -- ^ Actual list of entires
  -> Seq PendingAction -- ^ Actions that recreate the archive entries
toRecreatingActions path entries = E.foldl' f S.empty (M.keysSet entries)
  where f s e = s |> CopyEntry path e e

-- | Transform collection of 'PendingAction's into 'OptimizedActions' —
-- collection of data describing how to create resulting archive.

optimize
  :: Seq PendingAction -- ^ Collection of pending actions
  -> (ProducingActions, EditingActions) -- ^ Optimized data
optimize = foldl' f
  ( ProducingActions M.empty M.empty
  , EditingActions   M.empty M.empty M.empty M.empty )
  where
    f (pa, ea) a = case a of
      SinkEntry m src s ->
        ( pa { paSinkEntry   = M.insert s src (paSinkEntry pa) }
        , ea { eaCompression = M.insert s m (eaCompression ea) } )
      CopyEntry path os ns ->
        ( pa { paCopyEntry = M.alter (ef os ns) path (paCopyEntry pa)}
        , ea )
      RenameEntry os ns ->
        ( pa { paCopyEntry = M.map (M.map $ re os ns) (paCopyEntry pa)
             , paSinkEntry = renameKey os ns (paSinkEntry pa) }
        , ea )
      DeleteEntry s ->
        ( pa { paSinkEntry = M.delete s (paSinkEntry pa)
             , paCopyEntry = M.map (M.delete s) (paCopyEntry pa) }
        , ea )
      Recompress m s ->
        (pa, ea { eaCompression = M.insert s m (eaCompression ea) })
      SetEntryComment txt s ->
        (pa, ea { eaEntryComment = M.insert s txt (eaEntryComment ea) })
      DeleteEntryComment s ->
        (pa, ea { eaEntryComment = M.delete s (eaEntryComment ea) })
      SetModTime time s ->
        (pa, ea { eaModTime = M.insert s time (eaModTime ea) })
      AddExtraField n b s ->
        (pa, ea { eaExtraFields = M.alter (ef n b) s (eaExtraFields ea) })
      DeleteExtraField n s ->
        (pa, ea { eaExtraFields = M.alter (er n) s (eaExtraFields ea) })
      _ -> (pa, ea)
    re o n x = if x == o then n else x
    ef k v (Just m) = Just (M.insert k v m)
    ef k v Nothing  = Just (M.singleton k v)
    er k (Just m)   = let n = M.delete k m in
      if M.null n then Nothing else Just n
    er _ Nothing    = Nothing

-- | Copy entries from another archive and write them into file associated
-- with given handle. This actually can throw 'EntryDoesNotExist' if there
-- is no such entry in that archive.

copyEntries
  :: Handle            -- ^ Opened 'Handle' of zip archive file
  -> Path Abs File     -- ^ Path to file from which to copy the entries
  -> Map EntrySelector EntrySelector
     -- ^ 'Map' from original name to name to use in new archive
  -> EditingActions    -- ^ Additional info that can influence result
  -> IO (Map EntrySelector EntryDescription)
     -- ^ Info to generate central directory file headers later
copyEntries h path m e = do
  entries <- snd <$> scanArchive path
  done    <- forM (M.keys m) $ \s ->
    case s `M.lookup` entries of
      Nothing -> throwM (EntryDoesNotExist path s)
      Just desc -> sinkEntry h (m ! s) (sourceEntry path desc) e
  return (M.fromList done)

-- | Sink entry from given stream into file associated with given 'Handle'.

sinkEntry
  :: Handle            -- ^ Opened 'Handle' of zip archive file
  -> EntrySelector     -- ^ Name of entry to add
  -> Source (ResourceT IO) ByteString -- ^ Source of entry contents
  -> EditingActions    -- ^ Additional info that can influence result
  -> IO (EntrySelector, EntryDescription)
     -- ^ Info to generate central directory file headers later
sinkEntry h s src EditingActions {..} = do
  modTime <- getCurrentTime
  offset  <- hTell h
  let compression = M.findWithDefault Store s eaCompression
      extraField  = M.findWithDefault M.empty s eaExtraFields
      desc' = EntryDescription -- to write in local header
        { edVersionMadeBy    = zipVersion
        , edVersionNeeded    = zipVersion
        , edCompression      = compression
        , edModified         = M.findWithDefault modTime s eaModTime
        , edCRC32            = 0 -- to be overwritten after streaming
        , edCompressedSize   = 0 -- ↑
        , edUncompressedSize = 0 -- ↑
        , edOffset           = fromIntegral offset
        , edComment          = M.lookup s eaEntryComment
        , edExtraField       =
            M.insert 1 (B.replicate 28 0x00) extraField }
      lhSize = predictLocalHeaderLength s desc'
  B.hPut h (B.replicate lhSize 0x00)
  DataDescriptor {..} <- runResourceT (src $$ sinkData h compression)
  afterStreaming <- hTell h
  let zip64 = Zip64ExtraField
        { z64efUncompressedSize = ddUncompressedSize
        , z64efCompressedSize   = ddCompressedSize
        , z64efOffset           = fromIntegral offset }
      desc = desc'
        { edCRC32            = ddCRC32
        , edCompressedSize   = ddCompressedSize
        , edUncompressedSize = ddUncompressedSize
        , edExtraField       =
            M.insert 1 (makeZip64ExtraField zip64) extraField }
  hSeek h AbsoluteSeek offset
  B.hPut h (runPut (putLocalHeader s desc))
  hSeek h AbsoluteSeek afterStreaming
  return (s, desc)

-- | Create 'Sink' to stream data there. Once streaming is finished, return
-- 'DataDescriptor' for the streamed data. The action /does not/ close given
-- 'Handle'.

sinkData
  :: Handle            -- ^ Opened 'Handle' of zip archive file
  -> CompressionMethod -- ^ Compression method to apply
  -> Sink ByteString (ResourceT IO) DataDescriptor
     -- ^ 'Sink' where to stream data
sinkData h compression = do
  let crc32Sink = CL.fold crc32Update 0
      sizeSink  = CL.fold (\acc input -> B.length input + acc) 0
      dataSink  = fst <$> zipSinks sizeSink (CB.sinkHandle h)
      withCompression = zipSinks (zipSinks sizeSink crc32Sink)
  ((uncompressedSize, crc32), compressedSize) <-
    case compression of
      Store   -> withCompression
        dataSink
      Deflate -> withCompression $
        Z.compress 9 (Z.WindowBits (-15)) =$= dataSink
      BZip2   -> withCompression $
        BZ.bzip2 =$= dataSink
  return DataDescriptor
    { ddCRC32            = fromIntegral crc32
    , ddCompressedSize   = fromIntegral compressedSize
    , ddUncompressedSize = fromIntegral uncompressedSize }

-- | Append central directory entries and end of central directory record to
-- file that given 'Handle' is associated with. Note that this automatically
-- writes Zip64 end of central directory record and Zip64 end of central
-- directory locator when necessary.

writeCD
  :: Handle            -- ^ Opened handle of zip archive file
  -> Maybe Text        -- ^ Commentary to entire archive
  -> Map EntrySelector EntryDescription
  -- ^ Info about already written local headers and entry data
  -> IO ()
writeCD h comment m = do
  let cd = runPut (putCD m)
  cdOffset <- hTell h
  B.hPut h cd -- write central directory
  let totalCount = M.size m
      cdSize     = B.length cd
      needZip64  = totalCount >= 0xffff
        || cdSize   >= 0xffffffff
        || cdOffset >= 0xffffffff
  when needZip64 $ do
    zip64ecdOffset <- hTell h
    B.hPut h . runPut $ putZip64ECD totalCount cdSize cdOffset
    B.hPut h . runPut $ putZip64ECDLocator zip64ecdOffset
  B.hPut h . runPut $ putECD
    (bool totalCount 0xffff     needZip64)
    (bool cdSize     0xffffffff needZip64)
    (bool cdOffset   0xffffffff needZip64)
    comment

----------------------------------------------------------------------------
-- Binary serialization

-- | Extract number of bytes between start of file name in local header and
-- start of actual data.

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
  fileNameSize   <- fromIntegral <$> getWord16le -- file name length
  extraFieldSize <- fromIntegral <$> getWord16le -- extra field length
  return (fileNameSize + extraFieldSize)

-- | Create 'ByteString' representing local file header.

putLocalHeader :: EntrySelector -> EntryDescription -> Put
putLocalHeader = putHeader False

-- | Parse central directory file headers and put them into 'Map'.

getCD :: Get (Map EntrySelector EntryDescription)
getCD = M.fromList . catMaybes <$> many getCDHeader

-- | Parse single central directory file header. If it's a directory or file
-- compressed with unsupported compression method, 'Nothing' is returned.

getCDHeader :: Get (Maybe (EntrySelector, EntryDescription))
getCDHeader = do
  getSignature 0x02014b50 -- central file header signature
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
  skip 8 -- disk number start, internal/external file attributes
  offset         <- fromIntegral <$> getWord32le -- offset of local header
  fileName       <- T.unpack . T.decodeUtf8 <$>
    getBytes (fromIntegral fileNameSize) -- file name
  extraFields    <- M.fromList <$>
    isolate (fromIntegral extraFieldSize) (many getExtraField)
  -- ↑ extra fields in their raw form
  comment <- T.decodeUtf8 <$> getBytes (fromIntegral commentSize)
  -- ↑ file comment
  let mz64ef = M.lookup 1 extraFields >>= parseZip64ExtraField
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
            , edExtraField       = extraFields }
      in return $ (,desc) <$> (parseRelFile fileName >>= mkEntrySelector)

-- | Parse an extra-field.

getExtraField :: Get (Natural, ByteString)
getExtraField = do
  header <- getWord16le -- header id
  size   <- getWord16le -- data size
  body   <- getBytes (fromIntegral size) -- content
  return (fromIntegral header, body)

-- | Get signature. If extracted data is not equal to provided signature,
-- fail.

getSignature :: Word32 -> Get ()
getSignature sig = do
  x <- getWord32le -- grab 4-byte signature
  unless (x == sig) . fail $
    "Expected signature " ++ show sig ++ ", but got: " ++ show x

-- | Parse 'Zip64ExtraField' from its binary representation.

parseZip64ExtraField :: ByteString -> Maybe Zip64ExtraField
parseZip64ExtraField b = either (const Nothing) Just . flip runGet b $ do
  uncompressed <- fromIntegral <$> getWord64le -- uncompressed size
  compressed   <- fromIntegral <$> getWord64le -- compressed size
  offset       <- fromIntegral <$> getWord64le -- offset of local file header
  skip 4 -- number of the disk on which this file starts
  return (Zip64ExtraField uncompressed compressed offset)

-- | Produce binary representation of 'Zip64ExtraField', 28 bytes long.

makeZip64ExtraField :: Zip64ExtraField -> ByteString
makeZip64ExtraField Zip64ExtraField {..} = runPut $ do
  putWord64le (fromIntegral z64efUncompressedSize) -- uncompressed size
  putWord64le (fromIntegral z64efCompressedSize) -- compressed size
  putWord64le (fromIntegral z64efOffset) -- offset of local file header
  putWord32le 0 -- number of the disk on which this file starts

-- | Create 'ByteString' representing an extra field.

putExtraField :: Map Natural ByteString -> Put
putExtraField m = forM_ (M.keys m) $ \headerId -> do
  let b = m ! headerId
  putWord16le (fromIntegral headerId)
  putWord16le (fromIntegral $ B.length b)
  putByteString b

-- | Create 'ByteString' representing entire central directory.

putCD :: Map EntrySelector EntryDescription -> Put
putCD m = forM_ (M.keys m) $ \s ->
  putCDHeader s (m ! s)

-- | Create 'ByteString' representing central directory file header.

putCDHeader :: EntrySelector -> EntryDescription -> Put
putCDHeader = putHeader True

-- | Create 'ByteString' representing local file header if the first
-- argument is 'False' and central directory file header otherwise.

putHeader
  :: Bool              -- ^ Generate central directory file header
  -> EntrySelector     -- ^ Name of entry to write
  -> EntryDescription  -- ^ Description of entry
  -> Put
putHeader c s EntryDescription {..} = do
  putWord32le (bool 0x04034b50 0x02014b50 c)
  -- ↑ local/central file header signature
  when c $
    putWord16le (fromVersion edVersionMadeBy) -- version made by
  putWord16le (fromVersion edVersionNeeded) -- version needed to extract
  let entryName = getEntryName s
      rawName   = T.encodeUtf8 entryName
      comment   = maybe B.empty T.encodeUtf8 edComment
      unicode   = needsUnicode entryName
        || maybe False needsUnicode edComment
      modTime   = toMsDosTime edModified
  putWord16le (if unicode then 0x0000 else 0x0800)
  -- ↑ general purpose bit-flag
  putWord16le (fromCompressionMethod edCompression) -- compression method
  putWord16le (msDosTime modTime) -- last mod file time
  putWord16le (msDosDate modTime) -- last mod file date
  putWord32le (fromIntegral edCRC32) -- CRC-32 checksum
  putWord32le (fromIntegral edCompressedSize) -- compressed size
  putWord32le (fromIntegral edUncompressedSize) -- uncompressed size
  putWord16le (fromIntegral $ B.length rawName) -- file name length
  putWord16le (fromIntegral $ predictExtraFieldLength edExtraField)
  -- ↑ extra field length
  when c $ do
    putWord16le (fromIntegral $ B.length comment) -- file comment length
    putWord16le 0 -- disk number start
    putWord16le 0 -- internal file attributes
    putWord32le 0 -- external file attributes
    putWord32le (fromIntegral edOffset) -- relative offset of local header
  putByteString rawName -- file name (variable size)
  putExtraField edExtraField -- extra field (variable size)
  when c (putByteString comment) -- file comment (variable size)

-- | Create 'ByteString' representing Zip64 end of central directory record.

putZip64ECD
  :: Int               -- ^ Total number of entries
  -> Int               -- ^ Size of the central directory
  -> Integer           -- ^ Offset of central directory record
  -> Put
putZip64ECD totalCount cdSize cdOffset = do
  putWord32le 0x06064b50 -- zip64 end of central dir signature
  putWord64le 44 -- size of zip64 end of central dir record
  putWord16le (fromVersion zipVersion) -- version made by
  putWord16le (fromVersion zipVersion) -- version needed to extract
  putWord32le 0 -- number of this disk
  putWord32le 0 -- number of the disk with the start of the central directory
  putWord64le (fromIntegral totalCount) -- total number of entries (this disk)
  putWord64le (fromIntegral totalCount) -- total number of entries
  putWord64le (fromIntegral cdSize) -- size of the central directory
  putWord64le (fromIntegral cdOffset) -- offset of central directory

-- | Create 'ByteString' representing Zip64 end of central directory
-- locator.

putZip64ECDLocator
  :: Integer           -- ^ Offset of Zip64 end of central directory
  -> Put
putZip64ECDLocator ecdOffset = do
  putWord32le 0x07064b50 -- zip64 end of central dir locator signature
  putWord32le 0 -- number of the disk with the start of the zip64 end of
    -- central directory
  putWord64le (fromIntegral ecdOffset) -- relative offset of the zip64 end
    -- of central directory record
  putWord32le 1 -- total number of disks

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

-- | Create 'ByteString' representing end of central directory record.

putECD
  :: Int               -- ^ Total number of entries
  -> Int               -- ^ Size of the central directory
  -> Integer           -- ^ Offset of central directory record
  -> Maybe Text        -- ^ Zip file comment
  -> Put
putECD totalCount cdSize cdOffset mcomment = do
  putWord32le 0x06054b50 -- end of central dir signature
  putWord16le 0 -- number of this disk
  putWord16le 0 -- number of the disk with the start of the central directory
  putWord16le (fromIntegral totalCount) -- total number of entries on this disk
  putWord16le (fromIntegral totalCount) -- total number of entries
  putWord32le (fromIntegral cdSize) -- size of central directory
  putWord32le (fromIntegral cdOffset) -- offset of start of central directory
  let comment = maybe B.empty T.encodeUtf8 mcomment
  putWord16le (fromIntegral $ B.length comment)
  putByteString comment

-- | Find absolute offset of end of central directory record or, if present,
-- Zip64 end of central directory record.

locateECD :: Path Abs File -> Handle -> IO (Maybe Integer)
locateECD path h = sizeCheck
  where

    sizeCheck = do
      tooSmall <- (< 22) <$> hFileSize h
      if tooSmall
        then return Nothing
        else hSeek h SeekFromEnd (-22) >> loop

    loop = do
      sig <- getNum getWord32le 4
      pos <- subtract 4 <$> hTell h
      let again = hSeek h AbsoluteSeek (pos - 1) >> loop
          done  = pos == 0
      if sig == 0x06054b50
        then do
          result <- checkComment pos >>+ checkCDSig >>+ checkZip64
          case result of
            Nothing -> bool again (return Nothing) done
            Just ecd -> return (Just ecd)
        else bool again (return Nothing) done

    checkComment pos = do
      size <- hFileSize h
      hSeek h AbsoluteSeek (pos + 20)
      l <- fromIntegral <$> getNum getWord16le 2
      return $ if l + 22 == size - pos
        then Just pos
        else Nothing

    checkCDSig pos = do
      hSeek h AbsoluteSeek (pos + 16)
      sigPos <- fromIntegral <$> getNum getWord32le 4
      hSeek h AbsoluteSeek sigPos
      cdSig  <- getNum getWord32le 4
      return $ if cdSig == 0x02014b50 || cdSig == 0x06054b50
        then Just pos
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

-- | Chain 'Maybe' monad inside 'IO' monad.

infixl 1 >>+

(>>+) :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
a >>+ b = a >>= maybe (return Nothing) b

-- | Rename entry (key) in a 'Map'.

renameKey :: Ord k => k -> k -> Map k a -> Map k a
renameKey ok nk m = case M.lookup ok m of
  Nothing -> m
  Just e -> M.insert nk e (M.delete ok m)

-- | Return length of local header.

predictLocalHeaderLength :: EntrySelector -> EntryDescription -> Int
predictLocalHeaderLength s e = 30
  + B.length (T.encodeUtf8 $ getEntryName s)
  + predictExtraFieldLength (edExtraField e)

-- | Return number of bytes that given collection of extra fields will
-- occupy.

predictExtraFieldLength :: Map Natural ByteString -> Int
predictExtraFieldLength = M.foldl' f 0
  where f t b = 4 + B.length b + t

-- | Determine target entry of action.

targetEntry :: PendingAction -> Maybe EntrySelector
targetEntry (SinkEntry      _ _ s) = Just s
targetEntry (CopyEntry      _ _ s) = Just s
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

-- | Covert 'CompressionMethod' to its numeric representation as per .ZIP
-- specification.

fromCompressionMethod :: CompressionMethod -> Word16
fromCompressionMethod Store   = 0
fromCompressionMethod Deflate = 8
fromCompressionMethod BZip2   = 12

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
    seconds = fromIntegral $ 2 * (msDosTime .&. 0x1f)
    minutes = fromIntegral $ shiftR msDosTime 5 .&. 0x3f
    hours   = fromIntegral (shiftR msDosTime 11)

    day     = fromIntegral (msDosDate .&. 0x1f)
    month   = fromIntegral $ shiftR msDosDate 5 .&. 0xf
    year    = 1980 + fromIntegral (shiftR msDosDate 9)
