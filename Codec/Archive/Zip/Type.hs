-- |
-- Module      :  Codec.Archive.Zip.Type
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the package. You don't usually need to import this module,
-- because "Codec.Archive.Zip" re-exports everything you may need, import
-- that module instead.

{-# LANGUAGE DeriveDataTypeable #-}

module Codec.Archive.Zip.Type
  ( -- * Entry selector
    EntrySelector
  , mkEntrySelector
  , unEntrySelector
  , getEntryName
  , EntrySelectorException
    -- * Entry description
  , EntryDescription (..)
  , CompressionMethod (..)
    -- * Archive desrciption
  , ArchiveDescription (..)
    -- * Exceptions
  , ZipException (..) )
where

import Control.Arrow ((>>>))
import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (mapMaybe, fromJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Numeric.Natural
import Path
import qualified Data.CaseInsensitive    as CI
import qualified Data.List.NonEmpty      as NE
import qualified Data.Text               as T
import qualified System.FilePath         as FP
import qualified System.FilePath.Posix   as Posix
import qualified System.FilePath.Windows as Windows

----------------------------------------------------------------------------
-- Entry selector

-- | This data type serves for naming and selection of archive
-- entries. It can be created only with help of smart constructor
-- 'mkEntrySelector', and it's the only “key” that can be used to select
-- files in archive or to name new files.
--
-- The abstraction is crucial for ensuring that created archives are
-- portable across operating systems, file systems, and different
-- platforms. Since on some operating systems, file paths are
-- case-insensitive, this selector is also case-insensitive. It makes sure
-- that only relative paths are used to name files inside archive, as it's
-- recommended in the specification. It also guarantees that forward slashes
-- are used when the path is stored inside archive for compatibility with
-- Unix-like operating systems (as it is recommended in the
-- specification). On the other hand, in can be rendered as ordinary
-- relative file path in OS-specific format, when needed.

newtype EntrySelector = EntrySelector
  { unES :: NonEmpty (CI String)
    -- ^ Path pieces of relative path inside archive
  } deriving (Eq, Ord)

instance Show EntrySelector where
  show = show . unEntrySelector

-- | Create 'EntrySelector' from 'Path Rel File'. To avoid problems with
-- distribution of the archive, characters that some operating systems do
-- not expect in paths are not allowed. Proper paths should pass these
-- checks:
--
--     * 'System.FilePath.Posix.isValid'
--     * 'System.FilePath.Windows.isValid'
--
-- This function can throw 'EntrySelectorException' exception.

mkEntrySelector :: MonadThrow m => Path Rel File -> m EntrySelector
mkEntrySelector path =
  let fp           = toFilePath path
      g x          = if null x then Nothing else Just (CI.mk x)
      preparePiece = g . filter (not . FP.isPathSeparator)
      pieces       = mapMaybe preparePiece (FP.splitPath fp)
  in if Posix.isValid fp && Windows.isValid fp && not (null pieces)
       then (return . EntrySelector . NE.fromList) pieces
       else throwM (InvalidEntrySelector path)

-- | Make a relative path from 'EntrySelector'. Every 'EntrySelector'
-- produces single 'Path Rel File' that corresponds to it.

unEntrySelector :: EntrySelector -> Path Rel File
unEntrySelector = unES
  >>> NE.toList
  >>> fmap CI.original
  >>> FP.joinPath
  >>> parseRelFile
  >>> fromJust

-- | Get entry name given 'EntrySelector' in from that is suitable for
-- writing to file header.

getEntryName :: EntrySelector -> Text
getEntryName = unES
  >>> fmap CI.original
  >>> NE.intersperse "/"
  >>> NE.toList
  >>> concat
  >>> T.pack

-- | Exception describing various troubles you can have with
-- 'EntrySelector'.

data EntrySelectorException
  = InvalidEntrySelector (Path Rel File)
    -- ^ Selector cannot be created from this path
  deriving (Typeable)

instance Show EntrySelectorException where
  show (InvalidEntrySelector path) = "Cannot build selector from " ++ show path

instance Exception EntrySelectorException

----------------------------------------------------------------------------
-- Entry description

-- | This record represents all information about archive entry that can be
-- stored in a .ZIP archive. It does not mirror local file header or central
-- directory file header, but their binary representation can be built given
-- this date structure and actual archive contents.

data EntryDescription = EntryDescription
  { edVersionMadeBy    :: Version -- ^ Version made by
  , edVersionNeeded    :: Version -- ^ Version needed to extract
  , edCompression      :: CompressionMethod -- ^ Compression method
  , edModified         :: UTCTime -- ^ Last modification date and time
  , edCRC32            :: Natural -- ^ CRC32 check sum
  , edCompressedSize   :: Natural -- ^ Size of compressed entry
  , edUncompressedSize :: Natural -- ^ Size of uncompressed entry
  , edOffset           :: Natural -- ^ Absolute offset of local file header
  , edComment          :: Maybe Text -- ^ Entry comment
  , edExtraFields      :: Map Natural ByteString -- ^ All extra fields found
  } deriving (Eq)

-- | Supported compression methods.

data CompressionMethod
  = Store              -- ^ Store file uncompressed
  | Deflate            -- ^ Deflate
  | BZip2              -- ^ Compressed using BZip2 algorithm
    deriving (Eq, Enum, Read, Show)

----------------------------------------------------------------------------
-- Archive description

-- | Information about archive as a whole.

data ArchiveDescription = ArchiveDescription
  { adComment  :: Maybe Text -- ^ Comment of entire archive
  , adCDOffset :: Natural -- ^ Absolute offset to start of central directory
  , adCDSize   :: Natural -- ^ Size of central directory record
  } deriving (Eq, Show)

----------------------------------------------------------------------------
-- Exceptions

-- | Bad things that can happen when you use the library.

data ZipException
  = EntryDoesNotExist       (Path Abs File) EntrySelector
  | EntryAlreadyExists      (Path Abs File) EntrySelector
  | ExtraFieldDoesNotExist  (Path Abs File) EntrySelector Natural
  | ExtraFieldAlreadyExists (Path Abs File) EntrySelector Natural
  | ParsingFailed           (Path Abs File) String
  deriving (Typeable)

instance Show ZipException where
  show (EntryDoesNotExist file s) =
    "No such entry found: " ++ show s ++ " in " ++ show file
  show (EntryAlreadyExists file s) =
    "Entry already exists: " ++ show s ++ " in " ++ show file
  show (ExtraFieldDoesNotExist file s n) =
    "No such extra field: " ++ show s ++ " " ++ show n ++ " in " ++ show file
  show (ExtraFieldAlreadyExists file s n) =
    "Extra field already exists: " ++
    show s ++ " " ++ show n ++ " in " ++ show file
  show (ParsingFailed file msg) =
    "Parsing of archive structure failed: \n" ++ msg ++ "\nin " ++ show file

instance Exception ZipException
