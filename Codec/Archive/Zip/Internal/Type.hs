{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  Codec.Archive.Zip.Internal.Type
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the package.
module Codec.Archive.Zip.Internal.Type
  ( -- * Entry selector
    EntrySelector,
    mkEntrySelector,
    unEntrySelector,
    getEntryName,
    EntrySelectorException (..),

    -- * Entry description
    EntryDescription (..),
    CompressionMethod (..),

    -- * Archive description
    ArchiveDescription (..),

    -- * Exceptions
    ZipException (..),
  )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Data.Word (Word16, Word32)
import Numeric.Natural
import System.FilePath qualified as FP
import System.FilePath.Posix qualified as Posix
import System.FilePath.Windows qualified as Windows

----------------------------------------------------------------------------
-- Entry selector

-- | This data type serves for naming and selection of archive entries. It
-- can be created only with the help of the smart constructor
-- 'mkEntrySelector', and it's the only “key” that can be used to refer to
-- files in the archive or to name new archive entries.
--
-- The abstraction is crucial for ensuring that created archives are
-- portable across operating systems, file systems, and platforms. Since on
-- some operating systems, file paths are case-insensitive, this selector is
-- also case-insensitive. It makes sure that only relative paths are used to
-- name files inside archive, as it's recommended in the specification. It
-- also guarantees that forward slashes are used when the path is stored
-- inside the archive for compatibility with Unix-like operating systems (as
-- recommended in the specification). On the other hand, in can be rendered
-- as an ordinary relative file path in OS-specific format when needed.
newtype EntrySelector = EntrySelector
  { -- | Path pieces of relative path inside archive
    unES :: NonEmpty (CI String)
  }
  deriving (Eq, Ord, Typeable)

instance Show EntrySelector where
  show = show . unEntrySelector

-- | Create an 'EntrySelector' from a 'FilePath'. To avoid problems with
-- distribution of the archive, characters that some operating systems do
-- not expect in paths are not allowed.
--
-- Argument to 'mkEntrySelector' should pass these checks:
--
--     * 'System.FilePath.Posix.isValid'
--     * 'System.FilePath.Windows.isValid'
--     * it is a relative path without slash at the end
--     * binary representations of normalized path should be not longer than
--       65535 bytes
--
-- This function can throw an 'EntrySelectorException'.
mkEntrySelector :: (MonadThrow m) => FilePath -> m EntrySelector
mkEntrySelector path =
  let f x =
        case filter (not . FP.isPathSeparator) x of
          [] -> Nothing
          xs -> Just (CI.mk xs)
      giveup = throwM (InvalidEntrySelector path)
   in case NE.nonEmpty (mapMaybe f (FP.splitPath path)) of
        Nothing -> giveup
        Just pieces ->
          let selector = EntrySelector pieces
              binLength = B.length . T.encodeUtf8 . getEntryName
           in if Posix.isValid path
                && Windows.isValid path
                && not (FP.isAbsolute path || FP.hasTrailingPathSeparator path)
                && (CI.mk "." `notElem` pieces)
                && (CI.mk ".." `notElem` pieces)
                && binLength selector <= 0xffff
                then return selector
                else giveup

-- | Restore a relative path from 'EntrySelector'. Every 'EntrySelector'
-- corresponds to a 'FilePath'.
unEntrySelector :: EntrySelector -> FilePath
unEntrySelector =
  FP.joinPath . fmap CI.original . NE.toList . unES

-- | Get an entry name in the from that is suitable for writing to file
-- header, given an 'EntrySelector'.
getEntryName :: EntrySelector -> Text
getEntryName =
  T.pack . concat . NE.toList . NE.intersperse "/" . fmap CI.original . unES

-- | The problems you can have with an 'EntrySelector'.
newtype EntrySelectorException
  = -- | 'EntrySelector' cannot be created from this path
    InvalidEntrySelector FilePath
  deriving (Eq, Ord, Typeable)

instance Show EntrySelectorException where
  show (InvalidEntrySelector path) = "Cannot build selector from " ++ show path

instance Exception EntrySelectorException

----------------------------------------------------------------------------
-- Entry description

-- | The information about archive entry that can be stored in a zip
-- archive. It does not mirror local file header or central directory file
-- header, but their binary representations can be built given this data
-- structure and the archive contents.
data EntryDescription = EntryDescription
  { -- | Version made by
    edVersionMadeBy :: Version,
    -- | Version needed to extract
    edVersionNeeded :: Version,
    -- | Compression method
    edCompression :: CompressionMethod,
    -- | Last modification date and time
    edModTime :: UTCTime,
    -- | CRC32 check sum
    edCRC32 :: Word32,
    -- | Size of compressed entry
    edCompressedSize :: Natural,
    -- | Size of uncompressed entry
    edUncompressedSize :: Natural,
    -- | Absolute offset of local file header
    edOffset :: Natural,
    -- | Entry comment
    edComment :: Maybe Text,
    -- | All extra fields found
    edExtraField :: Map Word16 ByteString,
    -- | External file attributes
    --
    -- @since 1.2.0
    edExternalFileAttrs :: Word32
  }
  deriving (Eq, Typeable, Show)

-- | The supported compression methods.
data CompressionMethod
  = -- | Store file uncompressed
    Store
  | -- | Deflate
    Deflate
  | -- | Compressed using BZip2 algorithm
    BZip2
  | -- | Compressed using Zstandard algorithm
    --
    -- @since 1.6.0
    Zstd
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable)

----------------------------------------------------------------------------
-- Archive description

-- | The information about the archive as a whole.
data ArchiveDescription = ArchiveDescription
  { -- | The comment of the entire archive
    adComment :: Maybe Text,
    -- | Absolute offset of the start of central directory
    adCDOffset :: Natural,
    -- | The size of central directory record
    adCDSize :: Natural
  }
  deriving (Show, Read, Eq, Ord, Typeable, Data)

----------------------------------------------------------------------------
-- Exceptions

-- | The bad things that can happen when you use the library.
data ZipException
  = -- | Thrown when you try to get contents of non-existing entry
    EntryDoesNotExist FilePath EntrySelector
  | -- | Thrown when attempting to decompress an entry compressed with an
    -- unsupported compression method or the library is compiled without
    -- support for it.
    --
    -- @since 2.0.0
    UnsupportedCompressionMethod CompressionMethod
  | -- | Thrown when archive structure cannot be parsed.
    ParsingFailed FilePath String
  deriving (Eq, Ord, Typeable)

instance Show ZipException where
  show (EntryDoesNotExist file s) =
    "No such entry found: " ++ show s ++ " in " ++ show file
  show (ParsingFailed file msg) =
    "Parsing of archive structure failed: \n" ++ msg ++ "\nin " ++ show file
  show (UnsupportedCompressionMethod method) =
    "Encountered a zipfile entry with "
      ++ show method
      ++ " compression, but "
      ++ "zip library does not support it or has been built without support for it."

instance Exception ZipException
