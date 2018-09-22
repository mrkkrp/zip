-- |
-- Module      :  Codec.Archive.Zip.Type
-- Copyright   :  © 2016–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types used by the package.

{-# LANGUAGE DeriveDataTypeable #-}

module Codec.Archive.Zip.Type
  ( -- * Entry selector
    EntrySelector
  , mkEntrySelector
  , unEntrySelector
  , getEntryName
  , EntrySelectorException (..)
    -- * Entry description
  , EntryDescription (..)
  , CompressionMethod (..)
    -- * Archive description
  , ArchiveDescription (..)
    -- * Exceptions
  , ZipException (..) )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Typeable (Typeable)
import Data.Version (Version)
import Data.Word (Word16, Word32)
import Numeric.Natural
import qualified Data.ByteString         as B
import qualified Data.CaseInsensitive    as CI
import qualified Data.List.NonEmpty      as NE
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified System.FilePath         as FP
import qualified System.FilePath.Posix   as Posix
import qualified System.FilePath.Windows as Windows

----------------------------------------------------------------------------
-- Entry selector

-- | This data type serves for naming and selection of archive entries. It
-- can be created only with help of the smart constructor 'mkEntrySelector',
-- and it's the only “key” that can be used to refer to files in archive or
-- to name new archive entries.
--
-- The abstraction is crucial for ensuring that created archives are
-- portable across operating systems, file systems, and different platforms.
-- Since on some operating systems, file paths are case-insensitive, this
-- selector is also case-insensitive. It makes sure that only relative paths
-- are used to name files inside archive, as it's recommended in the
-- specification. It also guarantees that forward slashes are used when the
-- path is stored inside archive for compatibility with Unix-like operating
-- systems (as recommended in the specification). On the other hand, in can
-- be rendered as an ordinary relative file path in OS-specific format when
-- needed.

newtype EntrySelector = EntrySelector
  { unES :: NonEmpty (CI String)
    -- ^ Path pieces of relative path inside archive
  } deriving (Eq, Ord, Typeable)

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

mkEntrySelector :: MonadThrow m => FilePath -> m EntrySelector
mkEntrySelector path =
  let f x =
        case filter (not . FP.isPathSeparator) x of
          [] -> Nothing
          xs -> Just (CI.mk xs)
      giveup = throwM (InvalidEntrySelector path)
  in case NE.nonEmpty (mapMaybe f (FP.splitPath path)) of
       Nothing -> giveup
       Just pieces ->
         let selector  = EntrySelector pieces
             binLength = B.length . T.encodeUtf8 . getEntryName
         in if Posix.isValid   path &&
               Windows.isValid path &&
               not (FP.isAbsolute path || FP.hasTrailingPathSeparator path) &&
               (CI.mk "."  `notElem` pieces) &&
               (CI.mk ".." `notElem` pieces) &&
               binLength selector <= 0xffff
              then return selector
              else giveup

-- | Restore a relative path from 'EntrySelector'. Every 'EntrySelector'
-- corresponds to a single 'FilePath'.

unEntrySelector :: EntrySelector -> FilePath
unEntrySelector =
  FP.joinPath . fmap CI.original . NE.toList . unES

-- | Get an entry name in the from that is suitable for writing to file
-- header, given an 'EntrySelector'.

getEntryName :: EntrySelector -> Text
getEntryName =
  T.pack . concat . NE.toList . NE.intersperse "/" . fmap CI.original . unES

-- | The exception represents various troubles you can have with
-- 'EntrySelector'.

newtype EntrySelectorException
  = InvalidEntrySelector FilePath
    -- ^ 'EntrySelector' cannot be created from this path
  deriving (Eq, Ord, Typeable)

instance Show EntrySelectorException where
  show (InvalidEntrySelector path) = "Cannot build selector from " ++ show path

instance Exception EntrySelectorException

----------------------------------------------------------------------------
-- Entry description

-- | This record represents all information about archive entry that can be
-- stored in a zip archive. It does not mirror local file header or central
-- directory file header, but their binary representations can be built
-- given this data structure and the actual archive contents.

data EntryDescription = EntryDescription
  { edVersionMadeBy    :: Version -- ^ Version made by
  , edVersionNeeded    :: Version -- ^ Version needed to extract
  , edCompression      :: CompressionMethod -- ^ Compression method
  , edModTime          :: UTCTime -- ^ Last modification date and time
  , edCRC32            :: Word32  -- ^ CRC32 check sum
  , edCompressedSize   :: Natural -- ^ Size of compressed entry
  , edUncompressedSize :: Natural -- ^ Size of uncompressed entry
  , edOffset           :: Natural -- ^ Absolute offset of local file header
  , edComment          :: Maybe Text -- ^ Entry comment
  , edExtraField       :: Map Word16 ByteString -- ^ All extra fields found
  , edExternalFileAttrs :: Word32 -- ^ External file attributes
                                  --
                                  -- @since 1.2.0
  } deriving (Eq, Typeable)

-- | Supported compression methods.

data CompressionMethod
  = Store              -- ^ Store file uncompressed
  | Deflate            -- ^ Deflate
  | BZip2              -- ^ Compressed using BZip2 algorithm
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable)

----------------------------------------------------------------------------
-- Archive description

-- | Information about archive as a whole.

data ArchiveDescription = ArchiveDescription
  { adComment  :: Maybe Text -- ^ Comment of entire archive
  , adCDOffset :: Natural -- ^ Absolute offset of start of central directory
  , adCDSize   :: Natural -- ^ Size of central directory record
  } deriving (Show, Read, Eq, Ord, Typeable, Data)

----------------------------------------------------------------------------
-- Exceptions

-- | The bad things that can happen when you use the library.

data ZipException
  = EntryDoesNotExist FilePath EntrySelector
    -- ^ Thrown when you try to get contents of non-existing entry
  | ParsingFailed FilePath String
    -- ^ Thrown when archive structure cannot be parsed
  deriving (Eq, Ord, Typeable)

instance Show ZipException where
  show (EntryDoesNotExist file s) =
    "No such entry found: " ++ show s ++ " in " ++ show file
  show (ParsingFailed file msg) =
    "Parsing of archive structure failed: \n" ++ msg ++ "\nin " ++ show file

instance Exception ZipException
