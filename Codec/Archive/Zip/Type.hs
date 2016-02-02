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
    EntrySelectorException
  , EntrySelector
  , mkEntrySelector
  , unEntrySelector )
where

import Control.Exception (Exception)
import Control.Monad.Catch (MonadThrow (..))
import Data.CaseInsensitive (CI)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe, fromJust)
import Data.Typeable (Typeable)
import Path
import qualified Data.CaseInsensitive    as CI
import qualified Data.List.NonEmpty      as NE
import qualified System.FilePath         as FP
import qualified System.FilePath.Posix   as Posix
import qualified System.FilePath.Windows as Windows

-- | Exception describing various troubles you can have with
-- 'EntrySelector'.

data EntrySelectorException
  = InvalidEntrySelector (Path Rel File)
    -- ^ Selector cannot be created from this path
  deriving (Typeable)

instance Show EntrySelectorException where
  show (InvalidEntrySelector path) = "Cannot build selector from " ++ show path

instance Exception EntrySelectorException

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
unEntrySelector = fromJust
  . parseRelFile
  . FP.joinPath
  . fmap CI.original
  . NE.toList
  . unES
