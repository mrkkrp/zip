-- |
-- Module      :  Codec.Archive.Zip.Unix
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Unix specific functionality of zip archives.
--
-- @since 1.4.0

module Codec.Archive.Zip.Unix
  ( toFileMode
  , fromFileMode )
where

import Data.Bits
import Data.Word
import System.Posix.Types (CMode (..))

-- | Convert external attributes to the file info.
--
-- >>> toFileMode 2179792896
-- 0o0755
--
-- @since 1.4.0

toFileMode :: Word32 -> CMode
toFileMode attrs = fromIntegral $ (attrs `shiftR` 16) .&. 0x0fff

-- | Convert external attributes to the file info. The function assumes a
-- regular file and keeps DOS attributes untouched.
--
-- >>> fromFileMode 0o0755
-- 2179792896
--
-- @since 1.4.0

fromFileMode :: CMode -> Word32
fromFileMode cmode = (0o100000 .|. fromIntegral cmode) `shiftL` 16
