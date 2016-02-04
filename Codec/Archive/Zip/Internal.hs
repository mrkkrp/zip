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
  ( PendingAction
  )
where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

data PendingAction
  = AddEntry
  | RemoveEntry

----------------------------------------------------------------------------
-- Helpers

-- | Detect if the given text needs newer Unicode-aware features to be
-- properly encoded in archive.

needsUnicode :: Text -> Bool
needsUnicode = not . T.all validCP437
  where validCP437 x = let y = ord x in y >= 32 && y <= 126
