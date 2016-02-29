--
-- Tests for the ‘zip’ package.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Main (main) where

import Codec.Archive.Zip
import Codec.Archive.Zip.CP437
import Data.ByteString (ByteString)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Monoid
import Path
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Lazy    as LB
import qualified Data.ByteString.Builder as LB
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import qualified System.FilePath.Windows as Windows

-- | Zip tests. Please note that Zip64 feature is not currently tested
-- automatically because for it to expose itself we need > 4GB of
-- data. Handling such quantities of data locally is problematic and even
-- more problematic in the context of CI server.

main :: IO ()
main = hspec $ do
  describe "mkEntrySelector" mkEntrySelectorSpec
  describe "unEntrySelector" unEntrySelectorSpec
  describe "getEntryName"    getEntryNameSpec
  describe "decodeCP437"     decodeCP437Spec

----------------------------------------------------------------------------
-- Arbitrary instances and generators

instance Arbitrary (Path Rel File) where
  arbitrary = do
    x <- intercalate "/" <$> listOf1 (listOf1 charGen)
    case parseRelFile x of
      Nothing -> arbitrary
      Just path -> return path

instance Arbitrary EntrySelector where
  arbitrary = do
    x <- arbitrary
    case mkEntrySelector x of
      Nothing -> arbitrary
      Just s  -> return s

instance Arbitrary (ZipArchive (), Map EntrySelector EntryDescription) where
  arbitrary = undefined

instance Arbitrary (ZipArchive (), ArchiveDescription) where
  arbitrary = undefined

charGen :: Gen Char
charGen = frequency
  [ (3, choose ('a', 'z'))
  , (3, choose ('A', 'Z'))
  , (3, choose ('0', '9'))
  , (1, arbitrary) ]

binASCII :: Gen ByteString
binASCII = LB.toStrict . LB.toLazyByteString <$> go
  where go = frequency
          [ (10, (<>) <$> (LB.word8 <$> choose (0, 127)) <*> go)
          , (1,  return mempty) ]

----------------------------------------------------------------------------
-- Pure operations and periphery

mkEntrySelectorSpec :: Spec
mkEntrySelectorSpec = do
  context "when incorrect Windows paths are passed" $
    it "rejects them" $ property $ \path ->
      (not . Windows.isValid . toFilePath $ path)
        ==> mkEntrySelector path === Nothing
  context "when too long paths are passed" $
    it "rejects them" $ do
      path <- parseRelFile (replicate 0x10000 'a')
      mkEntrySelector path `shouldThrow` isEntrySelectorException path
  context "when correct paths are passed" $
    it "adequately represents them" $ do
      let c str = do
            s <- parseRelFile str >>= mkEntrySelector
            getEntryName s `shouldBe` T.pack str
      c "one/two/three"
      c "something.txt"

unEntrySelectorSpec :: Spec
unEntrySelectorSpec =
  context "when entry selector exists" $
    it "has corresponding path" $ property $ \s ->
      not . null . toFilePath . unEntrySelector $ s

getEntryNameSpec :: Spec
getEntryNameSpec =
  context "when entry selector exists" $
    it "has corresponding representation" $ property $ \s ->
      not . T.null . getEntryName $ s

decodeCP437Spec :: Spec
decodeCP437Spec =
  context "when ASCII-compatible subset is used" $
    it "has the same result as decoding UTF-8" $ property $
      forAll binASCII $ \bin ->
        decodeCP437 bin `shouldBe` T.decodeUtf8 bin

----------------------------------------------------------------------------
-- Manipulations on archives

-- TODO We need Quick Check tests here to test some properties of
-- 'EntrySelector'

-- List of tests to write:

-- Reading of zip archive (listing/inspecting) [no zip64, no unicode]
-- Reading of zip archive [zip64]
-- Reading of zip archive [unicode]
-- Reading of zip archive [zip64, unicode]
-- Decompressing of zip archive
-- Creation, compression, decompression and comparing. Probably QuickCheck

-- Do it with comments and other arbitrary settings (make Arbitrary instance
-- for ZipArchive ()). Try with archive comments that contain end of central
-- directory signature.

-- Test reading of empty archives.

-- Check ‘CopyEntry’ thing separately (?)

-- Try with different compression methods

----------------------------------------------------------------------------
-- Helpers

-- | Check whether given exception is 'EntrySelectorException' with specific
-- path inside.

isEntrySelectorException :: Path Rel File -> EntrySelectorException -> Bool
isEntrySelectorException path (InvalidEntrySelector p) = p == path

-- | Create sandbox directory to model some situation in it and run some
-- tests. Note that we're using new unique sandbox directory for each test
-- case to avoid contamination and it's unconditionally deleted after test
-- case finishes.

-- withSandbox :: ActionWith (Path Abs Dir) -> IO ()
-- withSandbox = withSystemTempDir "zip-sandbox"
