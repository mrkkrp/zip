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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Main (main) where

import Codec.Archive.Zip
import Codec.Archive.Zip.CP437
import Control.Monad.IO.Class
import Data.Bits (complement)
import Data.ByteString (ByteString)
import Data.Conduit (yield)
import Data.List (intercalate)
import Data.Map (Map, (!))
import Data.Monoid
import Data.Text (Text)
import Data.Time
import Path
import Path.IO
import System.IO
import System.IO.Error (isDoesNotExistError)
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Conduit.List       as CL
import qualified Data.Map                as M
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
  around withSandbox $ do
    describe "createArchive"      createArchiveSpec
    describe "withArchive"        withArchiveSpec
    describe "archive comment"    archiveCommentSpec
    describe "addEntry"           addEntrySpec
    describe "sinkEntry"          sinkEntrySpec
    describe "loadEntry"          loadEntrySpec
    describe "copyEntry"          copyEntrySpec
    describe "checkEntry"         checkEntrySpec
    describe "recompress"         recompressSpec
    describe "entry comment"      entryCommentSpec
    describe "setModTime"         setModTimeSpec

----------------------------------------------------------------------------
-- Arbitrary instances and generators

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf1 arbitrary

instance Arbitrary ByteString where
  arbitrary = B.pack <$> listOf arbitrary

instance Arbitrary CompressionMethod where
  arbitrary = elements [Store, Deflate, BZip2]

instance Arbitrary UTCTime where
  arbitrary = UTCTime
    <$> (ModifiedJulianDay <$> choose (44227, 90978))
    <*> (secondsToDiffTime <$> choose (0, 86400))

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

instance Show EntryDescription where
  show = const "<entry description>"

----------------------------------------------------------------------------
-- Pure operations and periphery

mkEntrySelectorSpec :: Spec
mkEntrySelectorSpec = do
  context "when incorrect Windows paths are passed" $
    it "rejects them" . property $ \path ->
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
    it "has corresponding path" . property $ \s ->
      not . null . toFilePath . unEntrySelector $ s

getEntryNameSpec :: Spec
getEntryNameSpec =
  context "when entry selector exists" $
    it "has corresponding representation" . property $ \s ->
      not . T.null . getEntryName $ s

decodeCP437Spec :: Spec
decodeCP437Spec = do
  context "when ASCII-compatible subset is used" $
    it "has the same result as decoding UTF-8" . property $
      forAll binASCII $ \bin ->
        decodeCP437 bin `shouldBe` T.decodeUtf8 bin
  context "when non-ASCII subset is used" $
    it "is decoded correctly" $ do
      let c b t = decodeCP437 (B.pack b ) `shouldBe` t
      c [0x80..0x9f] "ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒ"
      c [0xa0..0xbf] "áíóúñÑªº¿⌐¬½¼¡«»░▒▓│┤╡╢╖╕╣║╗╝╜╛┐"
      c [0xc0..0xdf] "└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀"
      c [0xe0..0xff] "αßΓπΣσµτΦΘΩδ∞φε∩≡±≥≤⌠⌡÷≈°∙·√ⁿ²■ "

----------------------------------------------------------------------------
-- Primitive editing/querying actions

createArchiveSpec :: SpecWith (Path Abs File)
createArchiveSpec = do
  context "when called with non-existent path and empty recipe" $
    it "creates correct representation of empty archive" $ \path -> do
      createArchive path (return ())
      B.readFile (toFilePath path) `shouldReturn` emptyArchive
  context "when called with occupied path" $
    it "overwrites it" $ \path -> do
      B.writeFile (toFilePath path) B.empty
      createArchive path (return ())
      B.readFile (toFilePath path) `shouldNotReturn` B.empty

withArchiveSpec :: SpecWith (Path Abs File)
withArchiveSpec = do
  context "when called with non-existent path" $
    it "throws 'isDoesNotExistError' exception" $ \path ->
      withArchive path (return ()) `shouldThrow` isDoesNotExistError
  context "when called with occupied path (empty file)" $
    it "throws 'ParsingFailed' exception" $ \path -> do
      B.writeFile (toFilePath path) B.empty
      withArchive path (return ()) `shouldThrow`
        isParsingFailed path "Cannot locate end of central directory"
  context "when called with occupied path (empty archive)" $
    it "does not overwrite the file unnecessarily" $ \path -> do
      let fp = toFilePath path
      B.writeFile fp emptyArchive
      withArchive path . liftIO $ B.writeFile fp B.empty
      B.readFile fp `shouldNotReturn` emptyArchive

archiveCommentSpec :: SpecWith (Path Abs File)
archiveCommentSpec = do
  context "when new archive is created" $
    it "returns no archive comment" $ \path ->
      createArchive path getArchiveComment `shouldReturn` Nothing
  context "when comment contains end of central directory signature" $
    it "reads it without problems" $ \path -> do
      entries <- createArchive path $ do
        setArchiveComment "I saw you want to have PK\ENQ\ACK here."
        commit
        getEntries
      entries `shouldBe` M.empty
  context "when comment is committed (delete/set)" $
    it "reads it and updates" $ \path -> property $ \txt -> do
      comment <- createArchive path $ do
        deleteArchiveComment
        setArchiveComment txt
        commit
        getArchiveComment
      comment `shouldBe` Just txt
  context "when comment is committed (set/delete)" $
    it "reads it and updates" $ \path -> property $ \txt -> do
      comment <- createArchive path $ do
        setArchiveComment txt
        deleteArchiveComment
        commit
        getArchiveComment
      comment `shouldBe` Nothing
  context "when pre-existing comment is overwritten" $
    it "returns the new comment" $ \path -> property $ \txt txt' -> do
      comment <- createArchive path $ do
        setArchiveComment txt
        commit
        setArchiveComment txt'
        commit
        getArchiveComment
      comment `shouldBe` Just txt'
  context "when pre-existing comment is deleted" $
    it "actually deletes it" $ \path -> property $ \txt -> do
      comment <- createArchive path $ do
        setArchiveComment txt
        commit
        deleteArchiveComment
        commit
        getArchiveComment
      comment `shouldBe` Nothing

addEntrySpec :: SpecWith (Path Abs File)
addEntrySpec =
  context "when an entry is added" $
    it "is there" $ \path -> property $ \m b s -> do
      info <- createArchive path $ do
        addEntry m b s
        commit
        (,) <$> getEntry s <*> (edCompression . (! s) <$> getEntries)
      info `shouldBe` (b, m)

sinkEntrySpec :: SpecWith (Path Abs File)
sinkEntrySpec =
  context "when an entry is sunk" $
    it "is there" $ \path -> property $ \m b s -> do
      info <- createArchive path $ do
        sinkEntry m (yield b) s
        commit
        (,) <$> sourceEntry s (CL.foldMap id)
          <*> (edCompression . (! s) <$> getEntries)
      info `shouldBe` (b, m)

loadEntrySpec :: SpecWith (Path Abs File)
loadEntrySpec =
  context "when an entry is loaded" $
    it "is there" $ \path -> property $ \m b s -> do
      let vpath = deriveVacant path
      B.writeFile (toFilePath vpath) b
      createArchive path $ do
        loadEntry m (const $ return s) vpath
        commit
        liftIO (removeFile vpath)
        saveEntry s vpath
      B.readFile (toFilePath vpath) `shouldReturn` b

copyEntrySpec :: SpecWith (Path Abs File)
copyEntrySpec =
  context "when entry is copied form another archive" $
    it "is there" $ \path -> property $ \m b s -> do
      let vpath = deriveVacant path
      createArchive vpath (addEntry m b s)
      info <- createArchive path $ do
        copyEntry vpath s s
        commit
        (,) <$> getEntry s <*> (edCompression . (! s) <$> getEntries)
      info `shouldBe` (b, m)

checkEntrySpec :: SpecWith (Path Abs File)
checkEntrySpec = do
  context "when entry is intact" $
    it "passes the check" $ \path -> property $ \m b s -> do
      check <- createArchive path $ do
        addEntry m b s
        commit
        checkEntry s
      check `shouldBe` True
  context "when entry is corrupted" $
    it "does not pass the check" $ \path -> property $ \b s ->
      not (B.null b) ==> do
        let r = 50 + (B.length . T.encodeUtf8 . getEntryName $ s)
        offset <- createArchive path $ do
          addEntry Store b s
          commit
          fromIntegral . edOffset . (! s) <$> getEntries
        withFile (toFilePath path) ReadWriteMode $ \h -> do
          hSeek h AbsoluteSeek (offset + fromIntegral r)
          byte <- B.map complement <$> B.hGet h 1
          hSeek h RelativeSeek (-1)
          B.hPut h byte
        withArchive path (checkEntry s) `shouldReturn` False

recompressSpec :: SpecWith (Path Abs File)
recompressSpec =
  context "when recompression is used" $
    it "gets recompressed" $ \path -> property $ \m m' b s -> do
      info <- createArchive path $ do
        addEntry m b s
        commit
        recompress m' s
        commit
        (,) <$> getEntry s <*> (edCompression . (! s) <$> getEntries)
      info `shouldBe` (b, m')

entryCommentSpec :: SpecWith (Path Abs File)
entryCommentSpec = do
  context "when comment is committed (delete/set)" $
    it "reads it and updates" $ \path -> property $ \txt s -> do
      comment <- createArchive path $ do
        addEntry Store "foo" s
        deleteEntryComment s
        setEntryComment txt s
        commit
        edComment . (! s) <$> getEntries
      comment `shouldBe` Just txt
  context "when comment is committed (set/delete)" $
    it "reads it and updates" $ \path -> property $ \txt s -> do
      comment <- createArchive path $ do
        addEntry Store "foo" s
        setEntryComment txt s
        deleteEntryComment s
        commit
        edComment . (! s) <$> getEntries
      comment `shouldBe` Nothing
  context "when pre-existing comment is overwritten" $
    it "returns the new comment" $ \path -> property $ \txt txt' s -> do
      comment <- createArchive path $ do
        addEntry Store "foo" s
        setEntryComment txt s
        commit
        setEntryComment txt' s
        commit
        edComment . (! s) <$> getEntries
      comment `shouldBe` Just txt'
  context "when pre-existing comment is deleted" $
    it "actually deletes it" $ \path -> property $ \txt s -> do
      comment <- createArchive path $ do
        addEntry Store "foo" s
        setEntryComment txt s
        commit
        deleteEntryComment s
        commit
        edComment . (! s) <$> getEntries
      comment `shouldBe` Nothing

setModTimeSpec :: SpecWith (Path Abs File)
setModTimeSpec = do
  context "when mod time is set (after creation)" $
    it "reads it and updates" $ \path -> property $ \time s -> do
      modTime <- createArchive path $ do
        addEntry Store "foo" s
        setModTime time s
        commit
        edModTime . (! s) <$> getEntries
      modTime `shouldSatisfy` isCloseTo time
  context "when mod time is set (before creation)" $
    it "has no effect" $ \path -> property $ \time time' s ->
      not (isCloseTo time time') ==> do
        modTime <- createArchive path $ do
          setModTime time s
          addEntry Store "foo" s
          commit
          edModTime . (! s) <$> getEntries
        modTime `shouldNotSatisfy` isCloseTo time

-- Reading of zip archive (listing/inspecting) [no zip64, no unicode]
-- Reading of zip archive [zip64]
-- Reading of zip archive [unicode]
-- Reading of zip archive [zip64, unicode]
-- Decompressing of zip archive
-- Creation, compression, decompression and comparing. Probably QuickCheck

-- Do it with comments and other arbitrary settings (make Arbitrary instance
-- for ZipArchive ()).

-- Check ‘CopyEntry’ thing separately (?)

-- Try with different compression methods

----------------------------------------------------------------------------
-- Helpers

-- | Check whether given exception is 'EntrySelectorException' with specific
-- path inside.

isEntrySelectorException :: Path Rel File -> EntrySelectorException -> Bool
isEntrySelectorException path (InvalidEntrySelector p) = p == path

-- | Check whether given exception is 'ParsingFailed' exception with
-- specific path and error message inside.

isParsingFailed :: Path Abs File -> String -> ZipException -> Bool
isParsingFailed path msg (ParsingFailed path' msg') =
  path == path' && msg == msg'
isParsingFailed _ _ _ = False

-- | Create sandbox directory to model some situation in it and run some
-- tests. Note that we're using new unique sandbox directory for each test
-- case to avoid contamination and it's unconditionally deleted after test
-- case finishes. The function returns vacant file path in that directory.

withSandbox :: ActionWith (Path Abs File) -> IO ()
withSandbox action = withSystemTempDir "zip-sandbox" $ \dir ->
  action (dir </> $(mkRelFile "foo.zip"))

-- | Given primary name (name of archive), generate a name that does not
-- collide with it.

deriveVacant :: Path Abs File -> Path Abs File
deriveVacant = (</> $(mkRelFile "bar")) . parent

-- | Compare times forgiving minor difference.

isCloseTo :: UTCTime -> UTCTime -> Bool
isCloseTo a b = abs (diffUTCTime a b) < 2

-- | Canonical representation of empty Zip archive.

emptyArchive :: ByteString
emptyArchive = B.pack
  [ 0x50, 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
