--
-- Tests for the ‘zip’ package.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Main (main) where

import Codec.Archive.Zip
import Codec.Archive.Zip.CP437
import Control.Monad
import Control.Monad.Catch (catchIOError)
import Control.Monad.IO.Class
import Data.Bits (complement)
import Data.ByteString (ByteString)
import Data.Conduit (yield)
import Data.Foldable (foldl')
import Data.List (intercalate)
import Data.Map (Map, (!))
import Data.Maybe (fromJust)
import Data.Monoid
import Data.Text (Text)
import Data.Time
import Data.Version
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
import qualified Data.Set                as E
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
    describe "getEntryDesc"       getEntryDescSpec
    describe "version needed"     versionNeededSpec
    describe "addEntry"           addEntrySpec
    describe "sinkEntry"          sinkEntrySpec
    describe "loadEntry"          loadEntrySpec
    describe "copyEntry"          copyEntrySpec
    describe "checkEntry"         checkEntrySpec
    describe "recompress"         recompressSpec
    describe "entry comment"      entryCommentSpec
    describe "setModTime"         setModTimeSpec
    describe "extra field"        extraFieldSpec
    describe "renameEntry"        renameEntrySpec
    describe "deleteEntry"        deleteEntrySpec
    describe "forEntries"         forEntriesSpec
    describe "undoEntryChanges"   undoEntryChangesSpec
    describe "undoArchiveChanges" undoArchiveChangesSpec
    describe "undoAll"            undoAllSpec
    describe "consistency"        consistencySpec
    describe "packDirRecur"       packDirRecurSpec
    describe "unpackInto"         unpackIntoSpec

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
    <$> (ModifiedJulianDay <$> choose (44239, 90989))
    <*> (secondsToDiffTime <$> choose (0, 86399))

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

data EM = EM EntrySelector EntryDescription (ZipArchive ()) deriving Show

instance Arbitrary EM where
  arbitrary = do
    s       <- arbitrary
    method  <- arbitrary
    content <- arbitrary
    modTime <- arbitrary
    comment <- arbitrary
    extraFieldTag <- arbitrary `suchThat` (/= 1)
    extraFieldContent <- arbitrary `suchThat` ((< 0xffff) . B.length)
    let action = do
          addEntry method content s
          setModTime modTime s
          setEntryComment comment s
          addExtraField extraFieldTag extraFieldContent s
    return $ EM s EntryDescription
      { edVersionMadeBy    = undefined
      , edVersionNeeded    = undefined
      , edCompression      = method
      , edModTime          = modTime
      , edCRC32            = undefined
      , edCompressedSize   = undefined
      , edUncompressedSize = fromIntegral (B.length content)
      , edOffset           = undefined
      , edComment          = Just comment
      , edExtraField       = M.singleton extraFieldTag extraFieldContent }
      action

data EC = EC (Map EntrySelector EntryDescription) (ZipArchive ()) deriving Show

instance Arbitrary EC where
  arbitrary = foldl' f (EC M.empty (return ())) <$> listOf1 arbitrary
    where f (EC m z') (EM s desc z) = EC (M.insert s desc m) (z' >> z)

charGen :: Gen Char
charGen = frequency
  [ (3, choose ('a', 'z'))
  , (3, choose ('A', 'Z'))
  , (3, choose ('0', '9'))
  , (1, arbitrary `suchThat` (>= ' ')) ]

binASCII :: Gen ByteString
binASCII = LB.toStrict . LB.toLazyByteString <$> go
  where go = frequency
          [ (10, (<>) <$> (LB.word8 <$> choose (0, 127)) <*> go)
          , (1,  return mempty) ]

instance Show EntryDescription where
  show ed = "{ edCompression = " ++ show (edCompression ed) ++
    "\n, edModTime = " ++ show (edModTime ed) ++
    "\n, edUncompressedSize = " ++ show (edUncompressedSize ed) ++
    "\n, edComment = " ++ show (edComment ed) ++
    "\n, edExtraField = " ++ show (edExtraField ed) ++
    " }"

instance Show (ZipArchive a) where
  show = const "<zip archive>"

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

getEntryDescSpec :: SpecWith (Path Abs File)
getEntryDescSpec =
  it "always returns correct description" $
    \path -> property $ \(EM s desc z) -> do
      desc' <- fromJust <$> createArchive path (z >> commit >> getEntryDesc s)
      desc' `shouldSatisfy` softEq desc

versionNeededSpec :: SpecWith (Path Abs File)
versionNeededSpec =
  it "writes correct version that is needed to extract archive" $
    -- NOTE for now we check only how version depends on compression method,
    -- it should be mentioned that the version also depends on Zip64 feature
    \path -> property $ \(EM s desc z) -> do
      desc' <- fromJust <$> createArchive path (z >> commit >> getEntryDesc s)
      edVersionNeeded desc' `shouldBe` makeVersion
        (case edCompression desc of
          Store   -> [2,0]
          Deflate -> [2,0]
          BZip2   -> [4,6])

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

extraFieldSpec :: SpecWith (Path Abs File)
extraFieldSpec = do
  context "when extra field is committed (delete/set)" $
    it "reads it and updates" $ \path -> property $ \n b s ->
      n /= 1 ==> do
        efield <- createArchive path $ do
          addEntry Store "foo" s
          deleteExtraField n s
          addExtraField n b s
          commit
          M.lookup n . edExtraField . (! s) <$> getEntries
        efield `shouldBe` Just b
  context "when extra field is committed (set/delete)" $
    it "reads it and updates" $ \path -> property $ \n b s ->
      n /= 1 ==> do
        efield <- createArchive path $ do
          addEntry Store "foo" s
          addExtraField n b s
          deleteExtraField n s
          commit
          M.lookup n . edExtraField . (! s) <$> getEntries
        efield `shouldBe` Nothing
  context "when pre-existing extra field is overwritten" $
    it "reads it and updates" $ \path -> property $ \n b b' s ->
      n /= 1 ==> do
        efield <- createArchive path $ do
          addEntry Store "foo" s
          addExtraField n b s
          commit
          addExtraField n b' s
          commit
          M.lookup n . edExtraField . (! s) <$> getEntries
        efield `shouldBe` Just b'
  context "when pre-existing extra field is deleted" $
    it "actually deletes it" $ \path -> property $ \n b s ->
      n /= 1 ==> do
        efield <- createArchive path $ do
          addEntry Store "foo" s
          addExtraField n b s
          commit
          deleteExtraField n s
          commit
          M.lookup n . edExtraField . (! s) <$> getEntries
        efield `shouldBe` Nothing

renameEntrySpec :: SpecWith (Path Abs File)
renameEntrySpec = do
  context "when renaming after editing of new entry" $
    it "produces correct result" $ \path -> property $ \(EM s desc z) s' -> do
      desc' <- createArchive path $ do
        z
        renameEntry s s'
        commit
        (! s') <$> getEntries
      desc' `shouldSatisfy` softEq desc
  context "when renaming existing entry" $
    it "gets renamed" $ \path -> property $ \(EM s desc z) s' -> do
      desc' <- createArchive path $ do
        z
        commit
        renameEntry s s'
        commit
        (! s') <$> getEntries
      desc' `shouldSatisfy` softEq desc

deleteEntrySpec :: SpecWith (Path Abs File)
deleteEntrySpec = do
  context "when deleting after editing of new entry" $
    it "produces correct result" $ \path -> property $ \(EM s _ z) -> do
      member <- createArchive path $ do
        z
        deleteEntry s
        commit
        doesEntryExist s
      member `shouldBe` False
  context "when deleting existing entry" $
    it "gets deleted" $ \path -> property $ \(EM s _ z) -> do
      member <- createArchive path $ do
        z
        commit
        deleteEntry s
        commit
        doesEntryExist s
      member `shouldBe` False

forEntriesSpec :: SpecWith (Path Abs File)
forEntriesSpec =
  it "affects all existing entries" $ \path -> property $ \(EC m z) txt -> do
    m' <- createArchive path $ do
      z
      commit
      forEntries (setEntryComment txt)
      commit
      getEntries
    let f ed = ed { edComment = Just txt }
    m' `shouldSatisfy` softEqMap (M.map f m)

undoEntryChangesSpec :: SpecWith (Path Abs File)
undoEntryChangesSpec =
  it "cancels all actions for specified entry" $
    \path -> property $ \(EM s _ z) -> do
      member <- createArchive path $ do
        z
        undoEntryChanges s
        commit
        doesEntryExist s
      member `shouldBe` False

undoArchiveChangesSpec :: SpecWith (Path Abs File)
undoArchiveChangesSpec = do
  it "cancels archive comment editing" $ \path -> property $ \txt -> do
    comment <- createArchive path $ do
      setArchiveComment txt
      undoArchiveChanges
      commit
      getArchiveComment
    comment `shouldBe` Nothing
  it "cancels archive comment deletion" $ \path -> property $ \txt -> do
    comment <- createArchive path $ do
      setArchiveComment txt
      commit
      deleteArchiveComment
      undoArchiveChanges
      commit
      getArchiveComment
    comment `shouldBe` Just txt

undoAllSpec :: SpecWith (Path Abs File)
undoAllSpec =
  it "cancels all editing at once" $ \path -> property $ \(EC _ z) txt -> do
    let fp = toFilePath path
    createArchive path (return ())
    withArchive path $ do
      z
      setArchiveComment txt
      undoAll
      liftIO (B.writeFile fp B.empty)
    B.readFile fp `shouldReturn` B.empty

----------------------------------------------------------------------------
-- Complex construction/restoration

consistencySpec :: SpecWith (Path Abs File)
consistencySpec =
  it "can save and restore arbitrary archive" $
    \path -> property $ \(EC m z) txt -> do
      (txt', m') <- createArchive path $ do
        z
        setArchiveComment txt
        commit
        (,) <$> getArchiveComment <*> getEntries
      txt' `shouldBe` Just txt
      m' `shouldSatisfy` softEqMap m

packDirRecurSpec :: SpecWith (Path Abs File)
packDirRecurSpec =
  it "packs arbitrary directory recursively" $
    \path -> property $ \contents -> do
      let dir = parent path
          f   = stripDir dir >=> mkEntrySelector
      blew <- catchIOError (do
        forM_ contents $ \s -> do
          let item = dir </> unEntrySelector s
          ensureDir (parent item)
          B.writeFile (toFilePath item) "foo"
        return False)
        (const $ return True)
      when blew discard -- TODO
      selectors <- M.keysSet <$>
        createArchive path (packDirRecur Store f dir >> commit >> getEntries)
      selectors `shouldBe` E.fromList contents

unpackIntoSpec :: SpecWith (Path Abs File)
unpackIntoSpec =
  it "unpacks archive contents into directory" $
    \path -> property $ \(EC m z) -> do
      let dir = parent path
      blew <- createArchive path $ do
        z
        commit
        catchIOError
          (unpackInto dir >> return False)
          (const $ return True)
      when blew discard -- TODO
      removeFile path
      selectors <- listDirRecur dir >>=
        mapM (stripDir dir >=> mkEntrySelector) . snd
      E.fromList selectors `shouldBe` M.keysSet m

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

-- | Compare only some fields of 'EntryDescription' record.

softEq :: EntryDescription -> EntryDescription -> Bool
softEq a b =
  edCompression a == edCompression b &&
  isCloseTo (edModTime a) (edModTime b) &&
  edUncompressedSize a == edUncompressedSize b &&
  edComment a == edComment b &&
  M.delete 1 (edExtraField a) == M.delete 1 (edExtraField b)

-- | Compare two maps describing archive entries in such a way that only
-- some fields in 'EntryDescription' record are tested.

softEqMap
  :: Map EntrySelector EntryDescription
  -> Map EntrySelector EntryDescription
  -> Bool
softEqMap n m = M.null (M.differenceWith f n m)
  where f a b = if softEq a b then Nothing else Just a

-- | Canonical representation of empty Zip archive.

emptyArchive :: ByteString
emptyArchive = B.pack
#ifdef USE_ZIP64_ECD
  [ 0x50, 0x4b, 0x06, 0x06, 0x2c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x2e, 0x00, 0x2d, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x50, 0x4b, 0x06, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x50
  , 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
#else
  [ 0x50, 0x4b, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
  , 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ]
#endif
