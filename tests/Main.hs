{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Codec.Archive.Zip
import Codec.Archive.Zip.CP437
import Codec.Archive.Zip.Unix
import Control.Monad
import Control.Monad.IO.Class
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder qualified as LB
import Data.ByteString.Lazy qualified as LB
import Data.Conduit qualified as C
import Data.Conduit.List qualified as CL
import Data.DList qualified as DList
import Data.List (intercalate)
import Data.Map (Map, (!))
import Data.Map.Strict qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as E
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time
import Data.Version
import Data.Word
import Numeric.Natural
import System.Directory
import System.FilePath ((</>))
import System.FilePath qualified as FP
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.IO.Temp
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

ffffffff :: Natural

#ifdef HASKELL_ZIP_DEV_MODE
ffffffff = 250
#else
ffffffff = 0xffffffff
#endif

-- | Zip tests. Please note that the Zip64 feature is not currently tested
-- automatically because we'd need > 4GB of data. Handling such quantities
-- of data locally is problematic and even more problematic on CI.
main :: IO ()
main = hspec $ do
  describe "mkEntrySelector" mkEntrySelectorSpec
  describe "unEntrySelector" unEntrySelectorSpec
  describe "getEntryName" getEntryNameSpec
  describe "decodeCP437" decodeCP437Spec
  describe "fromFileMode" fromFileModeSpec
  around withSandbox $ do
    describe "createArchive" createArchiveSpec
    describe "withArchive" withArchiveSpec
    describe "archive comment" archiveCommentSpec
    describe "getEntryDesc" getEntryDescSpec
    describe "version needed" versionNeededSpec
    describe "addEntry" addEntrySpec
    describe "sinkEntry" sinkEntrySpec
    describe "loadEntry" loadEntrySpec
    describe "copyEntry" copyEntrySpec
    describe "checkEntry" checkEntrySpec
    describe "recompress" recompressSpec
    describe "entry comment" entryCommentSpec
    describe "setModTime" setModTimeSpec
    describe "extra field" extraFieldSpec
    describe "setExternalFileAttrsSpec" setExternalFileAttrsSpec
    describe "renameEntry" renameEntrySpec
    describe "deleteEntry" deleteEntrySpec
    describe "forEntries" forEntriesSpec
    describe "undoEntryChanges" undoEntryChangesSpec
    describe "undoArchiveChanges" undoArchiveChangesSpec
    describe "undoAll" undoAllSpec
    describe "consistency" consistencySpec
    describe "packDirRecur'" packDirRecur'Spec
    describe "unpackInto" unpackIntoSpec

----------------------------------------------------------------------------
-- Arbitrary instances and generators

instance Arbitrary Text where
  arbitrary = T.pack <$> listOf1 arbitrary

instance Arbitrary ByteString where
  arbitrary = B.pack <$> scale (* 10) (listOf arbitrary)

{- ORMOLU_DISABLE -}

instance Arbitrary CompressionMethod where
  arbitrary =
    elements
      [ Store,
#ifdef ENABLE_BZIP2
        BZip2,
#endif
#ifdef ENABLE_ZSTD
        Zstd,
#endif
        Deflate
      ]

{- ORMOLU_ENABLE -}

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime
      <$> (ModifiedJulianDay <$> choose (44239, 90989))
      <*> (secondsToDiffTime <$> choose (0, 86399))

newtype RelPath = RelPath FilePath

instance Show RelPath where
  show (RelPath path) = show path

instance Arbitrary RelPath where
  arbitrary = do
    p <-
      resize 10 $
        intercalate "/"
          <$> listOf1
            ( (++)
                <$> vectorOf 3 charGen
                <*> listOf1 charGen
            )
    case mkEntrySelector p of
      Nothing -> arbitrary
      Just _ -> return (RelPath p)

instance Arbitrary EntrySelector where
  arbitrary = do
    RelPath x <- arbitrary
    case mkEntrySelector x of
      Nothing -> arbitrary
      Just s -> return s

data EM = EM EntrySelector EntryDescription (ZipArchive ()) deriving (Show)

instance Arbitrary EM where
  arbitrary = do
    s <- arbitrary
    method <- arbitrary
    content <- arbitrary
    modTime <- arbitrary
    comment <- arbitrary
    externalFileAttrs <- arbitrary
    extraFieldTag <- arbitrary `suchThat` (/= 1)
    extraFieldContent <- arbitrary `suchThat` ((< 0xffff) . B.length)
    let action = do
          addEntry method content s
          setModTime modTime s
          setEntryComment comment s
          addExtraField extraFieldTag extraFieldContent s
          setExternalFileAttrs externalFileAttrs s
    return $
      EM
        s
        EntryDescription
          { edVersionMadeBy = undefined,
            edVersionNeeded = undefined,
            edCompression = method,
            edModTime = modTime,
            edCRC32 = undefined,
            edCompressedSize = undefined,
            edUncompressedSize = fromIntegral (B.length content),
            edOffset = undefined,
            edComment = Just comment,
            edExtraField = M.singleton extraFieldTag extraFieldContent,
            edExternalFileAttrs = externalFileAttrs
          }
        action

data EC = EC (Map EntrySelector EntryDescription) (ZipArchive ()) deriving (Show)

instance Arbitrary EC where
  arbitrary = do
    let f (EM s d z) = (s, (d, z))
    m <- M.fromList . fmap f <$> downScale (listOf arbitrary)
    return (EC (M.map fst m) (sequence_ $ snd <$> M.elems m))

charGen :: Gen Char
charGen =
  frequency
    [ (3, choose ('a', 'z')),
      (3, choose ('A', 'Z')),
      (3, choose ('0', '9')),
      (1, arbitrary `suchThat` (>= ' '))
    ]

binASCII :: Gen ByteString
binASCII = LB.toStrict . LB.toLazyByteString <$> go
  where
    go =
      frequency
        [ (10, (<>) <$> (LB.word8 <$> choose (0, 127)) <*> go),
          (1, return mempty)
        ]

instance Show (ZipArchive a) where
  show = const "<zip archive>"

----------------------------------------------------------------------------
-- Pure operations and periphery

mkEntrySelectorSpec :: Spec
mkEntrySelectorSpec = do
  let rejects x =
        mkEntrySelector x `shouldThrow` isEntrySelectorException x
      accepts x = do
        s <- mkEntrySelector x
        getEntryName s `shouldBe` T.pack x
  context "when absolute paths are passed" $
    it "they are rejected" $
      property $ \(RelPath x) ->
        rejects ('/' : x)
  context "when paths with trailing path separator are passed" $
    it "they are rejected" $ do
      rejects "foo/"
      rejects "foo/bar/"
  context "when paths with dot as path segment are passed" $
    it "they are rejected" $ do
      rejects "./foo/bar"
      rejects "foo/./bar"
      rejects "foo/bar/."
  context "when paths with double dot as path segment are passed" $
    it "they are rejected" $ do
      rejects "../foo/bar"
      rejects "foo/../bar"
      rejects "foo/bar/.."
  context "when too long paths are passed" $
    it "rejects them" $ do
      let path = replicate 0x10000 'a'
      mkEntrySelector path `shouldThrow` isEntrySelectorException path
  context "when correct paths are passed" $
    it "adequately represents them" $ do
      accepts "foo"
      accepts "one/two/three"
      accepts "something.txt"

unEntrySelectorSpec :: Spec
unEntrySelectorSpec =
  context "when entry selector exists" $
    it "has corresponding path" . property $ \s ->
      not . null . unEntrySelector $ s

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
      let c b t = decodeCP437 (B.pack b) `shouldBe` t
      c [0x80 .. 0x9f] "ÇüéâäàåçêëèïîìÄÅÉæÆôöòûùÿÖÜ¢£¥₧ƒ"
      c [0xa0 .. 0xbf] "áíóúñÑªº¿⌐¬½¼¡«»░▒▓│┤╡╢╖╕╣║╗╝╜╛┐"
      c [0xc0 .. 0xdf] "└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌█▄▌▐▀"
      c [0xe0 .. 0xff] "αßΓπΣσµτΦΘΩδ∞φε∩≡±≥≤⌠⌡÷≈°∙·√ⁿ²■ "

fromFileModeSpec :: Spec
fromFileModeSpec =
  context "UNIX helpers" $ do
    it "toFileMode . fromFileMode == id .&. 0x0fffff" . property $ \mode ->
      (toFileMode . fromFileMode) (fromIntegral mode)
        == fromIntegral (mode .&. (0x0fff :: Word16))
    it "toFileMode == toFileMode . fromFileMode . toFileMode" . property $ \mode ->
      toFileMode mode == (toFileMode . fromFileMode . toFileMode) mode

----------------------------------------------------------------------------
-- Primitive editing/querying actions

createArchiveSpec :: SpecWith FilePath
createArchiveSpec = do
  context "when called with non-existent path and empty recipe" $
    it "creates correct representation of empty archive" $ \path -> do
      createArchive path (return ())
      B.readFile path `shouldReturn` emptyArchive
  context "when called with an occupied path" $
    it "overwrites it" $ \path -> do
      B.writeFile path B.empty
      createArchive path (return ())
      B.readFile path `shouldNotReturn` B.empty

withArchiveSpec :: SpecWith FilePath
withArchiveSpec = do
  context "when called with non-existent path" $
    it "throws 'isDoesNotExistError' exception" $ \path ->
      withArchive path (return ()) `shouldThrow` isDoesNotExistError
  context "when called with occupied path (empty file)" $
    it "throws 'ParsingFailed' exception" $ \path -> do
      B.writeFile path B.empty
      withArchive path (return ())
        `shouldThrow` isParsingFailed path "Cannot locate end of central directory"
  context "when called with occupied path (empty archive)" $
    it "does not overwrite the file unnecessarily" $ \path -> do
      B.writeFile path emptyArchive
      withArchive path $
        liftIO $
          B.writeFile path B.empty
      B.readFile path `shouldNotReturn` emptyArchive

archiveCommentSpec :: SpecWith FilePath
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
    it "reads it and updates" $ \path ->
      property $ \txt -> do
        comment <- createArchive path $ do
          deleteArchiveComment
          setArchiveComment txt
          commit
          getArchiveComment
        comment `shouldBe` Just txt
  context "when comment is committed (set/delete)" $
    it "reads it and updates" $ \path ->
      property $ \txt -> do
        comment <- createArchive path $ do
          setArchiveComment txt
          deleteArchiveComment
          commit
          getArchiveComment
        comment `shouldBe` Nothing
  context "when pre-existing comment is overwritten" $
    it "returns the new comment" $ \path ->
      property $ \txt txt' -> do
        comment <- createArchive path $ do
          setArchiveComment txt
          commit
          setArchiveComment txt'
          commit
          getArchiveComment
        comment `shouldBe` Just txt'
  context "when pre-existing comment is deleted" $
    it "actually deletes it" $ \path ->
      property $ \txt -> do
        comment <- createArchive path $ do
          setArchiveComment txt
          commit
          deleteArchiveComment
          commit
          getArchiveComment
        comment `shouldBe` Nothing

getEntryDescSpec :: SpecWith FilePath
getEntryDescSpec =
  it "always returns correct description" $ \path ->
    property $ \(EM s desc z) -> do
      desc' <- fromJust <$> createArchive path (z >> commit >> getEntryDesc s)
      desc' `shouldSatisfy` softEq desc

versionNeededSpec :: SpecWith FilePath
versionNeededSpec =
  it "writes correct version that is needed to extract archive" $ \path ->
    -- NOTE for now we check only how version depends on compression method,
    -- it should be mentioned that the version also depends on Zip64 feature
    property $ \(EM s desc z) -> do
      desc' <- fromJust <$> createArchive path (z >> commit >> getEntryDesc s)
      let minVersionZip64 =
            makeVersion $
              if edUncompressedSize desc' >= ffffffff
                || edCompressedSize desc' >= ffffffff
                then [4, 5]
                else [2, 0]
          minVersionCompression = makeVersion $ case edCompression desc of
            Store -> [2, 0]
            Deflate -> [2, 0]
            BZip2 -> [4, 6]
            Zstd -> [6, 3]
          versionNeeded = max minVersionZip64 minVersionCompression
      edVersionNeeded desc' `shouldBe` versionNeeded

addEntrySpec :: SpecWith FilePath
addEntrySpec =
  context "when an entry is added" $
    it "is there" $ \path ->
      property $ \m b s -> do
        info <- createArchive path $ do
          addEntry m b s
          commit
          checkEntry' s
          (,) <$> getEntry s <*> (edCompression . (! s) <$> getEntries)
        info `shouldBe` (b, m)

sinkEntrySpec :: SpecWith FilePath
sinkEntrySpec =
  context "when an entry is sunk" $
    it "is there" $ \path ->
      property $ \m b s -> do
        info <- createArchive path $ do
          sinkEntry m (C.yield b) s
          commit
          checkEntry' s
          (,)
            <$> sourceEntry s (CL.foldMap id)
            <*> (edCompression . (! s) <$> getEntries)
        info `shouldBe` (b, m)

loadEntrySpec :: SpecWith FilePath
loadEntrySpec =
  context "when an entry is loaded" $
    it "is there" $ \path ->
      property $ \m b s t -> do
        let vpath = deriveVacant path
        B.writeFile vpath b
        setModificationTime vpath t
        createArchive path $ do
          loadEntry m s vpath
          commit
          checkEntry' s
          liftIO (removeFile vpath)
          saveEntry s vpath
        B.readFile vpath `shouldReturn` b
        modTime <- getModificationTime vpath
        modTime `shouldSatisfy` isCloseTo t

copyEntrySpec :: SpecWith FilePath
copyEntrySpec =
  context "when entry is copied form another archive" $
    it "is there" $ \path ->
      property $ \m b s -> do
        let vpath = deriveVacant path
        createArchive vpath (addEntry m b s)
        info <- createArchive path $ do
          copyEntry vpath s s
          commit
          checkEntry' s
          (,) <$> getEntry s <*> (edCompression . (! s) <$> getEntries)
        info `shouldBe` (b, m)

checkEntrySpec :: SpecWith FilePath
checkEntrySpec = do
  context "when entry is intact" $
    it "passes the check" $ \path ->
      property $ \m b s ->
        asIO . createArchive path $ do
          addEntry m b s
          commit
          checkEntry' s
  context "when entry is corrupted" $
    it "does not pass the check" $ \path ->
      property $ \b s ->
        not (B.null b) ==> do
          let headerLength = 50 + (B.length . T.encodeUtf8 . getEntryName $ s)
          localFileHeaderOffset <- createArchive path $ do
            addEntry Store b s
            commit
            fromIntegral . edOffset . (! s) <$> getEntries
          withFile path ReadWriteMode $ \h -> do
            hSeek
              h
              AbsoluteSeek
              (localFileHeaderOffset + fromIntegral headerLength)
            byte <- B.map complement <$> B.hGet h 1
            hSeek h RelativeSeek (-1)
            B.hPut h byte
          withArchive path (checkEntry s) `shouldReturn` False

recompressSpec :: SpecWith FilePath
recompressSpec =
  context "when recompression is used" $
    it "gets recompressed" $ \path ->
      property $ \m m' b s -> do
        info <- createArchive path $ do
          addEntry m b s
          commit
          checkEntry' s
          recompress m' s
          commit
          checkEntry' s
          (,) <$> getEntry s <*> (edCompression . (! s) <$> getEntries)
        info `shouldBe` (b, m')

entryCommentSpec :: SpecWith FilePath
entryCommentSpec = do
  context "when comment is committed (delete/set)" $
    it "reads it and updates" $ \path ->
      property $ \txt s -> do
        comment <- createArchive path $ do
          addEntry Store "foo" s
          deleteEntryComment s
          setEntryComment txt s
          commit
          edComment . (! s) <$> getEntries
        comment `shouldBe` Just txt
  context "when comment is committed (set/delete)" $
    it "reads it and updates" $ \path ->
      property $ \txt s -> do
        comment <- createArchive path $ do
          addEntry Store "foo" s
          setEntryComment txt s
          deleteEntryComment s
          commit
          edComment . (! s) <$> getEntries
        comment `shouldBe` Nothing
  context "when pre-existing comment is overwritten" $
    it "returns the new comment" $ \path ->
      property $ \txt txt' s -> do
        comment <- createArchive path $ do
          addEntry Store "foo" s
          setEntryComment txt s
          commit
          setEntryComment txt' s
          commit
          edComment . (! s) <$> getEntries
        comment `shouldBe` Just txt'
  context "when pre-existing comment is deleted" $
    it "actually deletes it" $ \path ->
      property $ \txt s -> do
        comment <- createArchive path $ do
          addEntry Store "foo" s
          setEntryComment txt s
          commit
          deleteEntryComment s
          commit
          edComment . (! s) <$> getEntries
        comment `shouldBe` Nothing

setModTimeSpec :: SpecWith FilePath
setModTimeSpec = do
  context "when mod time is set (after creation)" $
    it "reads it and updates" $ \path ->
      property $ \time s -> do
        modTime <- createArchive path $ do
          addEntry Store "foo" s
          setModTime time s
          commit
          edModTime . (! s) <$> getEntries
        modTime `shouldSatisfy` isCloseTo time
  context "when mod time is set (before creation)" $
    it "has no effect" $ \path ->
      property $ \time time' s ->
        not (isCloseTo time time') ==> do
          modTime <- createArchive path $ do
            setModTime time s
            addEntry Store "foo" s
            commit
            edModTime . (! s) <$> getEntries
          modTime `shouldNotSatisfy` isCloseTo time

extraFieldSpec :: SpecWith FilePath
extraFieldSpec = do
  context "when extra field is committed (delete/set)" $
    it "reads it and updates" $ \path ->
      property $ \n b s ->
        n /= 1 ==> do
          efield <- createArchive path $ do
            addEntry Store "foo" s
            deleteExtraField n s
            addExtraField n b s
            commit
            M.lookup n . edExtraField . (! s) <$> getEntries
          efield `shouldBe` Just b
  context "when extra field is committed (set/delete)" $
    it "reads it and updates" $ \path ->
      property $ \n b s ->
        n /= 1 ==> do
          efield <- createArchive path $ do
            addEntry Store "foo" s
            addExtraField n b s
            deleteExtraField n s
            commit
            M.lookup n . edExtraField . (! s) <$> getEntries
          efield `shouldBe` Nothing
  context "when pre-existing extra field is overwritten" $
    it "reads it and updates" $ \path ->
      property $ \n b b' s ->
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
    it "actually deletes it" $ \path ->
      property $ \n b s ->
        n /= 1 ==> do
          efield <- createArchive path $ do
            addEntry Store "foo" s
            addExtraField n b s
            commit
            deleteExtraField n s
            commit
            M.lookup n . edExtraField . (! s) <$> getEntries
          efield `shouldBe` Nothing

setExternalFileAttrsSpec :: SpecWith FilePath
setExternalFileAttrsSpec =
  context "when an external file attribute is added (after creation)" $
    it "sets a custom external file attribute" $ \path ->
      property $ \attr s -> do
        attr' <- createArchive path $ do
          addEntry Store "foo" s
          setExternalFileAttrs attr s
          commit
          edExternalFileAttrs . (! s) <$> getEntries
        attr' `shouldBe` attr

renameEntrySpec :: SpecWith FilePath
renameEntrySpec = do
  context "when renaming after editing of new entry" $
    it "produces correct result" $ \path ->
      property $ \(EM s desc z) s' -> do
        desc' <- createArchive path $ do
          z
          renameEntry s s'
          commit
          (! s') <$> getEntries
        desc' `shouldSatisfy` softEq desc
  context "when renaming existing entry" $
    it "gets renamed" $ \path ->
      property $ \(EM s desc z) s' -> do
        desc' <- createArchive path $ do
          z
          commit
          renameEntry s s'
          commit
          (! s') <$> getEntries
        desc' `shouldSatisfy` softEq desc

deleteEntrySpec :: SpecWith FilePath
deleteEntrySpec = do
  context "when deleting after editing of new entry" $
    it "produces correct result" $ \path ->
      property $ \(EM s _ z) -> do
        member <- createArchive path $ do
          z
          deleteEntry s
          commit
          doesEntryExist s
        member `shouldBe` False
  context "when deleting existing entry" $
    it "gets deleted" $ \path ->
      property $ \(EM s _ z) -> do
        member <- createArchive path $ do
          z
          commit
          deleteEntry s
          commit
          doesEntryExist s
        member `shouldBe` False

forEntriesSpec :: SpecWith FilePath
forEntriesSpec =
  it "affects all existing entries" $ \path -> property $ \(EC m z) txt -> do
    m' <- createArchive path $ do
      z
      commit
      forEntries (setEntryComment txt)
      commit
      getEntries
    let f ed = ed {edComment = Just txt}
    m' `shouldSatisfy` softEqMap (M.map f m)

undoEntryChangesSpec :: SpecWith FilePath
undoEntryChangesSpec =
  it "cancels all actions for specified entry" $
    \path -> property $ \(EM s _ z) -> do
      member <- createArchive path $ do
        z
        undoEntryChanges s
        commit
        doesEntryExist s
      member `shouldBe` False

undoArchiveChangesSpec :: SpecWith FilePath
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

undoAllSpec :: SpecWith FilePath
undoAllSpec =
  it "cancels all editing at once" $ \path -> property $ \(EC _ z) txt -> do
    createArchive path (return ())
    withArchive path $ do
      z
      setArchiveComment txt
      undoAll
      liftIO (B.writeFile path B.empty)
    B.readFile path `shouldReturn` B.empty

----------------------------------------------------------------------------
-- Complex construction/restoration

consistencySpec :: SpecWith FilePath
consistencySpec =
  it "can save and restore arbitrary archive" $ \path ->
    property $ \(EC m z) txt -> do
      (txt', m') <- createArchive path $ do
        z
        setArchiveComment txt
        commit
        (,) <$> getArchiveComment <*> getEntries
      txt' `shouldBe` Just txt
      m' `shouldSatisfy` softEqMap m

packDirRecur'Spec :: SpecWith FilePath
packDirRecur'Spec =
  it "packs arbitrary directory recursively" $ \path ->
    property $
      forAll (downScale arbitrary) $ \contents ->
        withSystemTempDirectory "zip-sandbox" $ \dir -> do
          forM_ contents $ \s -> do
            let item = dir </> unEntrySelector s
            createDirectoryIfMissing True (FP.takeDirectory item)
            B.writeFile item "foo"
          let magicFileAttrs = 123456789
          entries <-
            createArchive path $ do
              packDirRecur'
                Store
                mkEntrySelector
                (setExternalFileAttrs magicFileAttrs)
                dir
              commit
              getEntries
          M.keysSet entries `shouldBe` E.fromList contents
          forM_ (M.elems entries) $ \desc ->
            edExternalFileAttrs desc `shouldBe` magicFileAttrs

unpackIntoSpec :: SpecWith FilePath
unpackIntoSpec =
  it "unpacks archive contents into directory" $ \path ->
    property $ \(EC m z) ->
      withSystemTempDirectory "zip-sandbox" $ \dir -> do
        createArchive path $ do
          z
          commit
          unpackInto dir
        selectors <- listDirRecur dir >>= mapM mkEntrySelector
        let x = E.fromList selectors
            y = M.keysSet m
        E.difference x y `shouldBe` E.empty

----------------------------------------------------------------------------
-- Helpers

-- | Change the size parameter of a generator by dividing it by 2.
downScale :: Gen a -> Gen a
downScale = scale (`div` 2)

-- | Check whether a given exception is 'EntrySelectorException' with a
-- specific path inside.
isEntrySelectorException :: FilePath -> EntrySelectorException -> Bool
isEntrySelectorException path (InvalidEntrySelector p) = p == path

-- | Check whether a given exception is 'ParsingFailed' exception with a
-- specific path and error message inside.
isParsingFailed :: FilePath -> String -> ZipException -> Bool
isParsingFailed path msg (ParsingFailed path' msg') =
  path == path' && msg == msg'
isParsingFailed _ _ _ = False

-- | Create a sandbox directory to model some situation in it and run some
-- tests. Note that we're using a new unique sandbox directory for each test
-- case to avoid contamination and it's unconditionally deleted after the
-- test case finishes. The function returns a vacant file path in that
-- directory.
withSandbox :: ActionWith FilePath -> IO ()
withSandbox action = withSystemTempDirectory "zip-sandbox" $ \dir ->
  action (dir </> "foo.zip")

-- | Like 'checkEntry' but automatically aborts the test if the check fails.
checkEntry' :: EntrySelector -> ZipArchive ()
checkEntry' s = do
  r <- checkEntry s
  liftIO (if r then return () else fail "Entry integrity check failed!")

-- | Given a primary name (name of archive), generate a name that does not
-- collide with it.
deriveVacant :: FilePath -> FilePath
deriveVacant = (</> "bar") . FP.takeDirectory

-- | Compare times forgiving a minor difference.
isCloseTo :: UTCTime -> UTCTime -> Bool
isCloseTo a b = abs (diffUTCTime a b) < 2

-- | Compare for equality taking into account only some fields of the
-- 'EntryDescription' record.
softEq :: EntryDescription -> EntryDescription -> Bool
softEq a b =
  edCompression a == edCompression b
    && isCloseTo (edModTime a) (edModTime b)
    && edUncompressedSize a == edUncompressedSize b
    && edComment a == edComment b
    && M.delete 1 (edExtraField a) == M.delete 1 (edExtraField b)

-- | Compare two maps describing archive entries in such a way that only
-- some fields in 'EntryDescription' record are tested.
softEqMap ::
  Map EntrySelector EntryDescription ->
  Map EntrySelector EntryDescription ->
  Bool
softEqMap n m = M.null (M.differenceWith f n m)
  where
    f a b = if softEq a b then Nothing else Just a

-- | The canonical representation of an empty Zip archive.
emptyArchive :: ByteString
emptyArchive =
  B.pack
    [ 0x50,
      0x4b,
      0x05,
      0x06,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00
    ]

-- | Recursively list a directory. Do not return paths to empty directories.
listDirRecur :: FilePath -> IO [FilePath]
listDirRecur path = DList.toList <$> go ""
  where
    go adir = do
      let cdir = path </> adir
      raw <- listDirectory cdir
      fmap mconcat . forM raw $ \case
        "" -> return mempty
        "." -> return mempty
        ".." -> return mempty
        x -> do
          let fullx = cdir </> x
              adir' = adir </> x
          isFile <- doesFileExist fullx
          isDir <- doesDirectoryExist fullx
          if isFile
            then return (DList.singleton adir')
            else
              if isDir
                then go adir'
                else return mempty

-- | Constrain the type of the argument monad to 'IO'.
asIO :: IO a -> IO a
asIO = id
