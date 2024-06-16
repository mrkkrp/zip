## Zip 2.1.0

* Exposed `Codec.Archive.Zip.Internal` and `Codec.Archive.Zip.Internal.Type`
  modules. [PR 115](https://github.com/mrkkrp/zip/pull/115).

* Derived `Show` for `EntryDescription`. [PR
  115](https://github.com/mrkkrp/zip/pull/115).

## Zip 2.0.1

* Fixed corruption of large entries when zip64 is used. [Issue
  111](https://github.com/mrkkrp/zip/issues/111).

## Zip 2.0.0

* Unified `BZip2Unsupported` and `ZstdUnsupported` into a single data
  constructor `UnsupportedCompressionMethod` with a `CompressionMethod`
  field.

## Zip 1.7.2

* Now the ZIP64 extra field is only written when it is necessary. Previously
  it was written unconditionally and it confused some tools.

## Zip 1.7.1

* Fixed compilation with zstd and/or bzip2 disabled.

## Zip 1.7.0

* Set user permissions on linux platform as follows: if an existing file is
  added, use its permissions; if an entry is generated from a bytestring or
  a stream, use 0600. This behavior mimics the zip utility.

## Zip 1.6.0

* Added support for Zstandard (zstd) compression

* Added a Cabal flag `-fdisable-zstd` to remove the zstd C library
  dependency and hence support for Zstd entries.

## Zip 1.5.0

* Added the `packDirRecur'` function.

* Dropped support for GHC 8.4.

## Zip 1.4.1

* Fixed the build on Mac.

## Zip 1.4.0

* The “version made by” info inside archive now correctly sets Unix as the
  OS that produced the archive when the library is compiled on Unix. This
  allows other utilities such as `unzip` to read and correctly restore file
  permissions. [Issue 62](https://github.com/mrkkrp/zip/issues/62).

* Added the `Codec.Archive.Zip.Unix` module.

## Zip 1.3.2

* Fix a bug where removing a temporary file failed in the prescence of
  async exceptions.

## Zip 1.3.1

* The test suite is now faster.

## Zip 1.3.0

* Dropped support for GHC 8.2 and older.

* Added a Cabal flag `-fdisable-bzip2` to remove the bzip2 C library
  dependency and hence support for BZip2 entries.

## Zip 1.2.0

* Added the `setExternalFileAttrs` function and the `edExternalFileAttrs`
  field in the `EntryDescription` record.

## Zip 1.1.0

* Made `saveEntry` and `unpackInto` restore modification time of files.

## Zip 1.0.0

* Works with `conduit-1.3.0`, `conduit-extra-1.3.0`, `resourcet-1.2.0` and
  `bzlib-conduit-0.3.0`.

* Stop depending on `path`, `path-io`, and `plub-b`.

* Made the module `Codec.Archive.Zip.Type` non-public.

* Remove derived instances of `Data` and `Generic` for `EntrySelector` not
  to expose its inner structure.

* Change signature of the `loadEntry` function, its second argument is now
  just `EntrySelector` of the entry to add.

* The second argument of `packDirRecur` now receives paths that are relative
  to the root of the directory we pack.

## Zip 0.2.0

* Added `MonadBase` and `MonadBaseControl` instances for the `ZipArchive`
  monad. Also exported the `ZipState` type without revealing its data
  constructor and records.

* Dropped `MonadThrow` and `MonadCatch` constraints for `createArchive` and
  `withArchive`.

## Zip 0.1.11

* Minor refactoring.

* Improved documentation and metadata.

## Zip 0.1.10

* Made `getEntrySource` polymorphic in terms of the `Source` it returns.

* Numerous cosmetic corrections to the docs.

* Derived `Eq` and `Ord` for `EntrySelectorException` and `ZipException`.

## Zip 0.1.9

* Fixed a bug with modification time serialization on 32 bit systems.

## Zip 0.1.8

* Fixed a bug that caused `zip` to write incorrect number of entries
  (instead of `0xffff`) in central directory when Zip64 feature is enabled.

## Zip 0.1.7

* Fix literal overflows on 32 bit systems.

## Zip 0.1.6

* Allowed `time-1.7`.

* Fixed an issue when empty archives with Zip 64 feature enabled could not
  be read (the “Parsing of archive structure failed: Cannot locate end of
  central directory”).

## Zip 0.1.5

* Switched to using `withBinaryFile` instead of `withFile`, because the
  latter does nasty conversions on Windows, see docs for `openBinaryFile`.

## Zip 0.1.4

* Added several simple code examples in `Codec.Archive.Zip`.

* Derived `Typeable`, `Data`, `Generic` for `EntrySelector`.

* Derived `Typeable` for `EntryDescription`.

* Derived `Show`, `Ord`, `Bounded`, `Data`, and `Typeable` for
  `CompressionMethod`.

* Derived `Read`, `Ord`, `Typeable`, and `Data` for `ArchiveDescription`.

## Zip 0.1.3

* Improved speed of detection of invalid archives.

* Introduced `getEntrySource` function.

## Zip 0.1.2

* Relaxed dependency on `semigroups`.

* Added explicit check of “version needed to extract”, so if archive uses
  some advanced features that we do not support yet, parsing fails.

* Value of “version needed to extract” field is now calculated dynamically
  with respect to actually used features, e.g. if you just store or deflate
  a not very big file, `2.0` version will be written (previously we wrote
  `4.6` unconditionally). This is needed to avoid scaring tools that can
  only handle basic Zip archives.

## Zip 0.1.1

* Make decoding of CP437 faster.

## Zip 0.1.0

* Initial release.
