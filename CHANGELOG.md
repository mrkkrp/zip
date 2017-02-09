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
