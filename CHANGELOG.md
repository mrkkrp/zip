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
