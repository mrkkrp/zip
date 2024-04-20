# Zip

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/zip.svg?style=flat)](https://hackage.haskell.org/package/zip)
[![Stackage Nightly](http://stackage.org/package/zip/badge/nightly)](http://stackage.org/nightly/package/zip)
[![Stackage LTS](http://stackage.org/package/zip/badge/lts)](http://stackage.org/lts/package/zip)
![CI](https://github.com/mrkkrp/zip/workflows/CI/badge.svg?branch=master)

* [Why this library was written](#why-this-library-was-written)
    * [zip-archive](#zip-archive)
    * [LibZip](#libzip)
    * [zip-conduit](#zip-conduit)
* [Features](#features)
    * [Compression methods](#compression-methods)
    * [Encryption](#encryption)
    * [Sources of file data](#sources-of-file-data)
    * [ZIP64](#zip64)
    * [Filenames](#filenames)
    * [Meta-information about files](#meta-information-about-files)
* [Quick start](#quick-start)
* [Contribution](#contribution)
* [License](#license)

This is a feature-rich, memory-efficient, and type-safe library to
manipulate Zip archives. The library was created with large multimedia data
in mind and provides all features users might expect, comparable in terms of
feature-set with libraries like `libzip` in C.

## Why this library was written

There are a few libraries to work with Zip archives, yet every one of them
provides only a subset of useful functionality or otherwise is flawed in
some way so it cannot be easily used in some situations. Let's examine all
libraries available on Hackage to understand the motivation for this
package.

### zip-archive

`zip-archive` is a widely used library. It's quite old, well-known and
simple to use. However, it creates Zip archives purely, as `ByteStrings`s in
memory. This is not acceptable if you work with big data. For example, if
you have a collection of files with the total size 500 MB and you want to
pack them into an archive, you can easily consume up to 1 GB of memory (the
files plus the resulting archive). This is not always affordable. Even if
you want just to look at the list of archive entries it will read the entire
archive into memory.

### LibZip

This is a binding to the C library [`libzip`][libzip]. There is always a
certain kind of trouble with bindings. For example, you need to ensure that
the target library is installed and its version is compatible with the
version of your binding.

It's not that bad with libraries that do not break their API for years, but
it's not the case with `libzip`. As the maintainer of `LibZip` puts it:

> libzip 0.10, 0.11, and 1.0 are not binary compatible. If your C library is
> 0.11.x, then you should use LibZip 0.11. If your C library is 1.0, then
> you should use LibZip master branch (not yet released to Hackage).

Now, on my machine I have the version 1.0. To put the package on Stackage we
had to use the version 0.10, because Stackage uses Ubuntu to build packages
and libraries on Ubuntu are always ancient. This means that I cannot use the
version of the library from Stackage, and I don't yet know what will be on
the server.

After much frustration, I decided to avoid using `LibZip`. After all, this
is not a project that shouldn't be done completely in Haskell. By rewriting
this in Haskell, I also can make it safer to use.

### zip-conduit

This one uses the right approach: leverage a good streaming library
(`conduit`) for memory-efficient processing. The library is however not
feature-rich and has certain problems (including the programming style, it
uses `error` if an entry is missing in the archive, among other things),
some of them are reported on its issue tracker. It also does not appear to
be maintained (the last sign of activity was on December 23, 2014).

## Features

The library supports all features specified in the modern .ZIP specification
except for encryption and multi-disk archives. See more about this below.

For reference, here is a [copy of the specification][specification].

### Compression methods

`zip` supports the following compression methods:

* Store (no compression, just store files “as is”)
* [DEFLATE]
* [Bzip2]
* [Zstandard]

The best way to add a new compression method to the library is to write a
conduit that will do the compression and publish it as a library. `zip` can
then depend on it and add it to the list of supported compression methods.
The current list of compression methods reflects what is available on
Hackage at the moment.

### Encryption

Encryption is currently not supported. Encryption system described in the
.ZIP specification is known to be seriously flawed, so it's probably not the
best way to protect your data anyway. The encryption method seems to be a
proprietary technology of PKWARE (at least that's what stated about it in
the .ZIP specification), so to hell with it.

### Sources of file data

The following sources are supported:

* *File name.* This is an efficient method to perform compression or
  decompression. You specify where to get data or where to save it and the
  rest is handled by the library.
* *Conduit source or sink.*
* *ByteString.* Use it only with small data.
* *Copy file from another archive.* An efficient operation, file is copied
  “as is”—no re-compression is performed.

### ZIP64

When necessary, the `ZIP64` extension is automatically used. It's necessary
when:

* The total size of the archive is greater than 4 GB.
* The size of a single compressed/uncompressed file in the archive is
  greater than 4 GB.
* There are more than 65535 entries in the archive.

The library is particularly well suited for processing large files. For
example, I've been able to create 6.5 GB archive with reasonable speed and
without significant memory consumption.

### Filenames

The library has an API that makes it impossible to create archive with
non-portable or invalid file names in it.

As of .ZIP specification 6.3.2, files with Unicode symbols in their names
can be stored in Zip archives. The library supports mechanisms for this and
uses them automatically when needed. Besides UTF-8, CP437 is also supported
as per the specification.

### Meta-information about files

The library allows us to attach comments to the entire archive or individual
files, and also gives its user full control over extra fields that are
written to file headers, so the user can store arbitrary information about
files in the archive.

## Quick start

The module `Codec.Archive.Zip` provides everything you may need to
manipulate Zip archives. There are three things that should be clarified
right away to avoid confusion.

First, we use the `EntrySelector` type that can be obtained from relative
`FilePath`s (paths to directories are not allowed). This method may seem
awkward at first, but it will protect you from the problems with portability
when your archive is unpacked on a different platform.

Second, there is no way to add directories, or to be precise, *empty
directories* to your archive. This approach is used in Git and I find it
sane.

Finally, the third feature of the library is that it does not modify the
archive instantly, because doing so on every manipulation would often be
inefficient. Instead, we maintain a collection of pending actions that can
be turned into an optimized procedure that efficiently modifies the archive
in one pass. Normally, this should be of no concern to you, because all
actions are performed automatically when you leave the `ZipArchive` monad.
If, however, you ever need to force an update, the `commit` function is your
friend.

Let's take a look at some examples that show how to accomplish most common
tasks.

To get full information about archive entries, use `getEntries`:

```haskell
λ> withArchive archivePath (M.keys <$> getEntries)
```

This will return a list of all entries in the archive at `archivePath`. It's
possible to extract contents of an entry as a strict `ByteString`:

```haskell
λ> withArchive archivePath (getEntry entrySelector)
```

…to stream them to a given sink:

```haskell
λ> withArchive archivePath (sourceEntry entrySelector mySink)
```

…to save a specific entry to a file:

```haskell
λ> withArchive archivePath (saveEntry entrySelector pathToFile)
```

…and finally just unpack the entire archive into a directory:

```haskell
λ> withArchive archivePath (unpackInto destDir)
```

See also `getArchiveComment` and `getArchiveDescription`.

Modifying is also easy. When you want to create a new archive use
`createArchive`, otherwise `withArchive` will do. To add an entry from
`ByteString`:

```haskell
λ> createArchive archivePath (addEntry Store "Hello, World!" entrySelector)
```

You can stream from `Source` as well:

```haskell
λ> createArchive archivePath (sinkEntry Deflate source entrySelector)
```

To add contents from a file, use `loadEntry`:

```haskell
λ> let toSelector = const (mkEntrySelector "my-entry.txt")
λ> createArchive archivePath (loadEntry BZip2 toSelector myFilePath)
```

Finally, you can copy an entry from another archive without re-compression
(unless you use `recompress`, see below):

```haskell
λ> createArchive archivePath (copyEntry srcArchivePath selector selector)
```

It's often desirable to just pack a directory:

```haskell
λ> createArchive archivePath (packDirRecur Deflate mkEntrySelector dir)
```

It's also possible to:

* rename an entry with `renameEntry`
* delete an entry with `deleteEntry`
* change compression method with `recompress`
* change comment associated with an entry with `setEntryComment`
* delete comment with `deleteEntryComment`
* set modification time with `setModTime`
* manipulate extra fields with `addExtraField` and `deleteExtraField`
* check if entry is intact with `checkEntry`
* undo changes with `undoEntryCanges`, `undoArchiveChanges`, and `undoAll`
* force changes to be written to file system with `commit`

This should cover all your needs. Feel free to open an issue if you're
missing something.

## Contribution

You can contact the maintainer via [the issue
tracker](https://github.com/mrkkrp/zip/issues).

Pull requests are welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.

[libzip]: https://en.wikipedia.org/wiki/Libzip
[specification]: https://pkware.cachefly.net/webdocs/APPNOTE/APPNOTE-6.3.3.TXT
[DEFLATE]: https://en.wikipedia.org/wiki/DEFLATE
[Bzip2]: https://en.wikipedia.org/wiki/Bzip2
[Zstandard]: https://en.wikipedia.org/wiki/Zstandard
