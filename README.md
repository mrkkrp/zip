# Zip

*Work in progress.*

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/zip.svg?style=flat)](https://hackage.haskell.org/package/zip)
[![Stackage Nightly](http://stackage.org/package/zip/badge/nightly)](http://stackage.org/nightly/package/zip)
[![Build Status](https://travis-ci.org/mrkkrp/zip.svg?branch=master)](https://travis-ci.org/mrkkrp/zip)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/zip/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/zip?branch=master)

* [Why this library is written](#why-this-library-is-written)
    * [zip-archive](#zip-archive)
    * [LibZip](#libzip)
    * [zip-conduit](#zip-conduit)
* [Features](#features)
    * [Compression methods](#compression-methods)
    * [Encryption](#encryption)
    * [Sources of file data](#sources-of-file-data)
    * [ZIP64](#zip64)
    * [Unicode in filenames](#unicode-in-filenames)
    * [Meta-information about files](#meta-information-about-files)
    * [File names](#file-names)
* [Quick start](#quick-start)
* [Contribution](#contribution)
* [License](#license)

This is a feature-rich, memory-efficient, and type-safe library to
manipulate Zip archives in Haskell. The library is specially written to be
production-quality and it's long-term supported. In particular, it's created
with large multimedia data in mind and provides all features users might
expect, comparable in terms of feature-set with libraries like `libzip` in
C.

## Why this library is written

There are a few libraries to work with Zip archives, yet every one of them
provides only subset of all functionality user may need (obviously the
libraries provide functionality that their authors needed) and otherwise is
flawed in some way so it cannot be easily used in some situations. Let's
examine all libraries available on Hackage to understand motivation for this
package.

### zip-archive

`zip-archive` is a widely used library. It's quite old, well-known and
simple to use. However it creates Zip archives purely, as `ByteStrings`s in
memory that you can then write to the file system. This is not acceptable if
you work with more-or-less large data. For example, if you have collection
of files with total size of 500 MB and you want to pack them into an
archive, you can easily consume up to 1 GB of memory (files plus resulting
archive). Not always you can afford to do this or do this at scale. Good
news is that Haskell now has mature libraries for stream processing that can
be used to do the same thing in constant memory. More about this below.

### LibZip

This is bindings to C library
[`libzip`](https://en.wikipedia.org/wiki/Libzip). There is always certain
kind of trouble when you are using bindings. For example, you need to take
care that target library is installed and its version is compatible with
version of your binding. Yes, this means additional headaches. It should be
just “plug and play” (if you're using Stack), but now you need to watch out
for compatibility.

It's not that bad with libraries that do not break their API for years, but
it's not the case with `libzip`. As maintainer of `LibZip` puts it:

> libzip 0.10, 0.11, and 1.0 are not binary compatible. If your C library is
> 0.11.x, then you should use LibZip 0.11. If your C library is 1.0, then
> you should use LibZip master branch (not yet released to Hackage).

Now, on my machine I have version 1.0. To put the package on Stackage we had
to use version 0.10, because Stackage uses Ubuntu to build packages and
libraries on Ubuntu are always ancient. This means that I cannot use version
of the library from Stackage, and I don't yet know what will be on the
server.

After much frustration with all these things I decided to avoid using of
`LibZip`, because after all, this is not that sort of project that shouldn't
be done in pure Haskell.

### zip-conduit

This one uses the right approach: leverage good streaming library
(`conduit`) for memory-efficient processing in pure Haskell. This is however
is not feature-rich and has certain problems (including programming style),
some of them are reported on its issue tracker. It also does not appear to
be maintained (last sign of activity was on December 23, 2014).

## Features

The library supports all features specified in modern Zip
specifications. The only feature that is not currently supported is
encryption, see more about this below.

For reference, here is a [copy of the specification](https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT).

### Compression methods

`zip` supports the following compression methods:

* Store (no compression, just store files “as is”)
* [DEFLATE](https://en.wikipedia.org/wiki/DEFLATE)
* [Bzip2](https://en.wikipedia.org/wiki/Bzip2)

### Encryption

Encryption is currently not supported. Encryption system described in Zip
specification is known to be seriously flawed, so it's probably not the best
way to protect your data anyway. The encryption method seems to be
proprietary technology of PKWARE, so to hell with it.

### Sources of file data

The library gives you many options how to get file contents to compress and
how to get extracted data. The following methods are supported:

* *File name.* This is an efficient method to perform compression or
  decompression. You just specify where to get data or where to save it and
  the rest is handled by the library.

* *Conduit source or sink.*

* *ByteString.* Use it only with small files.

### ZIP64

When necessary, the `ZIP64` extension is automatically enabled. It's
necessary when anything from this list takes place:

* Total size of archive is larger than 4 GB.

* Size of compressed/uncompressed file in archive is greater than 4 GB.

* There are more than 65535 entries in the archive.

* Size of file contents is not known in advance (for example when `conduit`
  is used as file source).

### Unicode in filenames

As of .ZIP specification 6.3.2, files with Unicode symbols in their names
can be put into Zip archives. The library supports mechanisms for this and
uses them automatically when needed.

### Meta-information about files

The library allows to attach comments to entire archive or individual files,
and also gives its user full control over extra fields that are written to
file headers, so the user can store arbitrary information about file in the
archive.

### File names

The library uses API that makes it impossible to create archive with
non-portable or invalid file names in it.

## Quick start

*Coming soon…*

## Contribution

Feel free to contact the maintainer via
[the issue tracker](https://github.com/mrkkrp/zip/issues).

We are open to pull requests, they will be merged quickly if they are good!

## License

Copyright © 2016 Mark Karpov

Distributed under BSD 3 clause license.
