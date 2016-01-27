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
* [Quick start](#quick-start)
* [Features](#features)
* [Contribution](#contribution)
* [License](#license)

This is a feature-rich, memory-efficient, and type-safe library to
manipulate Zip archives in Haskell. The library is specially written to be
production-quality, it's long-term supported, and complete. In particular,
it's created with large multimedia data in mind and provides all features
users might expect, comparable in terms of feature-set with libraries like
`libzip` in C.

## Why this library is written

There are a few libraries to work with Zip archives, yet every one of them
provides only subset of all functionality user may require (obviously the
libraries provide functionality that their authors needed) and otherwise is
flawed in some way so it cannot be easily used in some situations. Let's
examine all libraries available on Hackage to understand motivation for this
package.

### zip-archive

`zip-archive` is a widely used library. It's quite old, well-known and
simple to use. However it creates Zip archives purely, as `ByteStrings`s in
memory that you can then write to the file system. This is not acceptable if
you work with more-or-less large data. For example, if you have collection
of files with total size of 500 Mb and you want to pack them into an
archive, you can easily consume up to 1 Gb of memory (files plus resulting
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
it's not the case with `ziplib`. As maintainer of `LibZip` puts it:

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
is not feature-rich and has certain problems, reported on its issue
tracker. I would rather contribute to this project only if it was
maintained, but it's not (last sign of activity was on December 23, 2014).

## Quick start

*Coming soon…*

## Features

*Coming soon…*

## Contribution

Feel free to contact the maintainer via
[the issue tracker](https://github.com/mrkkrp/zip/issues).

We are open to pull requests, they will be merged quickly if they are good!

## License

Copyright © 2016 Mark Karpov

Distributed under BSD 3 clause license.
