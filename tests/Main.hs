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

module Main (main) where

-- import Test.Hspec

main :: IO ()
main = return () -- TODO

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
