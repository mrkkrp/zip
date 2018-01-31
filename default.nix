{ mkDerivation, base, bytestring, bzlib-conduit, case-insensitive
, cereal, conduit, conduit-extra, containers, criterion, digest
, exceptions, filepath, hspec, mtl, path, path-io, plan-b
, QuickCheck, resourcet, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "zip";
  version = "0.1.3";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring bzlib-conduit case-insensitive cereal conduit
    conduit-extra containers digest exceptions filepath mtl path
    path-io plan-b resourcet text time transformers
  ];
  testHaskellDepends = [
    base bytestring conduit containers exceptions filepath hspec path
    path-io QuickCheck text time transformers
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/mrkkrp/zip";
  description = "Operations on zip archives";
  license = stdenv.lib.licenses.bsd3;
}
