{ mkDerivation
, aoclib
, base
, containers
, glibc
, hpack
, hspec
, hspec-discover
, lib
, megaparsec
, parallel
, raw-strings-qq
, text
, vector
, extraToolDeps ? [ ]
}:
mkDerivation {
  pname = "aoc2022";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ aoclib base containers megaparsec parallel text vector ];
  libraryToolDepends = [ hpack glibc ];
  executableHaskellDepends = [ base containers megaparsec parallel text vector ];
  testHaskellDepends = [
    base
    containers
    hspec
    hspec-discover
    megaparsec
    parallel
    raw-strings-qq
    text
    vector
  ];
  testToolDepends = [ hspec-discover ] ++ extraToolDeps;
  prePatch = "hpack";
  homepage = "https://github.com/jisantuc/advent-of-code#readme";
  license = lib.licenses.mit;
}
