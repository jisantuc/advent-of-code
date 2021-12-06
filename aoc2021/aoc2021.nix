{ mkDerivation
, base
, containers
, glibc
, hpack
, hspec
, hspec-discover
, lib
, megaparsec
, text
, vector
, extraToolDeps ? [ ]
}:
mkDerivation {
  pname = "aoc2021";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers megaparsec text vector ];
  libraryToolDepends = [ hpack glibc ];
  executableHaskellDepends = [ base containers megaparsec text vector ];
  testHaskellDepends = [
    base
    containers
    hspec
    hspec-discover
    megaparsec
    text
    vector
  ];
  testToolDepends = [ hspec-discover ] ++ extraToolDeps;
  prePatch = "hpack";
  homepage = "https://github.com/jisantuc/aoc2021#readme";
  license = lib.licenses.mit;
}
