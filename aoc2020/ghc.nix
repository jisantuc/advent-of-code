with import <nixpkgs> {};

pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
        # actual project dependencies
        bytestring megaparsec text parser-combinators containers
        # build / environment-required tools
        haddock ghcide
])
