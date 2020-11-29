with import <nixpkgs> {};

pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
        # actual project dependencies
        bytestring megaparsec
        # build / environment-required tools
        haddock ghcide
])
