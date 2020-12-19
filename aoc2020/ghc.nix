with import <nixpkgs> {};

pkgs.haskellPackages.ghcWithPackages (ps: with ps; [
        # actual project dependencies
        bytestring megaparsec text parser-combinators containers
        vector
        # build / environment-required tools
        ghcide implicit-hie
])
