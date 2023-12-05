{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    aoclib.url = "github:jisantuc/aoclib";
  };

  outputs = { nixpkgs, utils, aoclib, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc94";
          haskellPackages = pkgs.haskell.packages.${compiler}.extend (final: prev: {
            aoclib = aoclib.packages.${system}.default;
          });
          packageDependencies = (ps: [
            ps.aoclib
            ps.bytestring
            ps.containers
            ps.megaparsec
            ps.mtl
            ps.optparse-applicative
            ps.raw-strings-qq
            ps.text
            ps.utf8-string
            ps.vector
            ps.word8
          ]);
          devDependencies = with haskellPackages; [
            cabal-install
            cabal-fmt
            haskell-language-server
            hlint
          ];
          testDependencies = (ps: [
            ps.hspec
            ps.hspec-discover
          ]);
          haskell = haskellPackages.ghcWithPackages
            (ps: packageDependencies ps ++ testDependencies ps);
        in
        {
          devShells.default = pkgs.mkShell
            {
              packages = [ haskell ] ++ devDependencies;
            };

          packages.default = haskellPackages.callCabal2nix "aoc2022" ./. { };
        }
      );
}
