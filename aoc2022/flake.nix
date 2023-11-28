{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
    aoclib.url = "/home/james/gitdirs/aoclib";
  };

  outputs = { nixpkgs, utils, aoclib, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc94";
          haskellPackages = pkgs.haskell.packages.${compiler}.extend (final: prev: {
            aoclib = aoclib.packages.${system}.default;
          });
          packageDependencies = (ps: [
            ps.aoclib
            ps.containers
            ps.megaparsec
            ps.parallel
            ps.text
            ps.vector
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
