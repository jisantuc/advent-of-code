{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, utils, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc94";
          packageDependencies = (ps: [
            ps.containers
            ps.megaparsec
            ps.text
            ps.vector
          ]);
          devDependencies = with pkgs.haskell.packages.${compiler}; [
            cabal-install
            cabal-fmt
            haskell-language-server
            hlint
          ];
          testDependencies = (ps: [
            ps.hspec
            ps.hspec-discover
          ]);
          haskell = pkgs.haskell.packages.${compiler}.ghcWithPackages
            (ps: packageDependencies ps ++ testDependencies ps);
        in
        {
          devShells.default = pkgs.mkShell
            {
              packages = [ haskell ] ++ devDependencies;
            };

          packages.default = pkgs.haskell.packages.${compiler}.callCabal2nix "aoclib" ./. { };
        }
      );
}
