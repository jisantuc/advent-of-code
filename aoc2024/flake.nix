{
  inputs = {
    nixpkgs.follows = "aoclib/nixpkgs";
    utils.url = "github:numtide/flake-utils";
    aoclib = {
      type = "github";
      owner = "jisantuc";
      repo = "aoclib";
      ref = "main";
    };
  };

  outputs = { nixpkgs, utils, aoclib, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc98";
          haskellPackages = pkgs.haskell.packages.${compiler};
          devDependencies = with haskellPackages; [
            cabal2nix
            cabal-fmt
            cabal-gild
            cabal-install
            haskell-language-server
            hlint
            ormolu
          ];
        in
        {
          devShells.default = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "aoclib" ./. { aoclib = aoclib.packages.${system}.default; }) ];
            nativeBuildInputs = devDependencies;
            withHoogle = true;
          };

          devShells.ci = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "aoclib" ./. { aoclib = aoclib.packages.${system}.default; }) ];
            nativeBuildInputs = with haskellPackages; [ cabal-install ];
          };

          packages.default = haskellPackages.callCabal2nix "aoclib" ./. { };
        }
      );
}

