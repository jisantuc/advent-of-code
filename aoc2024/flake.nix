{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-24.05";
    utils.url = "github:numtide/flake-utils";
    aoclib.url = "github:jisantuc/aoclib";
  };

  outputs = { nixpkgs, utils, aoclib, ... }:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ]
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          compiler = "ghc98";
          haskellPackages = pkgs.haskell.packages.${compiler}.extend(final: prev: {
            aoclib = aoclib.packages.${system}.default;
          });
          devDependencies = with haskellPackages; [
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
            packages = ps: [ (ps.callCabal2nix "aoclib" ./. { }) ];
            nativeBuildInputs = devDependencies;
            withHoogle = true;
          };

          devShells.ci = haskellPackages.shellFor {
            packages = ps: [ (ps.callCabal2nix "aoclib" ./. { }) ];
            nativeBuildInputs = with haskellPackages; [ cabal-install ];
          };

          packages.default = haskellPackages.callCabal2nix "aoclib" ./. { };
        }
      );
}

