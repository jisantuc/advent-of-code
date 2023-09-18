{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc94" }:
(import ./default.nix {
  extraToolDeps = [
    nixpkgs.cabal-install
    nixpkgs.haskell.compiler.ghc94
    nixpkgs.haskell.packages.ghc94.haskell-language-server
    nixpkgs.haskellPackages.hlint
    nixpkgs.haskellPackages.ormolu
  ]; inherit nixpkgs compiler;
}).env
