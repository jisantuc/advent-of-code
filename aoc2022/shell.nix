{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc90" }:
(import ./default.nix {
  extraToolDeps = [
    nixpkgs.cabal-install
    nixpkgs.ormolu
    nixpkgs.haskellPackages.haskell-language-server
    nixpkgs.haskell.compiler.ghc90
  ]; inherit nixpkgs compiler;
}).env
