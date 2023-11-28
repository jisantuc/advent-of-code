{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc94" }:
(import ./default.nix {
  extraToolDeps = [
    nixpkgs.cabal-install
    nixpkgs.haskell.compiler.${compiler}
    nixpkgs.haskell.packages.${compiler}.haskell-language-server
    nixpkgs.haskell.packages.${compiler}.haskell-dap
    nixpkgs.haskell.packages.${compiler}.haskell-debug-adapter
    # nixpkgs.haskell.packages.${compiler}.hlint
    nixpkgs.haskell.packages.${compiler}.ormolu
  ]; inherit nixpkgs compiler;
}).env
