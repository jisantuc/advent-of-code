{ nixpkgs ? import <nixpkgs> { }, compiler ? "ghc8107" }:
(import ./default.nix { extraToolDeps = [ nixpkgs.cabal-install nixpkgs.ormolu ]; inherit nixpkgs compiler; }).env
