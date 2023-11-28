{ nixpkgs ? import <nixpkgs> { }
, compiler ? "ghc94"
, extraToolDeps ? [ ]
}:
let
  aoclib = (import ../aoclib { inherit nixpkgs; }).packages.default;
  packages = nixpkgs.pkgs.haskell.packages.${compiler}.extend (final: prev: {
    aoclib = aoclib;
  });
in
packages.callPackage ./aoc2022.nix { inherit extraToolDeps; }
