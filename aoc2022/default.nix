{ nixpkgs ? import <nixpkgs> { }
, compiler ? "ghc94"
, extraToolDeps ? [ ]
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./aoc2022.nix { extraToolDeps = extraToolDeps; }
