{ nixpkgs ? import <nixpkgs> { }
, compiler ? "ghc90"
, extraToolDeps ? [ ]
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./aoc2022.nix { extraToolDeps = extraToolDeps; }
