{ nixpkgs ? import <nixpkgs> { }
, compiler ? "ghc8107"
, extraToolDeps ? [ ]
}:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./aoc2021.nix { extraToolDeps = extraToolDeps; }
