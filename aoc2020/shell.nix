with import <nixpkgs> {};

{ pkgs ? import <nixpkgs> {} }:
  let
    ghc = import ./ghc.nix;
  in
    pkgs.mkShell {
      name = "advent-of-code";
      buildInputs = [ pkgs.stack
                      pkgs.ormolu 
                      ghc ];
}
