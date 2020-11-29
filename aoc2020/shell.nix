with import <nixpkgs> {};

{ pkgs ? import <nixpkgs> {} }:
  let
    ghc = import ./ghc.nix;
  in
    pkgs.mkShell {
      buildInputs = [ pkgs.stack
                      pkgs.ormolu 
                      ghc ];
}
