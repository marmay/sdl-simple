{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107", doBenchmark ? false }:
(import ./default.nix { inherit nixpkgs compiler; }).env
