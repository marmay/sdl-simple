{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc8107", doBenchmark ? false }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./sdl-simple.nix {}
