{ sources ? import ./../../nix/sources.nix
, nixpkgs ? import sources.nixpkgs {}
, compiler ? "ghc8103" }:

(import ./default.nix { inherit nixpkgs compiler; }).env
