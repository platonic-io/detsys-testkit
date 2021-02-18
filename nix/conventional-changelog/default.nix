{ sources ? import ./../sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

let
  nodePackages = import ./node-composition.nix {
    inherit pkgs nodejs;
    inherit (stdenv.hostPlatform) system;
  };
in
nodePackages.package
