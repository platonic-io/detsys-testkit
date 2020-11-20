{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  ldfi = callPackage ./release.nix {
    pythonPackages = python38Packages;
  };
in ldfi
