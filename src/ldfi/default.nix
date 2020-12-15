{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  inherit (import sources.gitignore {}) gitignoreSource;
  ldfi = callPackage ./release.nix {
    lib = lib;
    pythonPackages = python38Packages;
    gitignoreSource = gitignoreSource;
  };
in ldfi
