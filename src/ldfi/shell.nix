{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

( let
    inherit (import sources.gitignore {}) gitignoreSource;
    ldfi = callPackage ./release.nix {
      pythonPackages = python38Packages;
      gitignoreSource = gitignoreSource;
    };
  in python38.withPackages (ps: [ ldfi ])
).env
