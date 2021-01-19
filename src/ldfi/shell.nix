{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  inherit (import sources.gitignore {}) gitignoreSource;
  ldfi = callPackage ./release.nix {
    pythonPackages = python38Packages;
    gitignoreSource = gitignoreSource;
  };
  pythonEnv = python38.withPackages (ps: [ ldfi ps.pytest ]);
in

mkShell {
  buildInputs = [ pythonEnv mypy black ];
}
