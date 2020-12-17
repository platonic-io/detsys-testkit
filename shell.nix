{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

let
  checker   = callPackage ./src/checker/default.nix {};
  cli       = callPackage ./src/cli/default.nix {};
  db        = callPackage ./src/db/default.nix {};
  debugger  = callPackage ./src/debugger/default.nix {};
  generator = callPackage ./src/generator/default.nix {};
  ldfi      = callPackage ./src/ldfi/default.nix {};
  scheduler = callPackage ./src/scheduler/default.nix {};
in

pkgs.mkShell {
  name = "dev-shell";

  buildInputs = [
    checker
    cli
    db
    debugger
    generator
    ldfi
    scheduler

    bazel
    go
    clojure
    python38

    git
    nix
    niv
    direnv
    lorri
    nix-index
  ];
}
