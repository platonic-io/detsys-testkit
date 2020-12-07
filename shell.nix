{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

let
  cli       = callPackage ./src/cli/default.nix {};
  db        = callPackage ./src/db/default.nix {};
  debugger  = callPackage ./src/debugger/default.nix {};
  ldfi      = callPackage ./src/ldfi/default.nix {};
  checker   = callPackage ./src/checker/default.nix {};
  scheduler = callPackage ./src/scheduler/default.nix {};
in

pkgs.mkShell {
  name = "dev-shell";

  buildInputs = [
    cli
    db
    debugger
    ldfi
    checker
    scheduler

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
