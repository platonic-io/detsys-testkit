{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

pkgs.mkShell {
  name = "dev-shell";

  buildInputs = [
    bazel
    go
    clojure
    python38

    plantuml
    git
    nix
    niv
    direnv
    lorri
    nix-index
  ];
}
