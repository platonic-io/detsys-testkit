{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

pkgs.mkShell {
  name = "dev-shell";

  buildInputs = [
    # PR to update bazel to 3.7.2: https://github.com/NixOS/nixpkgs/pull/105439
    # there seems to be a problem on MacOS though, so for now install bazel
    # 3.7.2 manually outside of nix.

    # PR for 4.0: https://github.com/NixOS/nixpkgs/pull/106984

    # bazel
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
