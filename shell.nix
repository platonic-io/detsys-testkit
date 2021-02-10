{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

let
  pythonEnv = python38.withPackages (ps: [ ps.pip ]);
  fake-lsb-release = pkgs.writeScriptBin "lsb_release" ''
    #!${pkgs.runtimeShell}

    case "$1" in
      -i) echo "nixos";;
      -r) echo "nixos";;
    esac
  '';
in

pkgs.mkShell {
  name = "dev-shell";

  buildInputs = [
    bazel_3
    buildifier # Bazel BUILD file formatter
    go
    clojure
    pythonEnv
    fake-lsb-release
    mypy
    haskellPackages.ghc
    haskellPackages.cabal-install

    git
    nix
    niv
    direnv
    lorri
    nix-index
  ];
}
