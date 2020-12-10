{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
  detsysLib = callPackage ../lib/default.nix {};
in

buildGoModule rec {
  pname = "detsys-db";
  version = lib.commitIdFromGitRepo ./../../.git;
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ./.;

  buildInputs = [ detsysLib ];
  propagatedBuildInputs = [ sqlite-interactive ];

  vendorSha256 = "0zwqn8k6i9idhp2q8ggmqrs84q7bx108r2031gd7zzjgvfy1na40";

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # We need CGO to include sqlite.
    export CGO_ENABLED=1
  '';

  postInstall = ''
    echo "installing migrations"
    cp -r $src/migrations $out
  '';
}
