{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
  detsysLib = callPackage ../lib/default.nix {};
in

buildGoModule rec {
  pname = "debugger";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];

  vendorSha256 = "0ssxxfgckap99n5vfl3z1fjb2sc59wlf1h65l6jgir7zx7ndlzvr";

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  subPackages = [ "cmd/detsys-debug" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # We need CGO to include sqlite.
    export CGO_ENABLED=1

    # enable json support for sqlite
    export CGO_CFLAGS="-DSQLITE_ENABLE_JSON1"
  '';
}
