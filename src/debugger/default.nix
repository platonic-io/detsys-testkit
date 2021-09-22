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

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];

  vendorSha256 = "19rv6v2aj9qzl07zmrfwr7wkhvvawy3p69ipl58mnbbr6ry8999g";

  buildFlagsArray =
    # This is a dummy git hash to avoid breaking the nix cache, it will be
    # patched in the `postInstall` phase of the top-level `default.nix`.
    [ "-ldflags=-X main.version=0000000000000000000000000000000000000000-nix" ];

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

  checkPhase = ''
    cd internal/
    go test
  '';
}
