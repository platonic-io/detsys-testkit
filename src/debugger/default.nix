{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
  detsysLib = callPackage ../lib/default.nix {};
in

buildGoModule rec {
  pname = "detsys-debug";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];
  propagatedBuildInputs = [ plantuml ];

  vendorSha256 = "0p4kpc3srbby7l1nv3n8szaqm0m28m44qd56zw2nhfyg3430x8ci";

  subPackages = [ "cmd/detsys-debug" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # We need CGO to include sqlite.
    export CGO_ENABLED=1
  '';
}
