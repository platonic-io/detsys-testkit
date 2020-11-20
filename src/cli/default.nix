{ pkgs ? import <nixpkgs> {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

buildGoModule rec {
  pname = "detsys";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys/cli";
  src = ./.;

  debugger = callPackage ../debugger/default.nix {};
  db = callPackage ../db/default.nix {};

  buildInput = [ debugger db ];

  # If vendorSha256 is null, then we won't fetch any dependencies and
  # rely on the vendor folder within the source.
  vendorSha256 = null;

  # Statically linked.
  preBuild = ''
    export CGO_ENABLED=0
  '';
}
