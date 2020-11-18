{ pkgs ? import <nixpkgs> {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

buildGoModule rec {
  pname = "detsys-debugger";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys/debugger";
  src = ./.;

  subPackages = [ "cmd/detsys-debug" ];

  # If vendorSha256 is null, then we won't fetch any dependencies and
  # rely on the vendor folder within the source.
  vendorSha256 = null;

  # We need CGO to include sqlite.
  preBuild = ''
    export CGO_ENABLED=1
  '';
}
