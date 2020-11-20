{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

buildGoModule rec {
  pname = "detsys-db";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys/db";
  src = ./.;

  # If vendorSha256 is null, then we won't fetch any dependencies and
  # rely on the vendor folder within the source.
  vendorSha256 = null;

  # We need CGO to include sqlite.
  preBuild = ''
    export CGO_ENABLED=1
  '';

  postInstall = ''
    cp -r $src/migrations $out
  '';
}
