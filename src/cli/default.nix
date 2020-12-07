{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

buildGoModule rec {
  pname = "detsys";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ../.;

  modRoot = "cli";
  vendorSha256 = "1wr4ddjjclc7asbwxh9rlvvwx0dkzmd8azkm03c04s46a427fk4g";

  # Static linking.
  preBuild = "export CGO_ENABLED=0";
}
