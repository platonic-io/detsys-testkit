{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

buildGoModule rec {
  pname = "lib";
  version = lib.commitIdFromGitRepo ./../../.git;
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";
  src = gitignoreSource ./.;
  vendorSha256 = null;

  postInstall = ''
    mkdir -p $out
    cp -R $src $out/src
  '';
}
