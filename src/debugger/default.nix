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
  version = lib.commitIdFromGitRepo ./../../.git;
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];
  propagatedBuildInputs = [ plantuml ];

  vendorSha256 = "1m0f0gxsky5yxj4di94lpd4wjbmc9aqr2kkv2r9dyq72zixbbcl4";

  subPackages = [ "cmd/detsys-debug" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # We need CGO to include sqlite.
    export CGO_ENABLED=1
  '';
}
