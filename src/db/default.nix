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

  vendorSha256 = "1qiw90f58yn9fid0p9zch1l91c2n81xxvdzcdqc3ci3qgqkyijja";

  buildFlagsArray = [ "-ldflags=-X main.version=${version}" ];

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
