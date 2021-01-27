{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

buildGoModule rec {
  pname = "logger";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  src = gitignoreSource ./.;

  vendorSha256 = "1dq4w81g3s26chpfqad3mqpybg4900646h2b2k6xsz295ds9qk7q";

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  preBuild = ''
    # We need CGO to include sqlite.
    export CGO_ENABLED=1
  '';

  postInstall = ''
    # We can't just use -o in buildFlags, because they get passed to both `go
    # build` and `go install` and the latter doesn't accept the -o flag.
    echo "renaming executable"
    mv $out/bin/${pname} $out/bin/detsys-${pname}
  '';
}
