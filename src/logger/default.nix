{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
  detsysLib = callPackage ../lib/default.nix {};
in

buildGoModule rec {
  pname = "logger";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];

  vendorSha256 = "1wy39jk78pd4512y9kimp251aiq7h0xcnbiwaaw1k006b00n8cfk";

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # We need CGO to include sqlite.
    export CGO_ENABLED=1
  '';

  postInstall = ''
    # We can't just use -o in buildFlags, because they get passed to both `go
    # build` and `go install` and the latter doesn't accept the -o flag.
    echo "renaming executable"
    mv $out/bin/reactor_${pname} $out/bin/detsys-${pname}
  '';
}
