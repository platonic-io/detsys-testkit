{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
  detsysLib = callPackage ../lib/default.nix {};
in

buildGoModule rec {
  pname = "db";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];
  propagatedBuildInputs = [ sqlite-interactive ];

  vendorSha256 = "1wb5jybpcsis9sb322kl4c6y6r122bw1bv8gpiazqx6ls11hrpz9";

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # We need CGO to include sqlite.
    export CGO_ENABLED=1

    # Enable json support for sqlite.
    export CGO_CFLAGS="-DSQLITE_ENABLE_JSON1"
  '';

  postInstall = ''
    echo "installing migrations"
    cp -r $src/migrations $out

    # We can't just use -o in buildFlags, because they get passed to both `go
    # build` and `go install` and the latter doesn't accept the -o flag.
    echo "renaming executable"
    mv $out/bin/db $out/bin/detsys-db
  '';
}
