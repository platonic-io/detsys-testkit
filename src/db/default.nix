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

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];
  propagatedBuildInputs = [ sqlite-interactive ];

  vendorSha256 = "1kx9h90crkmha1nm8fkw5h9jycyf4hb74r9yvlgyf1xsvifwjs81";

  buildFlagsArray =
    # This is a dummy git hash to avoid breaking the nix cache, it will be
    # patched in the `postInstall` phase of the top-level `default.nix`.
    [ "-ldflags=-X main.version=0000000000000000000000000000000000000000-nix" ];

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
