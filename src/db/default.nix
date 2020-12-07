{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

buildGoModule rec {
  pname = "detsys-db";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ../.;

  propagatedBuildInputs = [ sqlite-interactive ];

  # NOTE: If this hash is wrong, we get a cryptic "go: cannot find main module"
  # error...
  vendorSha256 = "00808wzgjb6jzb7kdjg209lzlqk2ipbqpbi87d4cc0iqnr37w9h1";

  preConfigure = "cd db";

  # We need CGO to include sqlite.
  preBuild = "export CGO_ENABLED=1";

  postInstall = "cp -r $src/db/migrations $out";
}
