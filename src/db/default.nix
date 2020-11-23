{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

buildGoModule rec {
  # TODO(stevan): Why can't we change pname to 'db'?!
  pname = "detsys-db";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  # TODO(stevan): This can be tighter, we only need the db and lib directories
  # from `./..`.
  src = lib.sourceFilesBySuffices ./.. [ ".go" ".mod" ".sum" ".sql" ];
  vendorSha256 = "00808wzgjb6jzb7kdjg209lzlqk2ipbqpbi87d4cc0iqnr37w9h1";

  preConfigure = "cd db";

  # We need CGO to include sqlite.
  preBuild = "export CGO_ENABLED=1";

  postInstall = "cp -r $src/db/migrations $out";
}
