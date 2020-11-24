{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

buildGoModule rec {
  pname = "detsys";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = lib.cleanSourceWith {
    filter = lib.cleanSourceFilter;
    src = lib.cleanSourceWith {
      filter = with pkgs.stdenv;
        name: type: let baseName = baseNameOf (toString name); in
                    baseName == "cli" ||
                    baseName == "cmd" ||
                    baseName == "lib" ||
                    lib.hasSuffix ".go" name ||
                    lib.hasSuffix ".mod" name ||
                    lib.hasSuffix ".sum" name;
      src = ../.;
    };
  };

  modRoot = "cli";
  vendorSha256 = "1wr4ddjjclc7asbwxh9rlvvwx0dkzmd8azkm03c04s46a427fk4g";

  # Static linking.
  preBuild = "export CGO_ENABLED=0";
}
