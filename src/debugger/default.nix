{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

buildGoModule rec {
  pname = "detsys-debug";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ../.;

  propagatedBuildInputs = [ plantuml ];

  # NOTE: `nix-hash --base32 --type sha256 vendor`
  vendorSha256 = "14g97i7ykxrxwdmdg51kfbfdp6z4r1ng9pk75hncns45cf94p67j";

  modRoot = "debugger";
  subPackages = [ "cmd/detsys-debug" ];

  # We need CGO to include sqlite.
  preBuild = ''
    export CGO_ENABLED=1
  '';
}
