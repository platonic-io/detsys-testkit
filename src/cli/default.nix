{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
  detsysLib = callPackage ../lib/default.nix {};
in

buildGoModule rec {
  pname = "detsys";
  version = lib.commitIdFromGitRepo ./../../.git;
  goPackagePath = "github.com/symbiont-io/detsys-testkit/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];

  # This hash should be the output of:
  #   go mod vendor && nix-hash --base32 --type sha256 vendor
  vendorSha256 = "1pr2nscfhpb1m5hlf8rmbxsf60pk09g7byqn2rlai61a2iyy8l1p";

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # Static linking.
    export CGO_ENABLED=0
  '';
}
