{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

buildGoModule rec {
  pname = "lib";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";
  src = gitignoreSource ./.;
  vendorSha256 = "1dq4w81g3s26chpfqad3mqpybg4900646h2b2k6xsz295ds9qk7q";

  postInstall = ''
    mkdir -p $out/src
    # Don't copy over bazel build file, or bazel will try to build that
    # directory.
    cp $(ls $src | grep -v BUILD.bazel) $out/src
  '';
}
