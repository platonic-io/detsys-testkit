{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

let
  inherit (import sources.gitignore {}) gitignoreFilter;
  customFilter = src:
    let
      # IMPORTANT: use a let binding like this to memoize info about the git directories.
      srcIgnored = gitignoreFilter src;
    in
      path: type:
         srcIgnored path type
         || lib.hasInfix "vendor" path;
in

buildGoModule rec {
  pname = "debugger";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  propagatedBuildInputs = [ plantuml ];

  src = lib.cleanSourceWith {
    filter = customFilter ./.;
    src = ./.;
    name = pname + "-source";
  };

  vendorSha256 = null;

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  subPackages = [ "cmd/detsys-debug" ];

  preBuild = ''
    # We need CGO to include sqlite.
    export CGO_ENABLED=1
  '';
}
