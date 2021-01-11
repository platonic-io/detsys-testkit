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
  pname = "db";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  src = lib.cleanSourceWith {
    filter = customFilter ./.;
    src = ./.;
    name = pname + "-source";
  };

  propagatedBuildInputs = [ sqlite-interactive ];

  vendorSha256 = null;

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  preBuild = ''
    # We need CGO to include sqlite.
    export CGO_ENABLED=1
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
