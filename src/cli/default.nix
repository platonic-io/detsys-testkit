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
  pname = "cli";
  version = "latest";
  goPackagePath = "github.com/symbiont-io/detsys-testkit/src/${pname}";

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];

  # This hash should be the output of:
  #   go mod vendor && nix-hash --base32 --type sha256 vendor
  vendorSha256 = "0d7nwxx0i834w0kvixl4map8wb6kw4cz30ckvk394cxm4sblzipd";

  buildFlagsArray =
    [ "-ldflags=-X main.version=${lib.commitIdFromGitRepo ./../../.git}" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # Static linking.
    export CGO_ENABLED=0
  '';

  # Rename the resulting binary. (We can't use buildFlags with `-o`, because
  # that also gets passed to `go install` which does not recognise that flag.)
  postBuild = ''
    mv $GOPATH/bin/cli $GOPATH/bin/detsys
  '';
}
