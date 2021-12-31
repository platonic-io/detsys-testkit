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

  src = gitignoreSource ./.;
  buildInputs = [ detsysLib ];

  vendorSha256 = "00mg22kx3n5z8ai4gdxljzdjb3cbshx8pj342ys4fl4ckav8z4rl";

  ldflags =
    # This is a dummy git hash to avoid breaking the nix cache, it will be
    # patched in the `postInstall` phase of the top-level `default.nix`.
    [ "-X main.version=0000000000000000000000000000000000000000-nix" ];

  preBuild = ''
    # We need to put the source of the library in `../lib`, because
    # that's where `go.mod` says to go look for it.
    cp -R ${detsysLib}/src ../lib

    # Need cgo for sqlite3.
    export CGO_ENABLED=1
  '';

  # Rename the resulting binary. (We can't use buildFlags with `-o`, because
  # that also gets passed to `go install` which does not recognise that flag.)
  postBuild = ''
    mv $GOPATH/bin/cli $GOPATH/bin/detsys
  '';
}
