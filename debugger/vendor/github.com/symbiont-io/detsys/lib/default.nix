{ pkgs ? import <nixpkgs> {} }:
with pkgs;

assert lib.versionAtLeast go.version "1.15";

buildGoModule rec {
  pname = "lib";
  version = "0.0.0";
  goPackagePath = "github.com/symbiont-io/detsys/lib";
  src = ./.;
  vendorSha256 = null;

  goDeps = ./deps.nix;
  allowGoReference = false;

  preBuild = ''
    export CGO_ENABLED=0
    # export GO111MODULE=on
    export GOPATH=$TMPDIR
    buildFlagsArray+=(-pkgdir "$TMPDIR")
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out
    dir="$GOPATH/bin"
    [ -e "$dir" ] && cp -r $dir $out
    echo $out
    cp -r $GOPATH/lib $out

    runHook postInstall
  '';

# https://github.com/NixOS/nixpkgs/pull/6119

# installPhase = args.installPhase or ''
#    runHook preInstall
#
#    local dir
#    for d in pkg src; do
#        mkdir -p $out/share/go
#        dir="$NIX_BUILD_TOP/go/$d"
#        [ -e "$dir" ] && cp -r $dir $out/share/go
#    done
#    mkdir $out
#
#    if [ -z "$dontInstallSrc" ]; then
#        local dir
#        for d in pkg src; do
#            mkdir -p $out/share/go
#            dir="$NIX_BUILD_TOP/go/$d"
#            [ -e "$dir" ] && cp -r $dir $out/share/go
#        done
#    fi
#
#    dir="$NIX_BUILD_TOP/go/bin"
#    [ -e "$dir" ] && cp -r $dir $out

}
