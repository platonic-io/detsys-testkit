{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
with pkgs;

let
  fake-lsb-release = pkgs.writeScriptBin "lsb_release" ''
    #!${pkgs.runtimeShell}

    case "$1" in
      -i) echo "nixos";;
      -r) echo "nixos";;
    esac
  '';
in

pkgs.mkShell {
  name = "dev-shell";

  buildInputs = [
    bazel_4
    buildifier # Bazel BUILD file formatter
    go
    clojure
    fake-lsb-release
    haskell.compiler.ghc8104
    haskellPackages.cabal-install
    haskellPackages.tasty-discover
    haskellPackages.cabal-fmt # For formatting .cabal files, example
                              # invocation: `cabal-fmt -i ldfi.cabal`.
    haskellPackages.stylish-haskell # For import statement formatting, can be
                                    # invoked from spacemacs via `, F`.
    haskellPackages.ormolu # Haskell code formatting, to format all files in the
                           # current directory: `ormolu --mode inplace $(find . -name '*.hs')`.
    nixpkgs-fmt # Nix code formatting, example invocation: `nixpkgs-fmt default.nix`.
    git
    nix
    niv
    direnv
    lorri
    nix-index

    z3
    zlib.dev
    zlib.out
    freetype.dev
    pkgconfig # Use `pkg-config --libs zlib` from inside the shell to figure out
              # what to pass to GraalVM's native-image.
  ] ++ lib.optionals stdenv.hostPlatform.isDarwin
    [ darwin.apple_sdk.frameworks.Foundation
      darwin.apple_sdk.frameworks.CoreFoundation
    ];
}
