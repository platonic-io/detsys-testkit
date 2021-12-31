{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {
  overlays = [
    (import "${builtins.fetchTarball {
      url =  https://github.com/tweag/gomod2nix/archive/67f22dd738d092c6ba88e420350ada0ed4992ae8.tar.gz;
      sha256 = "0a9rhw6djgcrky5nzly3srsl8095q42f2nyzxyisp9fh9fg70nih";
    }}/overlay.nix")
  ];
}
}:
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
    pkgs.gomod2nix
    clojure
    fake-lsb-release
    haskell.compiler.ghc8107
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
