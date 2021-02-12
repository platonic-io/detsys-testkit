{ sources ? import ./../../nix/sources.nix
, nixpkgs ? import sources.nixpkgs {}
, compiler ? "ghc8103" }:
let
  inherit (import sources.gitignore {}) gitignoreSource;
  pkg = nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./ldfi2.nix { };
in pkg.overrideAttrs(attrs: {
  # this should probably check that attrs.preBuild doesn't exist
  preBuild = ''
    export DETSYS_LDFI_VERSION="${nixpkgs.lib.commitIdFromGitRepo ./../../.git}"
  '';
  src = gitignoreSource ./.;
  # this should probably check that attrs.checkInputs doesn't exist
  checkInputs = [nixpkgs.pkgs.haskellPackages.tasty-discover];
  pname = "detsys-ldfi2";
})
