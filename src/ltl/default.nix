{ sources ? import ./../../nix/sources.nix
, compiler ? "ghc8103"
}:
let
  inherit (import sources.gitignore { }) gitignoreSource;
  nixpkgs = import sources.nixpkgs {
    config = {
      allowUnfree = true;
    };
  } ;
  pkg = nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "ltl" (./.) { };
in
pkg.overrideAttrs (attrs: {
  pname = "detsys-ltl";
  src = gitignoreSource ./.;
  preBuild = ''
    export DETSYS_LTL_VERSION="${nixpkgs.lib.commitIdFromGitRepo ./../../.git + "-nix"}"
  '';
  # this should probably check that attrs.checkInputs doesn't exist
  checkInputs = [ nixpkgs.pkgs.haskell.packages.${compiler}.tasty-discover ];
})
