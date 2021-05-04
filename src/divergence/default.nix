{ sources ? import ./../../nix/sources.nix
, compiler ? "ghc8104"
}:
let
  inherit (import sources.gitignore { }) gitignoreSource;
  nixpkgs = import sources.nixpkgs {
    config = {
      allowUnfree = true;
    };
  } ;
  pkg = nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "divergence" (./.) { };
in
pkg.overrideAttrs (attrs: {
  pname = "detsys-divergence";
  src = gitignoreSource ./.;
  configureFlags =
    # This is a dummy git hash to avoid breaking the nix cache, it will be
    # patched in the `postInstall` phase of the top-level `default.nix`.
    [ "--ghc-option=-D__GIT_HASH__=\"0000000000000000000000000000000000000000-nix\"" ];
  # this should probably check that attrs.checkInputs doesn't exist
  # checkInputs = [ nixpkgs.pkgs.haskell.packages.${compiler}.tasty-discover ];
})
