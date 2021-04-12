{ sources ? import ./../../nix/sources.nix
, compiler ? "ghc8103"
}:
let
  inherit (import sources.gitignore { }) gitignoreSource;
  nixpkgs = import sources.nixpkgs {
    config = {
      allowUnfree = true;
      packageOverrides = super:
        let pkgs = super.pkgs; in
        {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${compiler} = super.haskell.packages.${compiler}.override {
                overrides = self: super: {
                  z3 = self.callCabal2nix "z3"
                    (builtins.fetchGit {
                      url = "git@github.com:stevana/haskell-z3";
                      rev = "b984d0451969428f6fbc00d65f4d958c8998d05a";
                    })
                    { z3 = pkgs.z3; };
                };
              };
            };
          };
        };
    };
  };
  pkg = nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "ldfi" (./.) { };
in
pkg.overrideAttrs (attrs: {
  pname = "detsys-ldfi";
  src = gitignoreSource ./.;
  configureFlags =
    # This is a dummy git hash to avoid breaking the nix cache, it will be
    # patched in the `postInstall` phase of the top-level `default.nix`.
    [ "--ghc-option=-D__GIT_HASH__=\"0000000000000000000000000000000000000000-nix\"" ];
  # this should probably check that attrs.checkInputs doesn't exist
  checkInputs = [ nixpkgs.pkgs.haskell.packages.${compiler}.tasty-discover ];
})
