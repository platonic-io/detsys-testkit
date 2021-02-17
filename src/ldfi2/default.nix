{ sources ? import ./../../nix/sources.nix
, compiler ? "ghc8103"
}:
let
  inherit (import sources.gitignore {}) gitignoreSource;
  nixpkgs = import sources.nixpkgs {
    config = {
      allowUnfree = true;
      packageOverrides = super: let pkgs = super.pkgs; in
        {
          haskell = super.haskell // {
            packages = super.haskell.packages // {
              ${compiler} = super.haskell.packages.${compiler}.override {
                overrides = self: super: {
                  z3 = self.callPackage ./z3.nix
                    { z3 = pkgs.z3; };
                };
              };
            };
          };
        };
    };
  };
  pkg = nixpkgs.pkgs.haskell.packages.${compiler}.callCabal2nix "ldfi" (./.) {};
in pkg.overrideAttrs(attrs: {
  preBuild = ''
    export DETSYS_LDFI_VERSION="${nixpkgs.lib.commitIdFromGitRepo ./../../.git}"
  '';
  src = gitignoreSource ./.;
  # this should probably check that attrs.checkInputs doesn't exist
  checkInputs = [nixpkgs.pkgs.haskell.packages.${compiler}.tasty-discover];
  pname = "detsys-ldfi2";
})
