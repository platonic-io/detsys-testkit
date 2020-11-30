{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.hello

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
