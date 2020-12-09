{ sources ? import ./../../nix/sources.nix
, pkgs ? import sources.nixpkgs {}
}:
with pkgs;

let
  inherit (import sources.gitignore {}) gitignoreSource;
in

stdenv.mkDerivation rec {
  pname = "detsys-generator";
  version = "0.0.0";
  name = "${pname}-${version}";
  src = gitignoreSource ./.;

  installPhase = ''
    mkdir -p $out/bin
    install -Dm755 ${pname}.sh $out/bin/${pname}
  '';
}
