{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, nix-build-checker ? true
, nix-build-scheduler ? false }:
with pkgs;

let
  # TODO(stevan): remove these workarounds once checker (on both linux and
  # macos) and scheduler builds with bazel (on macos).
  checker = callPackage ./src/checker/default.nix {};
  scheduler = callPackage ./src/scheduler/default.nix {};
in

stdenv.mkDerivation {
  pname = "detsys";
  version = "latest";

  src = ./bazel-bin/src;

  phases = [ "installPhase" "installCheckPhase" ];

  propagatedBuildInputs = [ z3 ]
                          ++ lib.optional nix-build-checker [ checker ]
                          ++ lib.optional nix-build-scheduler [ scheduler ];

  installPhase = ''
    install -D $src/cli/cli_/cli \
               $out/bin/detsys
    install -D $src/db/db_/db \
               $out/bin/detsys-db
    install -D $src/debugger/cmd/detsys-debug/detsys-debug_/detsys-debug \
               $out/bin/detsys-debug
    ${if nix-build-checker then ''
    install -D ${checker.out}/bin/detsys-checker $out/bin
    '' else ''
    # TODO(stevan): uncomment once checker builds with rules_graal
    # install -D $src/checker/checker-bin \
    #            $out/bin/detsys-checker
    ''
    }
    ${if nix-build-scheduler then ''
    install -D ${scheduler.out}/bin/detsys-scheduler $out/bin
    '' else ''
    install -D $src/scheduler/scheduler-bin \
               $out/bin/detsys-scheduler
    ''
    }
    install -D $src/ldfi2/ldfi2 \
               $out/bin/detsys-ldfi
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    # TODO(stevan): figure out why this doesn't work.
    #if [ "$($out/bin/detsys --version)" != "detsys version unknown" ]; then
    #   echo "version mismatch"
    #fi
  '';
}
