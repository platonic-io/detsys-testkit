{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, nix-build-all       ? true
, nix-build-checker   ? false
, nix-build-cli       ? false
, nix-build-db        ? false
, nix-build-debugger  ? false
, nix-build-generator ? true
, nix-build-ldfi      ? false
, nix-build-scheduler ? false
}:
with pkgs;

let
  # TODO(stevan): remove these workarounds once checker and scheduler builds
  # with bazel on macos.
  checker = callPackage ./src/checker/default.nix {};
  cli = callPackage ./src/cli/default.nix {};
  db = callPackage ./src/db/default.nix {};
  debugger = callPackage ./src/debugger/default.nix {};
  generator = callPackage ./src/generator/default.nix {};
  ldfi = callPackage ./src/ldfi2/default.nix {};
  scheduler = callPackage ./src/scheduler/default.nix {};
in

stdenv.mkDerivation {
  pname = "detsys";
  version = "latest";

  src = ./bazel-bin/src;

  phases = [ "installPhase" "installCheckPhase" ];

  propagatedBuildInputs = [ z3 ]
                          ++ lib.optional (nix-build-all || nix-build-checker)   [ checker ]
                          ++ lib.optional (nix-build-all || nix-build-cli)       [ cli ]
                          ++ lib.optional (nix-build-all || nix-build-db)        [ db ]
                          ++ lib.optional (nix-build-all || nix-build-debugger)  [ debugger ]
                          ++ lib.optional (nix-build-all || nix-build-generator) [ generator ]
                          ++ lib.optional (nix-build-all || nix-build-ldfi)      [ ldfi ]
                          ++ lib.optional (nix-build-all || nix-build-scheduler) [ scheduler ];

  installPhase = ''
    mkdir -p $out/bin

    ${if nix-build-checker || nix-build-all then ''
    install -D ${checker.out}/bin/detsys-checker $out/bin
    '' else ''
    install -D $src/checker/checker-bin $out/bin/detsys-checker
    ''
    }
    ${if nix-build-cli || nix-build-all then ''
    install -D ${cli.out}/bin/detsys $out/bin
    '' else ''
    install -D $src/cli/cli_/cli $out/bin/detsys
    ''
    }
    ${if nix-build-db || nix-build-all then ''
    install -D ${db.out}/bin/detsys-db $out/bin
    cp -R ${db.out}/migrations $out
    '' else ''
    install -D $src/db/db_/db $out/bin/detsys-db
    # TODO(stevan): migrations need to be stored somewhere...
    ''
    }
    ${if nix-build-debugger || nix-build-all then ''
    install -D ${debugger.out}/bin/detsys-debug $out/bin
    '' else ''
    install -D $src/debugger/cmd/detsys-debug/detsys-debug_/detsys-debug \
               $out/bin/detsys-debug
    ''
    }
    ${if nix-build-generator || nix-build-all then ''
    install -D ${generator.out}/bin/detsys-generator $out/bin
    install -D ${generator.out}/bin/detsys-generator-version $out/bin
    '' else ''
    # TODO(stevan): build with bazel
    ''
    }
    ${if nix-build-ldfi || nix-build-all then ''
    install -D ${ldfi.out}/bin/detsys-ldfi $out/bin
    '' else ''
    install -D $src/ldfi2/ldfi2 $out/bin/detsys-ldfi
    ''
    }
    ${if nix-build-scheduler || nix-build-all then ''
    install -D ${scheduler.out}/bin/detsys-scheduler $out/bin
    '' else ''
    install -D $src/scheduler/scheduler-bin $out/bin/detsys-scheduler
    ''
    }
  '';

  doInstallCheck = true;

  installCheckPhase = ''
    # TODO(stevan): figure out why this doesn't work.
    #if [ "$($out/bin/detsys --version)" != "detsys version unknown" ]; then
    #   echo "version mismatch"
    #fi
  '';
}
