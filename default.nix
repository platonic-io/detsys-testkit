{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {}
, nix-build-all       ? pkgs.stdenv.isDarwin
, nix-build-checker   ? false
, nix-build-cli       ? false
, nix-build-db        ? false
, nix-build-debugger  ? false
, nix-build-generator ? true
, nix-build-ldfi      ? false
, nix-build-ltl       ? false
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
  ltl = callPackage ./src/ltl/default.nix {};
  scheduler = callPackage ./src/scheduler/default.nix {};
in

stdenv.mkDerivation {
  pname = "detsys";
  version = "latest";

  # NOTE: When a `go_binary` is compiled with gotags it for some reason ends up
  # in `bazel-out` rather than `bazel-bin`, which is why we need to add both
  # below.
  #
  # TODO(stevan): k8-fastbuild-ST-578d10beb4b5 is probably unique to my machine,
  # need to figure out how to get this path in a generic way in bazel.
  src = if nix-build-all
        then []
        else [ ./bazel-bin/src ./bazel-out/k8-fastbuild-ST-578d10beb4b5/bin ];

  phases = [ "installPhase" "installCheckPhase" ];

  nativeBuildInputs = if stdenv.isLinux then [ patchelf ] else [];

  propagatedBuildInputs = [ z3 ]
                          ++ lib.optional (nix-build-all || nix-build-checker)   [ checker ]
                          ++ lib.optional (nix-build-all || nix-build-cli)       [ cli ]
                          ++ lib.optional (nix-build-all || nix-build-db)        [ db ]
                          ++ lib.optional (nix-build-all || nix-build-debugger)  [ debugger ]
                          ++ lib.optional (nix-build-all || nix-build-generator) [ generator ]
                          ++ lib.optional (nix-build-all || nix-build-ldfi)      [ ldfi ]
                          ++ lib.optional (nix-build-all || nix-build-ltl)       [ ltl ]
                          ++ lib.optional (nix-build-all || nix-build-scheduler) [ scheduler ];

  installPhase = ''
    mkdir -p $out/bin

    # TODO(stevan): Is there a nicer way to access the two source directories?
    export bazelBin="$(echo $src | cut -d ' ' -f 1)"
    export bazelOut="$(echo $src | cut -d ' ' -f 2)"

    ${if nix-build-checker || nix-build-all then ''
    install -D ${checker.out}/bin/detsys-checker $out/bin
    '' else ''
    install -D $bazelBin/checker/checker-bin $out/bin/detsys-checker
    ''
    }
    ${if nix-build-cli || nix-build-all then ''
    install -D ${cli.out}/bin/detsys $out/bin
    '' else ''
    install -D $bazelBin/cli/cli_/cli $out/bin/detsys
    ''
    }
    ${if nix-build-db || nix-build-all then ''
    install -D ${db.out}/bin/detsys-db $out/bin
    cp -R ${db.out}/migrations $out
    '' else ''
    install -D $bazelBin/db/db_/db $out/bin/detsys-db
    # TODO(stevan): migrations need to be stored somewhere...
    ''
    }
    ${if nix-build-debugger || nix-build-all then ''
    install -D ${debugger.out}/bin/detsys-debug $out/bin
    '' else ''
    install -D $bazelOut/src/debugger/cmd/detsys-debug/detsys-debug_/detsys-debug \
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
    install -D $bazelBin/ldfi2/ldfi2 $out/bin/detsys-ldfi
    ${lib.optionalString stdenv.isLinux ''
         patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
                  "$out/bin/detsys-ldfi"
         rpath=${lib.makeLibraryPath [ z3 stdenv.cc.cc gmp libffi ]}
         patchelf --set-rpath "$out/lib:$rpath" \
                  "$out/bin/detsys-ldfi"
         ''
     }
     # TODO(stevan): Darwin probably needs fixing as well?
    ''
    }
    ${if nix-build-ltl || nix-build-all then ''
    install -D ${ltl.out}/bin/detsys-ltl $out/bin
    '' else ''
    install -D $bazelBin/ltl/ltl $out/bin/detsys-ltl
    ''
    }
    ${if nix-build-scheduler || nix-build-all then ''
    install -D ${scheduler.out}/bin/detsys-scheduler $out/bin
    '' else ''
    install -D $bazelBin/scheduler/scheduler-bin $out/bin/detsys-scheduler
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
