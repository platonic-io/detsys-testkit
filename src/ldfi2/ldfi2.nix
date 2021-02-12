{ mkDerivation, base, containers, filepath, HUnit, lib, mtl
, optparse-generic, QuickCheck, sqlite-simple, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, z3
}:
mkDerivation {
  pname = "ldfi";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers filepath mtl QuickCheck sqlite-simple
    template-haskell z3
  ];
  executableHaskellDepends = [ base optparse-generic ];
  testHaskellDepends = [
    base containers HUnit QuickCheck tasty tasty-hunit tasty-quickcheck
    z3
  ];
  description = "Lineage-driven fault injection";
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
}
