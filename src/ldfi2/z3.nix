{ mkDerivation, base, containers, fetchgit, hspec, lib, QuickCheck
, transformers, z3
}:
mkDerivation {
  pname = "haskell-z3";
  version = "408.2";
  src = fetchgit {
    url = "https://github.com/stevana/haskell-z3";
    sha256 = "0s6ckf87r1dcb2clcak4frz1z8yaqlcbpbgn54qz37sw7dsrb61a";
    rev = "b984d0451969428f6fbc00d65f4d958c8998d05a";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers transformers ];
  librarySystemDepends = [ z3 ];
  testHaskellDepends = [ base hspec QuickCheck ];
  homepage = "https://github.com/IagoAbal/haskell-z3";
  description = "Bindings for the Z3 Theorem Prover";
  license = lib.licenses.bsd3;
}
