{ mkDerivation, base, exceptions, mmorph, mtl, QuickCheck, stdenv
, test-framework, test-framework-quickcheck2, transformers
}:
mkDerivation {
  pname = "pipes";
  version = "4.3.1";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions mmorph mtl transformers
  ];
  testHaskellDepends = [
    base mtl QuickCheck test-framework test-framework-quickcheck2
    transformers
  ];
  description = "Compositional pipelines";
  license = stdenv.lib.licenses.bsd3;
}
