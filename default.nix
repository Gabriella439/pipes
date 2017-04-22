{ mkDerivation, base, exceptions, mmorph, mtl, QuickCheck, stdenv
, test-framework, test-framework-quickcheck2, transformers, void
}:
mkDerivation {
  pname = "pipes";
  version = "4.3.2";
  src = ./.;
  libraryHaskellDepends = [
    base exceptions mmorph mtl transformers void
  ];
  testHaskellDepends = [
    base mtl QuickCheck test-framework test-framework-quickcheck2
    transformers
  ];
  description = "Compositional pipelines";
  license = stdenv.lib.licenses.bsd3;
}
