{ mkDerivation
, lib
, hspec
, hlint-test
, megaparsec
, haskeline_0_8_1_0
}:

mkDerivation {
  pname = "lambda";
  version = "0.6";
  src = ./.;

  isLibrary = true;
  isExecutable = true;

  executableHaskellDepends = [ megaparsec haskeline_0_8_1_0 ];
  libraryHaskellDepends = [ megaparsec ];

  testHaskellDepends = [ hspec hlint-test ];

  homepage = "https://github.com/luc65r/lambda#readme";
  license = lib.licenses.mit;
}
