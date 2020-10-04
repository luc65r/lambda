{ mkDerivation
, lib
, hspec
, hlint-test
, megaparsec
, haskeline
}:

mkDerivation {
  pname = "lambda";
  version = "0.5";
  src = ./.;

  isLibrary = true;
  isExecutable = true;

  executableHaskellDepends = [ megaparsec haskeline ];
  libraryHaskellDepends = [ megaparsec haskeline ];

  testHaskellDepends = [ hspec hlint-test ];

  homepage = "https://github.com/luc65r/lambda#readme";
  license = lib.licenses.mit;
}
