{ mkDerivation
, lib
, hspec
, hlint-test
, megaparsec

, wrapWithReadline ? true, makeWrapper, rlwrap
}:

mkDerivation {
  pname = "lambda";
  version = "0.5";
  src = ./.;

  isLibrary = true;
  isExecutable = true;

  executableHaskellDepends = [ megaparsec ];
  libraryHaskellDepends = [ megaparsec ];

  testHaskellDepends = [ hspec hlint-test ];

  buildDepends = lib.optional wrapWithReadline [ makeWrapper ];
  postFixup = lib.optional wrapWithReadline ''
    makeWrapper ${rlwrap}/bin/rlwrap $out/bin/rlambda \
        --add-flags "$out/bin/lambda repl"
  '';

  homepage = "https://github.com/luc65r/lambda#readme";
  license = lib.licenses.mit;
}
