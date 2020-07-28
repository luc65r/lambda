{ mkDerivation
, lib
, hspec
, hlint-test

, wrapWithReadline ? true, makeWrapper, rlwrap
}:

mkDerivation {
  pname = "lambda";
  version = "0.3";
  src = ./.;

  isLibrary = true;
  isExecutable = true;

  testHaskellDepends = [ hspec hlint-test ];

  buildDepends = lib.optional wrapWithReadline [ makeWrapper ];
  postFixup = lib.optional wrapWithReadline ''
    makeWrapper ${rlwrap}/bin/rlwrap $out/bin/rlambda \
        --add-flags "$out/bin/lambda"
  '';

  homepage = "https://github.com/luc65r/lambda#readme";
  license = lib.licenses.mit;
}
