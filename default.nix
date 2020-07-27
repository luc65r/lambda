{ mkDerivation
, lib
, hspec

, wrapWithReadline ? true, makeWrapper, rlwrap
}:

mkDerivation {
  pname = "lambda";
  version = "0.2";
  src = ./.;

  isLibrary = true;
  isExecutable = true;

  testHaskellDepends = [ hspec ];

  buildDepends = lib.optional wrapWithReadline [ makeWrapper ];
  postFixup = lib.optional wrapWithReadline ''
    makeWrapper ${rlwrap}/bin/rlwrap $out/bin/rlambda \
        --add-flags "$out/bin/lambda"
  '';

  homepage = "https://github.com/luc65r/lambda#readme";
  license = lib.licenses.mit;
}
