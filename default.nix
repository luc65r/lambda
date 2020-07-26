{ mkDerivation
, lib

, wrapWithReadline ? true, makeWrapper, rlwrap
}:

mkDerivation {
  pname = "lambda";
  version = "0.1";
  src = ./.;

  isLibrary = true;
  isExecutable = true;

  buildDepends = lib.optional wrapWithReadline [ makeWrapper ];
  postFixup = lib.optional wrapWithReadline ''
    makeWrapper ${rlwrap}/bin/rlwrap $out/bin/rlambda \
        --add-flags "$out/bin/lambda"
  '';

  homepage = "https://github.com/luc65r/lambda#readme";
  license = lib.licenses.mit;
}
