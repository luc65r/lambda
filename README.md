[![Build Status](https://travis-ci.org/luc65r/lambda.svg?branch=master)](https://travis-ci.org/luc65r/lambda)

# lambda

A De Bruijn indexed lambda calculus interpreter written in Haskell


## What is lambda calculus?

You can find informations about lambda calculus [here](https://en.wikipedia.org/wiki/Lambda_calculus)
and about De Bruijn indices [here](https://en.wikipedia.org/wiki/De_Bruijn_index).


## Installation

### If you are using Nix

Just copy `default.nix`, replace `src`, and call it with
`haskellPackages.callPackage /path/to/default.nix { }` somewhere, for example in an overlay.

### If you aren't

I don't know. If you don't know either, search online.


## Usage

The executable `lambda` is a interpretor.
I included an example ([examples/fac.lambda](examples/fac.lambda)) which compute 4!.
You can try it by doing `lambda < path/to/fac.lambda`, and it should output the Church representation of 24,
which is `λ λ 2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 (2 1)))))))))))))))))))))))`.

You can also launch a simple REPL with `lambda repl`.
`:q` quits the REPL.

It is recommended to wrap the REPL with `rlwrap` to provide history.
If you used the `default.nix`, the wrap is provided under the name `rlambda`.


## Why De Bruijn indices?

Because it is easier to implement.
