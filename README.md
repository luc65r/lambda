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

The executable `lambda` is a simple REPL.
`:q` quits the REPL.

It is recommended to wrap it with `rlwrap` to provide history.
If you used the `default.nix`, the wrap is provided under the name `rlambda`.


## Why De Bruijn indices?

Because it is easier to implement.
