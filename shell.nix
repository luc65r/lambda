with import <nixpkgs> {};

let
  lambda = haskellPackages.callPackage ./default.nix {};

in (haskell.lib.addBuildTools lambda (with haskellPackages; [
  cabal-install
  ghcid
  hlint
])).env
