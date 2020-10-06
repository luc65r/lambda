# Revision history for lambda

## 0.1 -- 2020-07-27

* First version. Released on an unsuspecting world.


## 0.2 -- 2020-07-27

* Error messages for the parser
* Better tests


## 0.3 -- 2020-07-29

* Tests for the parser
* New function: apps
* CI
* Linter
* Better documentation
* Better code


## 0.4 -- 2020-07-29

* It now uses Megaparsec!
* New function: parseLambda
* Better REPL
* Better parsing error messages thanks to Megaparsec


## 0.5 -- 2020-07-30

* REPL moved to `lambda repl` or `rlambda`
* `lambda` now parses and reduct stdin, so you can do `lambda < expr.lambda`
* The REPL now will ask for more input if there are parentheses open


## 0.6 -- 2020-10-06

* Fix tests without Nix
* Use haskeline for the REPL, so no need for readline anymore
