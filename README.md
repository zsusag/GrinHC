# GrinHC --- Zachary J. Susag
A compiler written in [Haskell](https://www.haskell.org) for the CSC-312 course at Grinnell College during the Spring 2018 semester.

## Supported Grammar
Currently, GrinHC supports only the following grammar:
```
e ::= n | (e) | e1 + e2 | e1 - e2 | e1 * e2 | e1 / e2
    | e1 % e2 | true | false | e1 <= e2 | e1 == e2
    | e1 >= e2 | e1 < e2 | e1 > e2 | if e1 then e2 else e3
    | f | NaN | x | let x :: t = e1 in e2 | lambda (x::t1)::t2 => e
    | fix f (x::t1)::t2 => e | e1 e2 | () | (e1, e2) | fst e | snd e
    | [] :: t | e1:e2 | head e | tail e | empty e

t ::= Int | Bool | Float | Unit | t1 -> t2 | (t1, t2) | [t]
```
where `n` is an integer literal, `x` is a variable identifier of the form
```
[a-z][a-z A-Z 0-9 ' _]+
```
`f` is a floating point literal of the form
```
[0-9]+.[0-9]+
```
and `NaN` as defined in [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754).

## Building and Running with Stack
### Installing Stack
If `stack` is not already installed on your system you can install it using:
```
$ curl -sSL https://get.haskellstack.org/ | sh
```
If running GNU/Linux you could alternatively install `stack` using the package manager associated with your distribution (e.g. `pacman` for ArchLinux).

For additional directions on obtaining `stack`, see [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

#### Installing additional dependencies
GrinHC makes use of a Haskell linter called `hlint`. To install this program using stack, run,
```
$ stack install hlint
```

#### Enable githooks
To enable pre-commit hooks, run the following command from the root of the project repository,
```
$ cd .git/hooks && ln -s ../../githooks/pre-commit
```
This should enable a git pre-commit hook which will run `hlint` on all the source files as well as run the test suite automatically.

### Building GrinHC
The first time you clone the repository, you must run:
```
$ stack setup
```
To build the project thereafter:
```
$ stack build
```
You may need to run
```
$ stack init
```
if prompted to do so by stack. This will initialize the project according to your machine.

### Running GrinHC
To run GrinHC command line arguments:
```
$ stack exec GrinHC -- foo bar baz
```
**Note:** To pass command line arguments to GrinHC instead of `stack` the double `--` are necessary.

### Running the Testing Suite
To run the testing suite for GrinHC, run:
```
$ stack test
```

## Changelog
### [assignment-04] --- 2018-02-28
#### New Features
* Added a typechecking phase which executes before evaluation occurs and will print an error message if there is a type error
* Add `()` (unit) language feature to grammar
* Add pairs to grammar
* Add lists to grammar
#### Changes to Existing Features
* Expanded test suite to include tests for `()`, pairs, lists, and typechecking
* Fixed bug which was required disambiguating parentheses with function application
#### Known Bugs
* None
### [assignment-04] --- 2018-02-21
#### New Features
* Can now define functions, both recursive and not, let-bindings, and function applications
* Added `--step` command line argument to print out all of the evaluation steps
* Added `%`, `<`, `>`, `==`, and`>=` operators
#### Changes to Existing Features
* Transitioned from Big-Step evaluation to Small-Step evaluation
* Expanded test suite to include tests for new operators, functions, and let-bindings
    * `stack test` will also now test whether the steps taken during evaluation are correct
#### Known Bugs
* Given an expression of the form `5 * f 3` where `f` is a function the parser currently builds the following AST, `(5 * f) 3` instead of the correct AST, `5 * (f 3)`. As a result, failure to include disambiguating parentheses will result in an evaluation error.
### [assignment-03] --- 2018-02-12
#### New Features
* Error messages new report position information (line,column)
* `generate_baselines.sh` script added to, well, generate baselines automatically
#### Changes to Existing Features
* [Alex](https://www.haskell.org/alex/) and [Happy](https://www.haskell.org/happy/) are used instead of a hand-rolled lexer and parser
* Grammar changed from prefix to infix
    * `<=` is non-associative and thus binds the loosest.
    * `+` and `-` are left-associative and binds tighter than `<=`
    * `*` and `/` are left-associative and binds the tightest of all operations currently in the grammar
* Test suite upgraded
    * Tests updated to test infix grammar
    * `stack test` will now test lexing and parsing components of GrinHC as well as evaluation
#### Known Bugs
* None
### [assignment-02] --- 2018-02-06
#### New Features
* Add ability lex and parse basic S-Expressions in a LISP style syntax
* Ability to read in file with code return evaluated code
* Intepret S-Expressions with arithmetic operations, booleans, and floats
* Augment testing suite with tests that cover entire grammar
#### Changes to Existing Features
* Remove `--length` command line option
* Remove ability to print command line arguments given back to the user
* Remove old testing suite which tested old features
#### Known Bugs
* None
### [assignment-01] --- 2018-01-29
#### New Features
* Created the project
* Parse command line arguments and spit them back to `stdout`
* Add `--length` argument to print out the length of each argument instead
* Add infrastructure for testing suite
* Add a git-precommit hook to automatically run `hlint` on all `*.hs` files
  within the project as well as run the test suite.
#### Changes to Existing Features
* N/A
#### Known Bugs
* None
