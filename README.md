# GrinHC --- Zachary J. Susag
A compiler written in [Haskell](https://www.haskell.org) for the CSC-312 course at Grinnell College during the Spring 2018 semester. Currently, it doesn't compile anything but will eventually compile LISP style code.

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
