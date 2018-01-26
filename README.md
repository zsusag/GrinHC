# GrinHC
A compiler written in [Haskell](https://www.haskell.org) for the CSC-312 course
at Grinnell College during the Spring 2018 semester.
## Building and Running with Stack
For directions on obtaining `stack`, see
[here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

The first time you clone the repository, you must run:
```
$ stack setup
```
To build:
```
$ stack build
```
To run on input `foo.txt`:
```
$ stack exec GrinHC -- foo.txt
```

