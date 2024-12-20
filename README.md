# Kittens

A toolkit to generate concrete test cases out of .cat files.

# Use cases

## Exploding a cat model

From a cat model, we can create fully-expanded cases (aka, "kittens") for the
properties defined in the model. For example, to create all possible kittens
with length 2 out of the SC model, run the following command.

    kittens explode models/sc.cat 2

The result will have `empty` and `acyclic` statements that should hold in a
correct atomic, sequentially consistent implementation.

## Others

The other use cases, which are not merged in main yet are:
- converting a kitten to a C litmus test (runnable on Dartagnan and Herd7)
- converting a kitten to real C code (runnable natively, on Dartagnan or GenMC)
- converting a kitten to a Assembly litmus test (runnable on Dartagnan and Herd7)

# Code structure

The project structure is this:

- `lib/kittens/` - reusable code, parsers, etc
- `cmd/`         - commands (explode, ...)
- `vendor/`      - external source code (packrat)
- `models/`      - a few important .cat files

File types are:

- .cat - cat
- .scm - Scheme
- .sld - Scheme library definition
- .tst - Scheme test files
- .egg - CHICKEN Scheme package definition

There are also a few Makefiles that run tests in Chibi Scheme.

# Building and installing

## Dependencies

Kittens is written in Scheme R7RS and can be run with [Chibi Scheme][]
interpreter or compiled to binary with [CHICKEN Scheme][].

[Chibi Scheme]: https://github.com/ashinn/chibi-scheme
[CHICKEN Scheme]: https://call-cc.org

To a quick start, we recommend installing `chibi-scheme`. For example, assuming
`$HOME/.local/bin` is in your `PATH`, you can install chibi-scheme like this:

    git clone https://github.com/ashinn/chibi-scheme
    make -C chibi-scheme PREFIX=$HOME/.local install

To (optionally) install CHICKEN, use the package manager of your Linux
distribution.  For formatting the code in Kittens, you need to install
`schematic` packagen from CHICKEN:

    chicken-install schematic

## Compiling

With CHICKEN, first compile, then run:

    chicken-install -n
    ./explode <cat file> <#edges>

With Chibi, just call the commands:

    cmd/explode.scm <cat file> <#edges>

## Testing, building, formating

Use the targets in the `Makefile`:

    make test   # uses chibi-scheme by default
    make format # requires schematic package from chicken
    make build  # compiles the commands to binary with CHICKEN

## Installing

Run `make install PREFIX=...` to install kittens.
