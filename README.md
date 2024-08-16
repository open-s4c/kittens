# Kittens

The project structure is this:

- `lib/kittens/` - reusable code, parsers, etc
- `cmd/`         - commands (explode, grill, roast, ...)
- `vendor/`      - external source code (packrat)
- `models/`      - a few important .cat files

File types are:

- .cat - cat
- .scm - scheme
- .sld - scheme library definition
- .tst - scheme test files
- .egg - CHICKEN scheme package definition

There are also a few Makefiles that run the tests in chibi.

# Dependencies

Kittens is written in Scheme R7RS and can be run with [Chibi-Scheme][]
interpreter or compiled to binary with [CHICKEN Scheme][].

[Chibi-Scheme]: https://github.com/ashinn/chibi-scheme
[CHICKEN Scheme]: https://call-cc.org

To a quick start, we recommend installing `chibi-scheme`. For example, assuming
`$HOME/.local/bin` is in your `PATH`, you can install chibi-scheme like this:

    git clone https://github.com/ashinn/chibi-scheme
    make -C chibi-scheme PREFIX=$HOME/.local install

To (optionally) install CHICKEN, use the package manager of your Linux
distribution.  For formatting the code in Kittens, you need to install
`schematic` packagen from CHICKEN:

    chicken-install schematic

# How to run

With CHICKEN, first compile, then run:

    chicken-install -n
    ./explode <cat file> <#edges>

With Chibi, just call the commands:

    cmd/explode.scm <cat file> <#edges>

# Testing, building, formating

Use the targets in the `Makefile`:

    make test   # uses chibi-scheme by default
    make format # requires schematic package from chicken
    make build  # compiles the commands to binary with CHICKEN


# List of commands

- explode
    - input: .cat
    - output: .kittens file

- grill: filter bad kittens and come up with example events
    - input: kitten line
    - output: ???

- roast: create a litmus test from a good kitten
    - input: "???"
    - output: .litmus C

- defuse: converter
    - input: .litmus/C
    - output: .c

- pasm:
    - input: .litmus/C + object dump (from compiled .c)
    - output: .litmus/ASM

- nope:
    - input: .litmus/C
    - output: .litmus/ASM
    - details:
        - runs defuse
        - compiles result with gcc
        - disassembles with objdump
        - lifts result with pasm
    - requires: gcc-aarch64-linux-gnu package installed

