# Kittens

The project structure is this:

- lib/kittens - reusable code, parsers, etc
- cmd/        - commands (explode, grill, roast, ...)
- vendor/     - external source code (packrat)
- models/     - a few important .cat files

File types are:

.cat - cat
.scm - scheme
.sld - scheme library definition
.tst - scheme test files
.egg - CHICKEN scheme package definition

There are also a few Makefiles that run the tests in chibi.

# How to run

With CHICKEN, first compile, then run:

    chicken-install -n
    ./explode <cat file> <#edges>

With Chibi, just call the commands:

    cmd/explode.scm <cat file> <#edges>

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

- nope:
    - input: .litmus/C + object dump (from compiled .c)
    - output: .litmus/ASM

