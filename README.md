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

# Example usage:

## Generate kittens (ie, edge sequences):

    cmd/explode.scm models/sc.cat 4

Result:

```
$ cmd/explode.scm models/sc.cat 4  | head -n 10
# model file: models/sc.cat
# cycle len: 4
WARNING: cannot include 'cos.cat'
po po po po
po po po rf
po po po fr
po po po co
po po rf po
po po rf rf
po po rf fr
...
```

## Grill a Kitten (z3 dependency):

    cmd/grill.scm po fr po fr | z3 -in

Result:
```
$ cmd/grill.scm po fr po fr | z3 -in | head -n 20
sat
(model
  (define-fun evfr1 () Event
    (mk-event (- 4) 5 4 203 300 103 0 write))
  (define-fun ev0 () Event
    (mk-event 2285 0 0 203 302 101 10 write))
  (define-fun ev2 () Event
    (mk-event 2281 2 1 200 303 103 12 write))
  (define-fun evfr3 () Event
    (mk-event (- 3) 4 3 202 301 101 0 write))
  (define-fun ev1 () Event
    (mk-event 2282 1 0 205 305 103 0 read))
  (define-fun ev3 () Event
    (mk-event 2280 3 1 202 301 101 0 read))
  (define-fun edfr3 () Edge
    (mk-edge (mk-event (- 3) 4 3 202 301 101 0 write)
         (mk-event 2280 3 1 202 301 101 0 read)
         rf))
  (define-fun edfr1 () Edge
    (mk-edge (mk-event (- 4) 5 4 203 300 103 0 write)
...
```

## Roast the model

If Kitten ok, roast it into a litmus test:

    cmd/grill.scm po fr po fr | z3 -in > model.txt
    cmd/roast.scm model.txt

Result:
```
$ cmd/roast.scm model.txt
Test Name
Some Very Useful Information
{}

P0 (volatile int* cx, volatile int* cz) {
    *cx = 10;
    int r0 = *cz;
}

P1 (volatile int* cz, volatile int* cx) {
    *cz = 12;
    int r0 = *cx;
}

exists (0:r0=0 /\ 1:r0=0)
```

## Defuse it

```
$ cmd/roast.scm model.txt > test.litmus
$ cmd/nope test.litmus
AArch64 test.litmus
{
  0:x0 = cx;
  0:x1 = cz;
  0:x2 = r0_P0;
  1:x0 = cz;
  1:x1 = cx;
  1:x2 = r0_P1;
}

P0                     | P1                     ;
LC88:                  | LCa0:                  ;
      MOV     w3, #10  |       MOV     w3, #12  ;
LC8c:                  | LCa4:                  ;
      STR     w3, [x0] |       STR     w3, [x0] ;
LC90:                  | LCa8:                  ;
      LDR     w0, [x1] |       LDR     w0, [x1] ;
LC94:                  | LCac:                  ;
      STR     w0, [x2] |       STR     w0, [x2] ;
LC98:                  | LCb0:                  ;
LC9c:                  | LCb4:                  ;

exists (r0_P0 = 0 /\ r0_P1 = 0)
```


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

