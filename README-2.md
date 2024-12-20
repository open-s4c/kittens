

## Generate a C litmus test

To generate a C litmus test out of the kitten, we first need a concrete set of
events executed by threads, which in theory should not display the said
behavior.  For that, we translate the kitten to an SMT query and let `z3` fill
the gaps (when the kitten is valid).

With the SMT result, we can generate a concrete C litmus test with the `defuse`
command.

    cmd/grill.scm po fr po fr | z3 -in
