#import "@preview/fletcher:0.5.1" as fletcher: diagram, node, edge
#import "@preview/rubber-article:0.1.0": *

#show: article.with()

#let smm = { smallcaps([smm]) }
#let hmm = { smallcaps([hmm]) }
#let atomics = { smallcaps([atomics]) }
#let litc = { smallcaps([litmus/C]) }
#let lita = { smallcaps([litmus/A]) }
#let kittens = { smallcaps([Kittens]) }

//"Huawei Dresden OS Kernal Lab", ),

#maketitle(
  title: [#kittens %VER% --- Design Document],
  authors: ("github.com/open-s4c/kittens", ),
  date: datetime.today().display(),
)

= Introduction

#kittens is a toolkit to support the verification of atomic operations down
to the target assembly.

Atomic operations are the most fundamental primitives to build concurrent
programs.  They provide a common interface at C programming level, abstracting
away details of the implementation and target architecture.  Examples of
atomic operations are `atomic_load` and `atomic_exchange` in `stdatomic.h`
from C11 or `smp_load_acquire`, `atomic_cmpxchg` in Linux Kernel atomics.

At the C programming level, atomics are expected to respect a _software memory
model_ --- e.g., C11 memory model, Linux Kernel Memory Model (LKMM), VSync
Memory model (VMM) --- which specifies how the effect of the atomic operations
can be observed by different cores/threads in a system.  Developers rely on the
software memory model to claim the correctness of their concurrent programs
built upon the atomic operations.  Whether the atomic operations _indeed_
respect the software memory model, however, depends on their implementation for
the target architecture.  #kittens is a toolkit to support the verification of
exactly that.

= Verification approach

Given a target software memory model #smm, #kittens constructs (virtually) all
representative C-code examples of how the atomic interface (#atomics) is used
along with assertions of how these examples should not fail. These examples are
called litmus tests, more specifically, #litc tests. Using the atomic interface
and a desired hardware memory model #hmm, #kittens translates the high-level
litmus tests to low-level assembly litmus tests (#lita tests) the user can
determine whether the #atomics correctly implement the #smm on the platform's
#hmm with the help of a model checker such as Dartagnan or Herd7.  If the model
checker finds  a violation for any of these #lita, then #atomics is *broken*.

In the #kittens toolkit, these following three tools are responsible for the
main phases of our verification approach:

1. `explode`: from a cat model representing the #smm, `explode` creates a series
   of _kittens_, i.e., union-free subaxioms.
2. `defuse`: from each kitten, `defuse` create a #litc test
3. `assemble`: from each #litc test, `assemble` combine the test case with the
   #atomics implementation, resulting in a #lita test.

//#kittens creates several litmus tests (i.e., little test cases) out of a software
//memory model (encoded in the `.cat` format).  Each of these test cases
//represents a corner case that is expected to _never_ happen on the system
//providing the selected software memory model.

//The test cases are then compiled to the target architecture, e.g., AArch64, and
//subsequently disassembled.

The ISA of the target architecture is expected to respect a _hardware memory
model_.  Together, the hardware memory model and the assembly of the atomic
operations should implement the sofware memory model.  We employ a model
checker (either Dartagnan or Herd7) to explore all possible thread/core
interleavings and memory reorderings allowed in hardware.  If the model
checker finds a witness for any of these compiled programs, then one or more
atomic operations are broken; otherwise the atomics are correctly implemented.

#figure(
	scale(x:70%, y:70%,
		diagram(
			cell-size: 0mm,
			node-stroke: 2pt,
			edge-stroke: 2pt,
			spacing: 2em,
			mark-scale: 40%,
			{
				node((1,-1), corner-radius: 2pt, [`sw.cat`], fill: blue)
				node((5,-1), corner-radius: 2pt, [`hw.cat`], fill: green)
				node((1,0), corner-radius: 2pt, [`.kitten`])
				node((2,0), corner-radius: 2pt, [`.smt`])
				node((2,1), corner-radius: 2pt, [`.model`])
				node((3,1), corner-radius: 2pt, [`.litmus` \ C])
				node((3,2), corner-radius: 2pt, [`.c`])
				node((4,2), corner-radius: 2pt, [`.s`])
				node((4,3), corner-radius: 2pt, [`.litmus` \ ASM])
		  },
			edge((1,-1), (1,0), [explode], "->", bend: 0deg),
			edge((1,0), (2,0), [grill], "->", bend: 30deg),
			edge((2,0), (2,1), [z3], "->", bend: 0deg),
			edge((2,1), (3,1), [roast], "->", bend: 30deg),
			edge((3,1), (3,2), [defuse], "->", bend: 0deg),
			edge((3,2), (4,2), [gcc], "->", bend: 0deg),
			edge((4,2), (4,3), [pasm], "->", bend: 0deg),
			//edge((4,0), (7,0), [], "->", bend: -40deg),
			{
				node((4,1), [Check], fill: red)
				edge((1,-1), (4,1), "--|>", bend: 30deg)
				edge((3,1), (4,1), "--|>", bend: 0deg)
				node((5,3), [Check], fill: red)
				edge((4,3), (5,3), "--|>", bend: 0deg)
				edge((5,-1), (5,3), "--|>", bend: 0deg)
			})),
	supplement: [Figure],
	caption : [#kittens pipeline],
) <pipeline>


@pipeline depicts the kittens pipeline in more detail.  Software and
hardware memory models are written in the so-called _cat_ language.  A `.cat`
file contains contains the _constraints_ (i.e., axioms) of a memory model.
These constraints are compact ways of representing executions that allowed
or not allowed by the model.  In the figure, we refer to the software memory
model as `sw.cat` and the hardware memory as `hw.cat` without specifying
the exact memory models.

The pipeline has several steps.  Initially, the model constraints
are _exploded_ in many small constraints called _kittens_.  A kitten is a
sequence of events describing the interaction of threads via shared variables.
The kittens are transformed in SMT formulas (_grilled_) and given to `z3`
to find concrete values (an SMT model) to the constraint.  These values are
specify which threads execute which events of the the kitten, which values are
written and read, and so on.  Next, the SMT model is converted (_roasted_)
to a litmus C file, which is a program (in a sort of pseudo-C) that can be
easily interpreted by several model checkers.  Besides the program to be
executed, the litmus file encodes an invariant that shouldn't be violated
in any execution of the program under the `sw.cat` model.  At this point in
the pipeline, we run each litmus test with `sw.cat` to be certain that the
litmus test represent cases that should never happen.

The litmus C file is not aware of the concrete implementation of the atomic
operations, but only about their interface.  Hence, we convert (_defuse_)
the litmus C file to a real C program that can be executed natively.  At this
step, the actual implementation of the atomic operation is merged into the
test case via the typical compilation process.

Most model checkers cannot parse arbitrary binary objects (with exception of
Rmem).  The `pasm` step lift the disassembled C programs back to pseudo-ASM,
i.e., to a litmus ASM test.  Finally, we can run the litmus ASM in the model
checker using the `hw.cat` and ensure that no witness is found violating
the invariant of any of the generated kittens.  If the model checker finds a
witness for any of the kittens, then one or more atomic operations are broken.

= Explode --- From `.cat` to `.kitten`

The kittens project generates test cases out of such cat models. The axioms
are broken up in many little axioms, each representing a much smaller set
of executions. These little axioms are called _kittens_.

== Brief cat language explanation

== High-level algorithm:

- parsing
	- parse input .cat file
	- look for includes in .cat and parse those too
	- parse kitten.cat file
	- merge all parsed statements

- preparation
	- collect all "let x = expr" parsed statements
	- store them in a definitions map (`defm`).

- iteration over parsed axioms
	- expand axiom with `defm`
	- normalize axiom
	- visit axiom, exploding it in union-free axioms (called _kittens_)
		- validate each kitten
		- if valid, print kitten

```racket
(define (something x)
	(print x))
```
== Axiom AST

Axioms are either of the form `(empty . x)` or `(acyclic . x)`, where `x` is a parsed expression consisting of some of the following nodes:

- Terminals: pairs `(set . s)` and `(rel . s)`, where `s` is some string;
- Unary operators: pairs `(not . x)`, `(inv . x)`, and `(self . x)`, where `x`
	is an expression;
- Binary operators: `(union x y)`, `(seq x y)`, and `(isect x y)`, where
  `x` and `y` are expressions.


== The visit algorithm

The algorithm to produce outputs performs a depth first traversal of the AST.
Whenever a binary node is met, we visit the `x` first and push back the
processing of `y`.

== Discussion
- alternative implemeanttions

= Defuse --- From `.kitten` to `.C.litmus`

== intro explaining general workflow with SMT solver
  - phase constraint generation
  - solving
  - model to code

== main challenges
  - excessive use of registers down the line
  - tracing back from final implementation to original kitten
  - initialization to solve address dependencies
  - data types

== Design
- list of operations, etc etc
- list rules to convert language to events
- observer events

== Discussion
- alternative implementation (parse the .C.litmus ...)
- difference to diy7 tool

= Assemble --- From `.C.litmus` to `.S.litmus`

- compiling to assembly and adding implementation of #atomics.
- considerations about compiler optimizations and other issues (risks)
- from assembly, back to highlevel
- options to deploy binary to Rmem
- option to run on real hardware (difference to litmus tool)


//= Automating the checks

== Important examples

```
C empty addr;rf
Some Very Useful Information
{
  v0=addr0;
  v42=v0;
}

P0 (int* v0) {
    int r0 = *v0;
}

P1 (int* v42, int* v0) {
    int r0 = *v42;
    *r0 = 49;
}

exists (0:r0=49)
```

```



C empty rf;data;rf;addr;co
Some Very Useful Information
{
  v32=addr32;
  v2=addr2;
  v31=addr31;
}

P0 (int* v32) {
    int r0 = atomic_load_explicit((atomic_int *)v32, memory_order_seq_cst);
    int r1 = atomic_load_explicit((atomic_int *)v32, memory_order_seq_cst);
}

P1 (int* v32) {
    *v32 = 30;       // f
}

P2 (int* v2, int* v32) {
    int r0 = *v2;    // d
    *r0 = 37;        // e
}

P3 (int* v31, int* v2) {
    int r0 = *v31;    // b
    *v2 = r0;         // c
}

P4 (int* v31, int *v32) {
    *v31 = v32;       // a
}
exists (3:r0=v32/\2:r0=v32/\0:r0=37/\0:r1=30)
```


and

```
C empty data;rf;addr;co
Some Very Useful Information
{
  v0=addr0;
  v42=v0;
  v7=addr7;
}

P0 (int* v0) {
    int r0 = atomic_load_explicit((atomic_int *)v0, memory_order_seq_cst);
    int r1 = atomic_load_explicit((atomic_int *)v0, memory_order_seq_cst);
}

P1 (int* v42, int* v7) {
    int r0 = *v42; // a
    *v7 = r0;      // b
}

P2 (int* v7, int* v0) {
    int r0 = *v7;  // c
    *r0 = 18;      // d
}

P3 (int* v0) {
    *v0 = 15;      // e
}

exists (2:r0=v0/\0:r1=15/\0:r0=18)
```



== Creating a program out of the kitten

1. recursively create edges based on the kitten
2. group endpoints of edges as real events (partitions)
3. each event (partition) has a set of constraints given by the relations to
   which it participates

4. Now we can create for each event an assert or for each relation an assert.


=== what kind of constraints come into play

==== from self-relation over src/dst events
- src and dst are the same event
-


==== from rf, co, po over src/dst events
In all these cases, src.eid != dst.eid and

- rf:
	/\ src.type = writing
	/\ dst.type = reading
	/\ src.addr = dst.addr

- co:
	/\ src.type = writing
	/\ dst.type = writing
	/\ src.addr = dst.addr

- po:
	/\ src.tid = dst.tid

- data:
	/\ src.tid = dst.tid
	/\ src.type = reading
	/\ dst.type = writing
	/\ src.rval = dst.wval
	/\ src.addr != dst.addr

- addr:
	/\ src.tid = dst.tid
	/\ src.type = reading
	/\ src.rval = dst.addr
	/\ src.addr != dst.addr

- ctrl:



==== events

- reading from rf or from init
- if RMW write-v != read-v


==== for all rf




;; Observer threads can only observe writes that are adjacent to co edges
;; First we collect all events that are either the src or dst of a co edge
;; Then we propagate to all events with the same eid based on the eid-partition
;(define (update-co-properties events group-mates edges)

    ;; Initialize list for "co" events
    (define co-events '())

    ;; Property map for "co" edges
    (define (property-map co-events ev)
      (if (member ev co-events) t f))

    ;; Helper function to find to which eid group an event belongs to
    (define (find-group event groups)
      (filter (lambda (group) (member event group)) groups))

    ;; Collecting affected events based on "co" edges
    (for-each (lambda (edge)
                (let ((src (edge-src edge))
                      (dst (edge-dst edge))
                      (type (edge-type edge)))
                  (when (equal? type "co")
                    (set! co-events (cons src (cons dst co-events))))))
              edges)

    ;; Collect all affected "co" group-mates
    (let ((all-co (apply append (map (lambda (co-ev) (car (find-group co-ev group-mates))) co-events))))
      (map (lambda (ev) (cons ev (property-map all-co ev))) events)))

;; In exists condition we should only assert reads that are dst of rf edges
;; First we collect all events that are dst of a rf edge
;; Then we propagate to all events with the same eid based on the eid-partition
;(define (update-rf-properties events group-mates edges)

    ;; Initialize list for "rf" events
    (define rf-events '())

    ;; Property map for "rf" edges
    (define (property-map rf-events ev)
      (if (member ev rf-events) t f))

    ;; Helper function to find to which eid group an event belongs to
    (define (find-group event groups)
      (filter (lambda (group) (member event group)) groups))

    ;; Collecting affected events based on "rf" edges
    (for-each (lambda (edge)
                (let ((dst (edge-dst edge))
                      (type (edge-type edge)))
                  (when (equal? type "rf")
                    (set! rf-events (cons dst rf-events)))))
              edges)
    ;(print rf-events)
    ;; Collect all affected "rf" group-mates
    (let ((all-rf (apply append (map (lambda (rf-ev) (car (find-group rf-ev
                                                                      group-mates))) rf-events))))
      (map (lambda (ev) (cons ev (property-map all-rf ev))) events)))


;; Different events write/read from different locations based on their types and edges they are connected to
;; This information is stored in the arg field
;; If an event comes after a data edge it has the data arg
;; If an event comes after an addr edge it has the addr arg
;; If an event comes after the first part of a ctrl (ctrl-a) it has the ctrl arg

;; First we collect all events that are dst of data/addr/ctrl edges
;; Then we propagate to all events with the same eid b ased on the eid-partition
;(define (update-dep-properties events group-mates edges)

    ;; Initialize lists for different properties
    (define data-events '())
    (define addr-events '())
    (define ctrl-events '())

    ;; Propert map that tells us to what arg each event is mapped
    (define (property-map data-events addr-events ctrl-events ev)
      (cond
        ((member ev data-events) "data")
        ((member ev addr-events) "addr")
        ((member ev ctrl-events) "ctrl")
        (else "reg")))

    ;; Helper function to find to which eid group an event belongs to
    (define (find-group event groups)
      (filter (lambda (group) (member event group)) groups))

    ;; Collecting affected events based on edge types
    (for-each (lambda (edge)
                (let ((dst (edge-dst edge))
                      (type (edge-type edge)))
                  (cond
                    ((equal? type "data-dep")
                     (set! data-events (cons dst data-events)))
                    ((equal? type "addr-dep")
                     (set! addr-events (cons dst addr-events)))
                    ((equal? type "ctrl-a-dep")
                     (set! ctrl-events (cons dst ctrl-events))))))
              edges)

    ;; We propagate affected events' arguments to all events with the same eid
    (let ((all-data (apply append (map (lambda (data-ev) (car (find-group data-ev group-mates))) data-events)))
          (all-addr (apply append (map (lambda (addr-ev) (car (find-group addr-ev group-mates))) addr-events)))
          (all-ctrl (apply append (map (lambda (ctrl-ev) (car (find-group ctrl-ev group-mates))) ctrl-events))))
      (map (lambda (ev) (cons ev (property-map all-data all-addr all-ctrl ev))) events)))


;; Edges of the form [X] connect an event with its self therefore they have the same eid
;; If the acyclic keyword is used, the first and last event have the same eid
;; We get the eid groups after performing simple uf
;; The we collect all pairs of rf edges
;; Finally, we use the rf-uf procedure to get the eid-partition of all events
;(define (get-eid-partition events edges is-acyclic)
    (let ((same-eid-sets (get-same-eid-set edges))
           (same-eid (if is-acyclic (append same-eid-sets (list (list 1 0))) same-eid-sets))
           (groups (simple-uf (map list events) same-eid))
           (rf-pairs (get-rf-pairs edges))
           (groups (rf-uf groups rf-pairs)))
      groups))

;; Helper method to get all pairs of rf edges
;; First we collect all edges with type rf
;; We transform them in a simplified form (we only care about src and dst)
;; We get all pairs
;(define (get-rf-pairs edges)
    (let ((rf-edges (filter (lambda (edge) (equal? (edge-type edge) "rf")) edges))
           (rf-edges (map (lambda (e) (list (edge-src e) (edge-dst e))) rf-edges))
           (rf-pairs (all-pairs rf-edges))
           (rf-pairs (map (lambda (pair) (list (list (cadar pair) (cadadr pair)) (list (caar pair) (caadr pair)))) rf-pairs)))
      rf-pairs
      ))

