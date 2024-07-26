(import (scheme small)
        (srfi 166)
        (chibi test)
        (rebottled packrat))

(include "stmt.scm")
(include "generator.scm")
(define generator token-generator)

(test-begin "expr")
; and * . The postfix operator ^-1 performs relation inversion. The construct expr ? (option) evaluates
; to the union of expr value and of the identity relation. Notice that postfix operators operate on relations
; only.
; There is one prefix operator ~ that performs relation and set complement.

; Finally, there is one last unary operator: [ expr ] that evaluate expr to an event set and returns the
; identity relation over this set.

; Infix operators are
; - | (union),
; - ++ (set addition),
; - ; (sequence)
; - & (intersection)
; - \ (set difference)
; - * (cartesian product).

; Infix operators are listed in order of increasing precedence, while
; postfix and prefix operators bind tighter than infix operators.
; All infix operators are right-associative, except set difference which is
; left-associative, and cartesian product which is non-associative.

; The union, intersection and difference operators apply to relations and
; all kinds of sets.

; cartesian product takes two event sets as arguments and returns a relation.

; The addition operator expr1 ++ expr2 operates on sets: the value of expr2 must be a set of values (or an event, or an element of relation) S and the operator returns the set S augmented with the value of expr1 ; (or a new event set, or a new relation). By exception, the arguments to the addition operator can also be ; an elementary relation. It then yields a relation.

(let ()
  (define g (generator
             '((let) (id . hb) (=) (oparen) (set . A) (union) (set . B) (cparen) (cart) (set . C) (union) (rel . R))))

  (define r (stmt-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(test-end)
