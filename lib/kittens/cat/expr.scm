; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define predefined-sets
  (map symbol->string
       '(M ; memory accesses
         A ; Atomic operations
         F ; fences
         W ; write operations
         R ; read operations
         B
         RMW  ; read-modify-write operations
         XCHG ;
         FAA  ; fetch and add
         CAS
         CAS-S
         CAS-F
         RLX
         ACQ  ; atomic operation with Acq barrier
         REL  ; atomic operation with Rel barrier
         SC   ; atomic operation with SC barrier
         Marked ; marked atomic operation
         Plain ; Unmarked operation
         )))

(define (preset results)
  (let ((tok (parse-results-token-value results)))
    (if (member tok predefined-sets)
        (make-result tok (parse-results-next results))
        (make-expected-result
         (parse-results-position results) 'id))))

(define expr-parser
  (packrat-parser
   expr

   (expr
    ((x <- rels) x)
    ((x <- sets) x))

   ; Event sets: W,R,RMW,F,ACQ,REL,SC

   ; Operations: high-to-low = | * &
   ; * = sets |-> relations
   ; [X] == x,x  : x \in X


   ; Infix operators are
   ; - | (union),
   ; - ++ (set addition) ; TODO: how to deal with this
   ; - ; (sequence)
   ; - & (intersection)
   ; - \ (set difference)
   ; - * (cartesian product)

   (sets ((x <- set-union) x)
         ((x <- set-isect) x)
         ((x <- set-diff) x)
         ((x <- set-not) x)
         ((x <- set) x))

   (set-union
    ((a <- set-isect 'union b <- set-union) (list 'union a b))
    ((a <- set-isect) a))

   (set-isect
    ((a <- set-diff 'isect b <- set-isect) (list 'isect a b))
    ((a <- set-diff) a))

   (set-diff
    ;((a <- set 'diff b <- set-diff) (list 'diff a b))
    ; TODO: \ is left associative!
    ((a <- set 'diff b <- set) (list 'diff a b))
    ((a <- set-not) a))

   (set-not
    (('not a <- rel) (cons 'not a))
    (('not a <- set) (cons 'not a))
    ((a <- rel) a)
    ((a <- set) a))

   (set
    ((s <- preset) (cons 'set s))
    ((s <- 'set) (cons 'set s))
    (('oparen a <- sets 'cparen) a))

   ; let implied = po & ( W*R & ((M * A) | (A * M)) )
   ; Infix operators are
   ; - | (union),
   ; - ++ (set addition) ; TODO: how to deal with this
   ; - ; (sequence)
   ; - & (intersection)
   ; - \ (set difference)
   ; - * (cartesian product)

   (rels ((x <- rel-union) x)
         ((x <- rel-seq) x)
         ((x <- rel-isect) x)
         ((x <- rel-cart) x)
         ((x <- rel-inv) x)
         ((x <- rel-not) x)
         ((x <- rel) x))

   (rel-union
    ((a <- rel-seq 'union b <- rel-union) (list 'union a b))
    ((a <- rel-seq) a))

   (rel-seq
    ((a <- rel-isect 'seq b <- rel-seq) (list 'seq a b))
    ((a <- rel-isect) a))

   (rel-isect
    ((a <- rel-cart 'isect b <- rel-isect) (list 'isect a b))
    ((a <- rel-cart) a))

   (rel-cart
    ((a <- set 'cart b <- rel-cart) (list 'cart a b))
    ((a <- rel-inv) a))

   (rel-inv
    ((a <- rel 'inv) (cons 'inv a))
    ((a <- rel-not) a))

   (rel-not
    (('not a <- rel) (cons 'not a))
    (('not a <- set) (cons 'not a))
    ((a <- rel) a)
    ((a <- set) a))

   (rel
    (((! preset) r <- 'id) (cons 'rel r))
    ((r <- 'rel) (cons 'rel r))
    (('obrack s <- sets 'cbrack) (cons 'self s))
    (('oparen a <- rels 'cparen) a)
    )))
