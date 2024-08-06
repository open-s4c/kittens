; -*- scheme -*-

(import (scheme small)
        (rebottled packrat)
        (kittens generator)
        (kittens test))

(include "expr.scm")
(include "stmt.scm")

(test-begin "stmt")

(let* ((g (token-generator
           ; let hb = (A | B)*C | R
           '((let) (id . hb) (=) (oparen) (set . A) (union)
                   (set . B) (cparen) (cart) (set . C) (union) (rel . R))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let hb (union (cart (union (set . A) (set . B))
                              (set . C))
                        (rel . R)))
        (parse-result-semantic-value r)))

(test-end)
