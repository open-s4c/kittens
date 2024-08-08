; -*- scheme -*-

(import (scheme base)
        (kittens packrat)
        (kittens generator)
        (kittens test))

(include "expr.scm")
(include "stmt.scm")
(include "model.scm")

(test-begin "model")

(let* ((g (token-generator
           '((id . "TEST")
             (let) (id . hb) (=) (oparen) (set . A) (union) (set . B)
             (cparen) (cart) (set . C) (union) (rel . R))))
       (r (model-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(model (name "TEST")
                ((let hb (union (cart (union (set . A) (set . B))
                                      (set . C))
                                (rel . R)))))
        (parse-result-semantic-value r)))

(test-end)
