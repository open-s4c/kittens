; -*- scheme -*-

(import (scheme base))

(include "model.scm")

(test-begin "model-parser")

(let ()
  (define g (generator
             '((let) (id . hb) (=) (oparen) (set . A) (union) (set . B) (cparen) (cart) (set . C) (union) (rel . R))))

  (define r (model-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(test-end)
