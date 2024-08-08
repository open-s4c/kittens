; -*- scheme -*-

(import (scheme small)
        (kittens packrat)
        (kittens generator)
        (kittens test))

(include "expr.scm")

(define (parse tokens)
  (let ((g (token-generator tokens)))
    (expr-parser (base-generator->results g))))

(test-begin "expr")

(let* ((r (parse '((set . "set1") (isect) (set . "set2")))))
  (test-assert (parse-result-successful? r))
  (test '(isect (set . "set1") (set . "set2"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((id . "W") (isect) (set . "set2")))))
  (test-assert (parse-result-successful? r))
  (test '(isect (set . "W") (set . "set2"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((id . "W") (isect) (id . "R")))))
  (test-assert (parse-result-successful? r))
  (test '(isect (set . "W") (set . "R"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((id . "W") (cart) (id . "R")))))
  (test-assert (parse-result-successful? r))
  (test '(cart (set . "W") (set . "R"))
        (parse-result-semantic-value r)))


(let* ((r (parse '((id . "po") (isect) (id . "W") (cart) (id . "R")))))
  (test-assert (parse-result-successful? r))
  (test '(isect (rel . "po") (cart (set . "W") (set . "R")))
        (parse-result-semantic-value r)))

(test-end)