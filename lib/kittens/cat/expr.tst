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

(let* ((r (parse '((id . "rf") (seq) (id . "rmw")))))
  (test-assert (parse-result-successful? r))
  (test '(seq (rel . "rf") (rel . "rmw"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((id . "rf") (seq) (obrack) (id . "RMW") (cbrack)))))
  (test-assert (parse-result-successful? r))
  (test '(seq (rel . "rf") (self . (set . "RMW")))
        (parse-result-semantic-value r)))

(let* ((r (parse '((obrack) (id . "RMW") (cbrack) (seq) (id . "rf")))))
  (test-assert (parse-result-successful? r))
  (test '(seq (self . (set . "RMW")) (rel . "rf"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((id . "rf") (union) (id . "rf") (seq) (obrack) (id . "RMW") (cbrack)))))
  (test-assert (parse-result-successful? r))
  (test '(union (rel . "rf") (seq (rel . "rf") (self . (set . "RMW"))))
        (parse-result-semantic-value r)))

(let* ((r (parse '((obrack) (id . "RMW") (cbrack) (seq) (id . "rf") (union) (id . "rf") (seq) (obrack) (id . "RMW") (cbrack)))))
  (test-assert (parse-result-successful? r))
  (test '(union (seq (self . (set . "RMW")) (rel . "rf")) (seq (rel . "rf") (self . (set . "RMW"))))
        (parse-result-semantic-value r)))

(let ((r (parse '((obrack) (id . "W") (cbrack) (seq) (id . "xrf") (union) (obrack) (id . "RMW") (cbrack) (seq) (id . "xrf")))))
  (test-assert (parse-result-successful? r))
  (test '(union (seq (self . (set . "W")) (rel . "xrf")) (seq (self . (set . "RMW")) (rel . "xrf")))
        (parse-result-semantic-value r)))

(let ((r (parse '((obrack) (id . "FAA") (cbrack) (seq) (id . "rfx")))))
  (test-assert (parse-result-successful? r))
  (test '(seq (self . (seq . "FAA")) (id . "rfx"))
        (parse-result-semantic-value r)))

(test-end)
