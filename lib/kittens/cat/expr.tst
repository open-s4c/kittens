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

(let* ((r (parse '((rel . "set1") (inv)))))
  (test-assert (parse-result-successful? r))
  (test '(inv . (rel . "set1"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((rel . "set1") (union) (rel . "rel1") (inv)))))
  (test-assert (parse-result-successful? r))
  (test '(union (rel . "set1") (inv . (rel . "rel1")))
        (parse-result-semantic-value r)))

(let* ((r (parse '((obrack) (set . "rel1") (cbrack) (inv)))))
  (test-assert (parse-result-successful? r))
  (test '(inv self set . "rel1")
        (parse-result-semantic-value r)))

(let* ((r (parse '((oparen) (rel . "rel1") (union) (rel . "rel2") (cparen) (inv)))))
  (test-assert (parse-result-successful? r))
  (test '(inv . (union (rel . "rel1") (rel . "rel2")))
        (parse-result-semantic-value r)))

(let* ((r (parse '((rel . "rel1") (isect) (rel . "rel2") (inv)))))
  (test-assert (parse-result-successful? r))
  (test '(isect (rel . "rel1") (inv . (rel . "rel2")))
        (parse-result-semantic-value r)))

(let* ((r (parse '((rel . "rel1") (seq) (rel . "rel2") (inv)))))
  (test-assert (parse-result-successful? r))
  (test '(seq (rel . "rel1") (inv . (rel . "rel2")))
        (parse-result-semantic-value r)))

(let* ((r (parse '((not) (rel . "rel1") (seq) (rel . "rel2")))))
  (test-assert (parse-result-successful? r))
  (test '(seq (not rel . "rel1") (rel . "rel2"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((not) (oparen) (rel . "rel1") (seq) (rel . "rel2") (cparen)))))
  (test-assert (parse-result-successful? r))
  (test '(not seq (rel . "rel1") (rel . "rel2"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((not) (obrack) (set . "set1") (isect) (set . "set2") (cbrack)))))
  (test-assert (parse-result-successful? r))
  (test '(not self isect (set . "set1") (set . "set2"))
        (parse-result-semantic-value r)))

(let* ((r (parse '((not) (oparen) (rel . "rel1") (seq) (rel . "rel2") (cparen)))))
  (test-assert (parse-result-successful? r))
  (test '(not seq (rel . "rel1") (rel . "rel2"))
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

(let* ((r (parse '((obrack) (id . "W") (isect) (id . "RMW") (cbrack)))))
  (test-assert (parse-result-successful? r))
  (test '(self isect (set . "W") (set . "RMW"))
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

(let ((r (parse '((obrack) (id . "XCHG") (cbrack) (isect) (oparen) (oparen) (oparen) (oparen) (obrack) (id . "W") (cbrack) (seq) (id . "rf") (seq) (obrack) (id . "R") (cbrack) (cparen) (seq) (id . "co") (cparen) (isect) (id . "ext") (cparen) (seq) (oparen) (id . "co") (isect) (id . "ext") (cparen) (cparen)))))
  (test-assert (parse-result-successful? r))
  (test '(seq (self . (set . "FAA")) (rel . "rfx"))
        (parse-result-semantic-value r)))

(let ((r (parse '((obrack) (id . "FAA") (cbrack) (seq) (id . "rfx")))))
  (test-assert (parse-result-successful? r))
  (test '(seq (self . (set . "FAA")) (rel . "rfx"))
        (parse-result-semantic-value r)))

(test-end)
