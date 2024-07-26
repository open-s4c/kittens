(import (scheme small)
        (srfi 166)
        (chibi test)
        (rebottled packrat))


(include "expr.scm")
(include "generator.scm")
(define generator token-generator)

(test-begin "expr")

(let ()
  (define g (generator
             '((oparen) (set . A) (union) (set . B) (cparen) (cart) (set . C) (union) (rel . R))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(let ()
  (define g (generator
             '((set . A) (cart) (set . B) (union) (rel . R))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(let ()
  (define g (generator
             '((num . A) (*) (num . B) (+) (num . V))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(let ()
  (define g (generator
             '((set . A) (diff) (set . B) (union) (set . C) (isect) (set . D))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(let ()
  (define g (generator
             '((set . A) (isect) (set . B) (union) (set . C))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))


(let ()
  (define g (generator
             '((set . A) (union) (set . B) (isect) (set . C))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))

(let ()
  (define g (generator
             '((oparen) (set . A) (union) (set . B) (union) (set . V) (cparen))))

  (define r (expr-parser (base-generator->results g)))
  (test-assert (parse-result-successful? r))
  (test 10 (parse-result-semantic-value r)))


(test-end)

