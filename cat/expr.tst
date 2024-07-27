#!/usr/bin/env -S chibi-scheme -I..

(import (scheme small)
        (srfi 166)
        (chibi test)
        (rebottled packrat))


(define (pretty-print x)
  (show (current-output-port) (pretty x)))

(define (print . xs)
  (for-each display xs)
  (newline))

(include "expr.scm")
(include "generator.scm")
(define generator token-generator)

(define (print-results r)
  (define (results-iter x)
    (when x
      (results-iter (parse-results-next x))
      (newline)
      (pretty-print (parse-results-token-value x))
      (pretty-print (parse-results-token-kind x))))
  (results-iter (parse-results-next x)))

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

(test-end) (exit)
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

