; -*- scheme -*-

(import (scheme small)
	(chibi test)
        (srfi 166)
        (rebottled packrat))

(include "parser.scm")
(include "generator.scm")
(include "lexer.scm")

(test-begin "lexer")

(define (parse g)
    (lexer (base-generator->results g)))

(let ((input "NOT_SC"))
    (define r (parse (str-generator input)))
    (test-assert (parse-result-successful? r))
    (test '((id "NOT_SC")) (parse-result-semantic-value r)))

(let ((input "A B 3.c"))
    (define r (parse (str-generator input)))
    (test-assert (parse-result-successful? r))
    (test '((id "A") (id "B") (id "3.c")) (parse-result-semantic-value r)))

(let ((input "A B 3.c "))
    (define r (parse (str-generator input)))
    (test-assert (parse-result-successful? r))
    (test '((id "A") (id "B") (id "3.c")) (parse-result-semantic-value r)))

(let ((input "A B   3.c"))
    (define r (parse (str-generator input)))
    (test-assert (parse-result-successful? r))
    (test '((id "A") (id "B") (id "3.c")) (parse-result-semantic-value r)))

(let ((input "   A B 3.c"))
    (define r (parse (str-generator input)))
    (test-assert (parse-result-successful? r))
    (test '((id "A") (id "B") (id "3.c")) (parse-result-semantic-value r)))

(let ((fn "toy.cat"))
    (define r (parse (file-generator fn)))
    (test-assert (parse-result-successful? r))
    (test '((id "SC")) (parse-result-semantic-value r)))

(test-end)
