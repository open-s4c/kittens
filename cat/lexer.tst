; -*- scheme -*-

(import (scheme small)
        (rebottled packrat)
        (kittens generator)
        (kittens test))

(include "lexer.scm")

(test-begin "lexer")

(define (parse g)
  (lexer (base-generator->results g)))

(let ((input "NOT_SC"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "NOT_SC")) (parse-result-semantic-value r)))

(let ((input "A B 3.c"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "A B 3.c "))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "A B   3.c"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "   A B 3.c"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((text (code-append
             "SC"
             "include cos.cat"
             "// a nice comment"
             "another"
             ""
             "(* something useless *)"
             "final"
             ""
             "(* here is a comment with multiple lines i"
             "and it also will terminate using // in the same line as *)"
             ""
             "include \"string.cat\" //"
             ""
             "( A )[ A|B ] (* hello world *)"
             ""
             "let hx = [R] D"
             ""
             "acyclic hb"
             )))
  (define r (parse (str-generator text)))
  (test-assert (parse-result-successful? r))
  (test '((id . "SC")
          (include) (id . "cos.cat")
          (id . "another") (id . "final")
          (include) (string . "string.cat")
          (oparen) (id . "A") (cparen)
          (obrack) (id . "A") (union) (id . "B") (cbrack)
          (let) (id . "hx") (=)
          (obrack) (id . "R") (cbrack) (id ."D")
          (acyclic) (id . "hb"))
        (parse-result-semantic-value r)))

(test-end)
