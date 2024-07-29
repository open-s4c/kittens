; -*- scheme -*-

(import (scheme small)
        (chibi test)
        (srfi 166)
        (rebottled packrat))

(include "proc.scm")
(include "../cat/generator.scm")

(test-begin "proc")

(define (parse g)
  (proc-parser (base-generator->results g)))

(define (code . lines)
  (apply string-append
         (map (lambda (line)
                (string-append line "\n"))
              lines)))

(let ((input (code
              "P0 (volatile int* y,volatile int* x) {"
              "int r0 = *x;"
              "*y = 1;"
              "}"
              )))

  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id "NOT_SC")) (parse-result-semantic-value r)))
(test-end)
