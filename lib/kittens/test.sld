(define-library (kittens test)
  (export code-append
          print-results
          test-group
          test-begin
          test-end
          test
          test-assert)
  (import (scheme base)
          (kittens debug))
  (cond-expand
    (chicken (import (srfi 64)))
    (else (import (chibi test))))

  (begin
    (define (code-append . lines)
      (apply string-append
             (map (lambda (line) (string-append line "\n"))
                  lines)))))
