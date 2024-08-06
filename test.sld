(define-library (kittens test)
  (export code-append
          print-results
          test-group
          test-begin
          test-end
          test
          test-assert
          )
  (import (scheme base)
          (chibi test)
          (kittens debug)
          (srfi 166))
  (begin
    (define (code-append . lines)
      (apply string-append
             (map (lambda (line) (string-append line "\n"))
                  lines)))))
