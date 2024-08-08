(define-library (kittens match)
  (export match)
  (cond-expand
    (chicken (import matchable))
    (else (import (chibi match)))))
