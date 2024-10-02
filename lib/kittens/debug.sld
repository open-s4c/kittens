(define-library (kittens debug)
  (export pretty-print
          print-results)

  (import (scheme base)
          (scheme file)
          (scheme write)
          (kittens packrat))
  (cond-expand
    (chicken (import (srfi 48)))
    (else (import (chibi show)
                  (chibi show pretty))))
  (begin
    ;; TODO these two functions are not available in chicken
    ;; we need to do something about it
    (cond-expand
      (chicken
       (define (show . xs) (apply display xs))
       (define (pretty x) x)))

    (define (pretty-print . xs)
      (for-each (lambda (x)
                  (show (current-output-port) (pretty x)))
                xs))

    (define (print-results r)
      (define (results-iter x)
        (when x
          (results-iter (parse-results-next x))
          (newline)
          (pretty-print (parse-results-token-value x))
          (pretty-print (parse-results-token-kind x))))
      (results-iter (parse-results-next r)))))
