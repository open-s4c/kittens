(define-library (kittens utils)
  (export unique
          seq)

  (import (scheme base))
  (begin
    (define (unique lst)
      (define (iter lst out)
        (if (null? lst)
            out
            (if (member (car lst) out)
                (iter (cdr lst) out)
                (iter (cdr lst) (cons (car lst) out)))))
      (iter lst '()))

    (define (seq n)
      (let loop ((i 0) (lst '()))
        (if (= i n)
            lst
            (loop (+ i 1) (cons (- n i 1) lst)))))))
