(define-library (kittens command)
  (export print
          die-unless
          parse-or-die
          unique
          seq

          ; reexport
          command-args
          display
          newline
          string-join)

  (import (scheme base)
          (scheme write)
          (only (srfi 193) command-args)
          (only (srfi 130) string-join)
          (kittens packrat))
  (begin
    (define (print . xs)
      (for-each display xs)
      (newline))

    (define-syntax die-unless
      (syntax-rules ()
        ((_ cnd msg)
         (unless cnd
           (usage)
           (error 'argument-error msg 'cnd)))))

    (define (parse-or-die parser generator)
      (let ((result (parser (base-generator->results generator))))
        (if (parse-result-successful? result)
            (parse-result-semantic-value result)
            (error 'parse-error "parse error"
                   (let ((e (parse-result-error result)))
                     (list 'parse-error
                           (parse-position->string (parse-error-position e))
                           (parse-error-expected e)
                           (parse-error-messages e)))))))

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
