(define-library (kittens command)
  (export print
          die-unless
          parse-or-die
          start-command

          ; reexport
          display
          newline
          string-join)

  (import (scheme base)
          (scheme write)
          (only (srfi 130) string-contains)
          (only (srfi 193) command-args command-line)
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

    (define (start-command foo)
      (let ((arg0 (car (command-line))))
        (unless (string-contains arg0 ".tst")
          (foo (command-args)))))))
