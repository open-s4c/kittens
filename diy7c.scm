(import (scheme small)
        (srfi 193) ; command-args
        (srfi 166) ; formatting
        (srfi 125) ; hash tables
        (only (chibi string) string-join)
        (rebottled packrat)
        (diy7c parser))

(define (pretty-print x)
  (show (current-output-port) (pretty x)))

(define (print . xs)
  (for-each display xs)
  (newline))

(define-syntax die-unless
  (syntax-rules ()
    ((_ cnd msg)
     (unless cnd
       (print "<kittens> <model file> <cycle length>")
       (newline)
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


(define (generate-c tokens)
  ; print preamble in comment
  (let ((preamble (car tokens)))
    (when #t
      (print "/*")
      (for-each print (cdr preamble))
      (print "*/")))

  ; add includes
  (newline)
  (print "#include <stdatomic.h>")
  (newline)

  ; define vars if necessary, for now just assert empty
  (let ((vars (cadr tokens)))
    (when #f
      (print "/*")
      (for-each print (cdr vars))
      (print "*/")))

  ; collect all arguments
  (let ((procs (cdr (caddr tokens))))
    (let* ((args (map cdadr procs))
           (args (apply append args))
           (args (unique args)))
      (newline)
      (for-each (lambda (arg) (print arg ";"))
                args)
      (newline))

    ; create one function per proc and use the arguments as global variables
    (when #t
      (for-each (lambda (p)
                  (let ((pid (number->string (cdar p)))
                        (args (cdr (cadr p)))
                        (code (cdr (caddr p))))
                    (print "void " (string-append "run" pid " (" (string-join args ",") ") {"))
                    (for-each print code)
                    (print "}")))
                procs)))

  ; print exists as comment
  (let ((exists (cadddr tokens)))
    (when #t
      (print "// exists " (cdr exists))))
  )


(define (main args)
  (die-unless (not (null? args)) "input file")
  (let* ((fn (car args))
         (g (file-generator fn)))
    (let ((tokens (parse-or-die lexer g)))
      (generate-c tokens)))
  0)
(main (command-args))
