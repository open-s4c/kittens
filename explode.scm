#!/usr/bin/env chibi-scheme

(import (scheme small)
        (srfi 193) ; command-args
        (srfi 166)
        (rebottled packrat)
        (cat parser))

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


(define (include-files model)
  (let ((stmts (caddr model)))
    (define (iter stmts nstmts)
      (if (null? stmts)
          nstmts
          (let ((stmt (car stmts))
                (rest (cdr stmts)))
            (if (eq? 'include (car stmt))
                (if (file-exists? (cadr stmt))
                    (let* ((tks (tokenize-cat (cadr stmt)))
                           (imodel (parse-cat tks))
                           (istmts (caddr imodel)))
                      (iter rest (append istmts nstmts)))
                    (begin
                      (print "WARNING: cannot include '" (cadr stmt) "'")
                      (iter rest nstmts)))
                (iter rest (cons stmt nstmts))))))
    (list 'model (cadr model) (reverse (iter stmts '())))))

(define (main args)
  (die-unless (= (length args) 2) "wrong arguments")

  ; car , cdr
  ; (list 1 2 3) == '(1 2 3)
  ; lst <- '(1 2 3)
  ; (car lst) -> 1
  ; (cdr lst) -> '(2 3)

  (let* ((fn (car args))
         (len (car (cdr args)))
         (len (string->number len))
         (port (open-input-file fn)))
    (print "model file: " fn)
    (print "cycle len: " len)

    (let ((tokens (tokenize-cat fn)))
      (pretty-print tokens)
      (newline)
      (display "-------------------------------------------------")
      (newline)
      (let* ((model (parse-cat tokens))
             (model (include-files model)))

        (pretty-print model)

        )))
  0)

(main (command-args))


