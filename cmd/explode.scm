#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (scheme cxr)
        (only (srfi 1) filter)
        (kittens cat)
        (kittens match)
        (kittens generator)
        (kittens command))

(cond-expand ; hash-table
  (chicken (import (srfi 69)))
  (else (import (srfi 125)
                (only (srfi 128) string-hash))))

(define (tokenize-cat fn)
  (parse-or-die lexer (file-generator fn)))

(define (parse-cat tokens)
  (parse-or-die model-parser (token-generator tokens)))

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

(define (get-hash-table model)
  (let ((ht (make-hash-table equal? string-hash 1024))
        (stmts (caddr model)))
    (for-each (lambda (stmt)
                (when (eq? 'let (car stmt))

                  (let ((label (cadr stmt))
                        (expr (caddr stmt)))
                    (hash-table-set! ht label expr))))
              stmts)
    ht))

(define (explode-expr expr ht)
  (define (explode expr)
    (match expr
           (('union . exprs) (apply append (map explode exprs)))
           (('rel . label) (if (hash-table-exists? ht label)
                               (explode (hash-table-ref ht label))
                               (list expr)))
           (else `(,expr))
           ))
  (explode expr))

(define (explode-accs model ht)
  (let* ((stmts (caddr model))
         (accs (filter (lambda (x) (eq? (car x) 'acyclic)) stmts)))
    (apply append (map (lambda (acc) (explode-expr (cadr acc) ht)) accs))))

(define (dfs edges path d)
  (if (eq? d 0)
      (list path)
      (apply append (map (lambda (edge)
                           (dfs edges (append path (list (cdr edge))) (- d 1))
                           ) edges))))

(define (cycles-print cycles)
  (define (print-plus cycle)
    (display cycle)
    (display " ")
    )
  (for-each (lambda (cycle) (for-each (lambda (el) (print-plus el)) cycle) (newline)) cycles))

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
    (print "# model file: " fn)
    (print "# cycle len: " len)

    (let ((tokens (tokenize-cat fn)))
      ;(pretty-print tokens)
      ;(newline)
      ;(display "-------------------------------------------------")
      ;(newline)
      (let* ((model (parse-cat tokens))
             (model (include-files model))
             (ht (get-hash-table model))
             (edges (explode-accs model ht))
             (cycles (dfs edges '() len)))
        (cycles-print cycles))))
  0)

(start-command main)


