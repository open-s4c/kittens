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

(define (usage)
  (print "explode <model file> <edges>"))

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
                      (print "# WARNING: cannot include '" (cadr stmt) "'")
                      (iter rest nstmts)))
                (iter rest (cons stmt nstmts))))))
    (list 'model (cadr model) (reverse (iter stmts '())))))

(define (include-file model fn)
  (if (file-exists? fn)
      (let* ((tks (tokenize-cat fn)))
        ;(display tks)
        ;(newline)
        (let* ((imodel (parse-cat tks))
               (istmts (caddr imodel))
               (stmts (caddr model)))
          ;(display istmts)
          ;(newline)
          (list 'model (cadr model) (append istmts stmts))))
      model))

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

(define (flatten lst)
  (cond
    ((null? lst) '())
    ((not (pair? (car lst)))
     (cons (car lst) (flatten (cdr lst))))
    (else
     (append (flatten (car lst)) (flatten (cdr lst))))))


(define (cartesian-product lst-of-lsts)
  (if (null? lst-of-lsts)
      '(())  ; Base
      (let ((rest-products (cartesian-product (cdr lst-of-lsts))))
        (apply append
               (map (lambda (x)
                      (map (lambda (y) (cons x y))
                           rest-products))
                    (car lst-of-lsts))))))

(define (explode-expr expr ht)
  (define (explode expr)
    (match expr
           (('union . exprs) (apply append (map explode  exprs)))
           (('seq . exprs)
            (cartesian-product (map explode exprs)))
           (('set . label) (list label))
           (('self . label)
            (map (lambda (l)
                   (string-append "[" l "]"))
                 (explode label)))
           (('rel . label)
            (if (pair? label)
                (list (string-append "[" (cdr label) "]"))
                (if (hash-table-exists? ht label)
                    (explode (hash-table-ref ht label))
                    (list label))))
           (else (list expr))))
  (explode expr))

(define (explode-accs model ht)
  (let* ((stmts (caddr model))
         (accs (filter (lambda (x) (eq? (car x) 'acyclic)) stmts)))
    (apply append (map (lambda (acc) (explode-expr (cadr acc) ht)) accs))))

(define (dfs edges path d)
  (if (eq? d 0)
      (list path)
      (apply append (map (lambda (edge)
                           (dfs edges (append path  edge) (- d 1))
                           ) edges))))

(define (cycles-print cycles)
  (define (print-plus cycle)
    (display cycle)
    (display " "))
  (for-each (lambda (cycle) (for-each (lambda (el) (print-plus el)) cycle) (newline)) cycles))

(define (main args)
  (die-unless (= (length args) 2) "wrong arguments" usage)

  (let* ((fn (car args))
         (len (car (cdr args)))
         (len (string->number len))
         (port (open-input-file fn)))
    (print "# model file: " fn)
    (print "# cycle len: " len)

    (let ((tokens (tokenize-cat fn)))

      (let* ((model (parse-cat tokens))
             (model (include-file model "models/kittens.cat"))
             (model (include-files model)))

        ;(display model)
        ;(newline)
        (let* ((ht (get-hash-table model))
               (edges (explode-accs model ht))
               (edges (map (lambda (e) (flatten (list e))) edges))
               (cycles (dfs edges '() len)))

	  (display ht)
          ;(display model)
          ;(newline)
          ;(display edges)
          ;(newline)
          (cycles-print cycles)))))
  0)

(start-command main)


