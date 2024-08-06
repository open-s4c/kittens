#!/usr/bin/env chibi-scheme

(import (scheme small)
        (srfi 193) ; command-args
        (srfi 166)
	(srfi 125) ; hash-table
	(srfi 128) ; symbol-hash
	(srfi 1)
        (rebottled packrat)
	(chibi match)
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
                      (iter rest nstmts))
		)
                (iter rest (cons stmt nstmts))
	    )
          )
    )
  )
    (list 'model (cadr model) (reverse (iter stmts '())))))
        
(define (get-hash-table model)
  (let ((ht (make-hash-table equal? symbol-hash 1024))
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
	     (ht (get-hash-table model)))	
        (let* ((edges (explode-accs model ht))
	      (cycles (dfs edges '() len)))
	    (cycles-print cycles) 
	)

        )))
  0)

(main (command-args))


