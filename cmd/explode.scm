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
; member
(define (usage)
  (print "explode <model file> <edges>"))

(define (but-last xs) (reverse (cdr (reverse xs))))

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
    (cond ((null? lst) '())
	          ((not (pair? lst)) (list lst))
		          (else (append (flatten (car lst)) (flatten (cdr lst))))))

;(define (flatten lst)
;  (cond
;    ((null? lst) '())
;    ((not (pair? (car lst)))
;     (cons (car lst) (flatten (cdr lst))))
;    (else
;     (append (flatten (car lst)) (flatten (cdr lst))))))
;

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
           (('union . exprs) (apply append (map explode exprs)))
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

(define (explode-expr-t expr ht)
  (define (explode expr)
    (match expr
	  (('union . exprs) (list 'union (explode (car exprs)) (explode (cadr exprs))))
	  (('seq . exprs) (let* ((left (explode (car exprs)))
				 (right (explode (cadr exprs))))
			  (combine-op-unions left right 'seq)))
	  (('isect . exprs) (let* ((left (explode (car exprs)))
				   (right (explode (cadr exprs))))
			  (combine-op-unions left right 'isect)))
	  (('rel . label) 
	   	(if (hash-table-exists? ht label)
		    (explode (hash-table-ref ht label))
		    expr))
	  (('self 'set . label)
	   	(if (hash-table-exists? ht label)
		    (explode (hash-table-ref ht label))
		    expr))
	  (else (error "Unrecognized expression type" expr))))

  (define (combine-op-unions left right op)
    (match left
	  (('union . l-exprs) (list 'union 
				    (combine-op-unions (car l-exprs) right op)
				    (combine-op-unions (cadr l-exprs) right op)))

	   (else 
	     (match right
		   (('union . r-exprs) (list 'union 
		  			     (combine-op-unions left (car r-exprs) op)
					     (combine-op-unions left (cadr r-exprs) op)))

		    (else (list op left right))))))
  (explode expr))

(define (get-accs model)
  (map cadr (filter (lambda (ext) (eq? 'acyclic (car ext))) model)))

(define (get-empties model) 
  (map cadr (filter (lambda (ext) (eq? 'empty (car ext))) model))) 

(define (flatten-union expr)
  (match expr
    (('union . ex) (apply append (list (flatten-union (car ex)) (flatten-union (cadr ex)))))
    (else (list expr))))

(define (explode-empty-rule rule ht)
  (map (lambda (e) (list 'empty e)) (flatten-union (explode-expr-t rule ht))))

(define (explode-acyclic-rule rule ht d)
  (map (lambda (a) (list 'acyclic a)) (generate-combinations (flatten-union (explode-expr-t rule ht)) d)))

(define (explode-accs model ht)
  (let* ((stmts (caddr model))
         (accs (filter (lambda (x) (eq? (car x) 'acyclic)) stmts)))
    (apply append (map (lambda (acc) (explode-expr-t (cadr acc) ht)) accs))))

(define (generate-combinations edges n)
  (apply append (generate-combinations-h edges n)))

(define (generate-combinations-h edges n)
  (define (helper current-list n)
    (if (zero? n)
	current-list
	(map (lambda (edge)
	     (helper (list 'seq edge current-list) (- n 1)))
	     edges)))
  (apply append (map (lambda (edge) (helper edge (- n 1))) edges)))

(define (flatten-once lst)
  (cond ((and (pair? lst) (null? (cdr lst)) (pair? (car lst)))
	 (flatten-once (car lst)))  
	((pair? lst)
	 (cons (flatten-once (car lst)) (flatten-once (cdr lst))))
	(else lst)))  

(define (contains-isomorphism res cycle)
  (define (helper res cycle n)
    (if (eq? n 0)
        #f
        (if (or (member cycle res)
                (member (reverse cycle) res))
            #t
            (helper res (rotate-list cycle) (- n 1)))))
  (helper res cycle (length res)))

(define (rotate-list lst)
  (if (null? lst)
      lst
      (append (cdr lst) (list (car lst)))))

(define (remove-dub res remaining)
  (if (null? remaining)
      res
      (if (contains-isomorphism res (car remaining))
          (remove-dub res (cdr remaining))
          (remove-dub (append res (list (car remaining))) (cdr remaining)))))

(define (print-empty empty)
  (display "empty ")
  (print-stmt (cadr empty) 'first)
  (newline))

(define (print-acyclic acyclic)
  (display "acyclic ")
  (print-stmt (cadr acyclic) 'first)
  (newline))

(define (print-stmt stmt last)
  (let ((br (and (not (or (eq? (car stmt) 'rel) (eq? (car stmt) 'self)))
  		 (not (or (eq? last 'first) (eq? (car stmt) last))))))
  (if br (display "(")) 

  (match stmt
        (('seq . rest) (print-stmt (car rest) 'seq) (display ";") (print-stmt (cadr rest) 'seq))
        (('isect . rest) 
		(print-stmt (car rest) 'isect) (display "&") (print-stmt (cadr rest) 'isect))
        (('rel . rest) (display (match rest 
				       ("rfx" "rf")
				       (else rest))))
        (('self 'set . rest) (display "[") (display rest) (display "]"))
   	(else (display stmt)))
  (if br (display ")"))))
   
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

	(let* ((ht (get-hash-table model))
               (empty-rules (get-empties (caddr model)))
	       (acyclic-rules (get-accs (caddr model)))
	       (empties (apply append (map (lambda (e) (explode-empty-rule e ht)) empty-rules)))
	       (acyclics (apply append (map (lambda (a) (explode-acyclic-rule a ht len)) acyclic-rules)))
               )  	  
          (for-each print-empty empties)
	  (newline)
	  (for-each print-acyclic acyclics)
	  ))))
  0)

(start-command main)

(define test-expr 
    '(seq (isect (union (rel a1) (seq (rel b1) (rel b2))) (union (rel c1) (rel c2)))
	          (seq (isect (union (rel d1) (seq (rel e1) (rel e2))) (union (rel f1) (rel f2)))
		                    (union (rel g1) (isect (rel h1) (rel h2))))))


