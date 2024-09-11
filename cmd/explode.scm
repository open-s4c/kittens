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
        (let* ((imodel (parse-cat tks))
               (istmts (caddr imodel))
               (stmts (caddr model)))
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

(define (explode-expr-t expr ht)
  (define (explode expr)
    (display expr)
    (newline)
    (match expr
	  (('union . exprs) (list 'union (explode (car exprs)) (explode (cadr exprs))))
	  
	  (('seq . exprs) (let* ((left (explode (car exprs)))
				 (right (explode (cadr exprs))))
			  (combine-op-unions left right 'seq)))
	  
	  (('isect . exprs) (let* ((left (explode (car exprs)))
				   (right (explode (cadr exprs))))
			  (combine-op-unions left right 'isect)))
	  
	  (('inv . expr) (combine-op-single (explode expr) 'inv))
	  
	  (('self . expr) (combine-op-single (explode expr) 'self))
          
	  (('not . rest) (combine-op-not (explode rest)))

	  (('rel . label) 
	   	(if (hash-table-exists? ht label)
		    (explode (hash-table-ref ht label))
		    expr))

	  (('set . label)
	   	(if (hash-table-exists? ht label)
		    (explode (hash-table-ref ht label))
		    expr))
	  (else (error "Unrecognized expression type" expr))))

  (define (combine-op-not expr)
    (display "-- ")
    (display expr)
    (newline)
    (match expr

	  (('isect . rest) (let ((a (car rest))
			         (b (cadr rest)))
			  (explode `(union ,(cons 'not a) ,(cons 'not b)))))

	  (('union . rest) (let ((a (car rest))
			         (b (cadr rest)))
			  (explode `(isect ,(cons 'not a) ,(cons 'not b)))))
	 
	  (('seq . rest) (let ((a (car rest))
			       (b (cadr rest)))
			  (explode `(union (seq ,(cons 'not a) ,b)
				      	   (union (seq ,(cons 'not a) ,(cons 'not b))
	  				     	  (seq ,a ,(cons 'not b)))))))
	  (('not . rest) rest)
	  
	  (else (cons 'not expr))
    ))

  (define (combine-op-single expr op)
    (match expr
	   (('union . exprs) (list 'union
				   (combine-op-single (car exprs) op)
				   (combine-op-single (cadr exprs) op)))
	    (else (cons op expr))))



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
  (generate-combinations (flatten-union (explode-expr-t rule ht)) 1))

(define (explode-acyclic-rule rule ht d)
  (generate-combinations (flatten-union (explode-expr-t rule ht)) d))

(define (generate-combinations edges n)
  (generate-combinations-h edges n))

(define (generate-combinations-h edges n)
  (define (helper current-list n)
    (if (zero? n)
	(list current-list)
	(apply append (map (lambda (edge)
	     (helper (list 'seq edge current-list) (- n 1)))
	     edges))))
  (apply append (map (lambda (edge) (helper edge (- n 1))) edges)))

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
  (print-stmt empty #f)
  (newline))

(define (print-acyclic acyclic)
  (display "acyclic ")
  (print-stmt acyclic #f)
  (newline))

(define (infix)
  (list 'union 'sadd 'seq 'isect 'diff 'cart))

(define (parentheses out in)
 ; (display out)
 ; (display "-")
 ; (display in)
 ; (newline)
  (match out
	 ('self #f)
	 ('set #f)
	 ('rel #f)
	 ('inv (member in (infix)))
	 ('not (member in (infix)))
	 ('zone (member in (infix)))
	 ('kstar (member in (infix)))
	 ('aone (member in (infix)))
	 ('union #t)
	 ('sadd (member in (list 'union)))
	 ('seq (member in (list 'union 'sadd)))
	 ('isect (member in (list 'union 'sadd 'seq)))
	 ('diff (member in (list 'union 'sadd 'seq 'isect)))
	 ('cart (member in (list 'union 'sadd 'seq 'isect 'diff)))))

(define (print-stmt stmt br)
  (if br (display "(")) 

  (match stmt
	(('seq . rest) (print-stmt (car rest) (parentheses 'seq (caar rest))) (display ";") (print-stmt (cadr rest) (parentheses 'seq (caadr rest))))
        (('isect . rest) 
		(print-stmt (car rest) (parentheses 'isect (caar rest))) (display "&") (print-stmt (cadr rest) (parentheses 'isect (caadr rest))))

        (('self . rest) (display "[") (print-stmt (car rest) (parentheses 'self (caar rest))) (display "]"))

        (('rel . rest) (display (match rest 
				       ("rfx" "rf")
				       (else rest))))
	(('set . rest) (display rest))
	(('inv . rest) (print-stmt rest (parentheses 'inv (car rest))) (display "^-1"))
	(('not . rest) (display "~") (print-stmt rest (parentheses 'not (car rest))))
	(else (display stmt)))
  (if br (display ")")))
   
(define (main args)
  (die-unless (= (length args) 2) "wrong arguments" usage)

  (let* ((fn (car args))
         (len (car (cdr args)))
         (len (string->number len))
         (port (open-input-file fn)))
    (print "# model file: " fn)
    (print "# cycle len: " len)

    (let ((tokens (tokenize-cat fn)))
      ;(display tokens)
      (let* ((model1 (parse-cat tokens))
             (model2 (include-file model1 "models/kittens.cat"))
             (model (include-files model2)))

	(let* ((ht (get-hash-table model))
               (empty-rules (get-empties (caddr model)))
	       (acyclic-rules (get-accs (caddr model)))
	       (empties (apply append (map (lambda (e) (explode-empty-rule e ht)) empty-rules)))
	       (acyclics (apply append (map (lambda (a) (explode-acyclic-rule a ht len)) acyclic-rules)))
               )  	  
          (for-each print-empty empties)
	  (newline)
	  (for-each print acyclics)
	  (newline)
	  (newline)
	  (for-each print-acyclic acyclics)
	  ))))
  0)

(start-command main)

(define test-expr 
    '(seq (isect (union (rel a1) (seq (rel b1) (rel b2))) (union (rel c1) (rel c2)))
	          (seq (isect (union (rel d1) (seq (rel e1) (rel e2))) (union (rel f1) (rel f2)))
		                    (union (rel g1) (isect (rel h1) (rel h2))))))


