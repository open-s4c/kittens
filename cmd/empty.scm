#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (kittens cat)
        (kittens utils)
        (kittens match)
	(kittens generator)
        (kittens command)
        (srfi 1) ; filter
        (srfi 130))

(define (usage)
  (print "empty <expr>"))	
	
(define (flatten lst)
  (cond
    ((null? lst) '())
    ((not (pair? (car lst)))
     (cons (car lst) (flatten (cdr lst))))
    (else
     (append (flatten (car lst)) (flatten (cdr lst)))))) 

(define-record-type 
  constraint-record
  (constraint src trg type)
  constraint?
  (src constraint-src)
  (trg constraint-trg)
  (type constraint-type))

(define (parse-expr str)
  (let ((str-gen (str-generator str)))
    (let ((token-gen (token-generator (parse-or-die lexer str-gen))))
      (parse-or-die expr-parser token-gen))))

(define (make-constraints el er expr)
  (match expr 
    (('rel . "fr") (list 
	(constraint (floor (/ (+ el er) 2)) el "rf")
	(constraint (floor (/ (+ el er) 2)) er "co")))
    (('rel . rel) 
	(list (constraint el er rel)))
    (('seq . rest) 
	(list
		  (make-constraints el (floor (/ (+ el er) 2)) (car rest)) 
		  (make-constraints (floor (/ (+ el er) 2)) er (cadr rest))))
    (('isect . rest)
	
	    (list (make-constraints el er (car rest))
		  (make-constraints el er (cadr rest))))
    (('inv . rest)
	(make-constraints er el rest))  ; just swap er and el
    (('self . ('set . rel)) 
	(constraint el er rel))
    (else "hjuj")))

(define (main args)
  (die-unless (= 1 (length args)) "<expr>")

  (let* ((expr (car args))
         (expr (parse-expr expr))
	 (constraints (flatten (make-constraints 0 10 expr)))
	 (events (unique (flatten (map (lambda (constraint) (list (constraint-trg constraint) (constraint-src constraint))) constraints)))))
    (newline)
    (print expr)
    (newline)
    (print events)
    (newline)
    (print constraints)
    )

  0)

(start-command main)


