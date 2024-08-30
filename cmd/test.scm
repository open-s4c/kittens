#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (kittens cat)
	(kittens utils)
        (kittens command)
	(kittens generator)
	(kittens match)
	(srfi 69)
	(srfi 95) ; sort
	(srfi 1) ; filter
        (srfi 130))

(define maxi-threads-flag #t)

(define maxi-addr-flag #t)

(define edges-count (make-hash-table))

(define (usage)
  (print "grill <edge> ..."))

(define (comment str)
  (list (string-append "; " str)))

(define (if-comment cd str)
  (if cd (comment str) '()))

(define (but-last xs) (reverse (cdr (reverse xs))))

(define (print-boilerplate)
  (with-input-from-file
   "boilerplate.smt2"
   (lambda ()
     (let loop ((line (read-line)))
       (unless (eof-object? line)
         (write-string line)
         (newline)
         (loop (read-line)))))))

(define (print-epilogue name)
  (newline)
  (print (string-append "(assert (= rels \"" name "\"))"))
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "; ask SMT solver for an answer")
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "(check-sat)")
  (print "(get-model)"))

(define (event->symbol event)
    (string->symbol
     (string-append "ev" (number->string event))))
 
(define (edge->name el er)
    (string->symbol
     (let ((name-base (string-append "ed" (number->string el) "-" (number->string er))))
       (if (hash-table-exists? edges-count name-base)
	 (hash-table-set! edges-count name-base (+ (hash-table-ref edges-count name-base) 1))
	 (hash-table-set! edges-count name-base 1)
       )
       (string-append name-base "-" (number->string (hash-table-ref edges-count name-base)))
    )))
 
(define (find-indices lst target)
  (let loop ((lst lst) (index 0) (indices '()))
    (cond
      ((null? lst) (reverse indices))
      ((equal? (car lst) target)
       (loop (cdr lst) (+ index 1) (cons index indices)))
      (else (loop (cdr lst) (+ index 1) indices)))))

(define (flatten lst)
  (cond
    ((null? lst) '())
    ((not (pair? lst)) lst)
    ((not (pair? (car lst)))
     (cons (car lst) (flatten (cdr lst))))
    (else
     (append (flatten (car lst)) (flatten (cdr lst))))))

(define-record-type
   edge-record
   (edge src trg type name)
   edge?
   (src edge-src)
   (trg edge-trg)
   (type edge-type)
   (name edge-name))

(define (parse-expr str)
   (let ((str-gen (str-generator str)))
    (let ((token-gen (token-generator (parse-or-die lexer str-gen))))
      (parse-or-die expr-parser token-gen))))

(define (make-edges el er expr)
  (match expr
    (('rel . "fr") (list
        (edge (floor (/ (+ el er) 2)) el "rf" (edge->name (floor (/ (+ el er) 2)) el))
        (edge (floor (/ (+ el er) 2)) er "co" (edge->name (floor (/ (+ el er) 2)) er))))
    (('rel . rel)
        (list (edge el er rel (edge->name el er))))
    (('seq . rest)
        (list
                 (make-edges el (floor (/ (+ el er) 2)) (car rest))
                 (make-edges (floor (/ (+ el er) 2)) er (cadr rest))))
    (('isect . rest)

            (list (make-edges el er (car rest))
                 (make-edges el er (cadr rest))))
    (('inv . rest)
        (make-edges er el rest))  ; just swap er and el
    (('self . ('set . rel))
         (list (edge el er rel (edge->name el er))))
    (else "hjuj")))

(define (generate-constraints edges)
  (let* ((events (sort (unique (flatten (map (lambda (edge) (list (edge-trg edge) (edge-src edge))) edges)))))
         (event-names (map event->symbol events))
	 (edge-names (map edge-name edges))
	)
    (append
     
     '(newline)
     (comment "event declarations")
     
     (map (lambda (e) `(declare-const ,e Event))
          event-names)

     '(newline)
     (comment "edge declarations")
     (map (lambda (e) `(declare-const ,e Edge))
          edge-names)

     '(newline)
     (comment "uid is distinct for all events")
     `((assert (distinct ,@(map (lambda (e)
                                  `(uid , e))
                                event-names))))
     '(newline)
     (comment "assertions to stop smt from creating edges")
     (comment "without inSet the solver will create edges to make the latter forall assertions fail")
     (let* ((equalis (map (lambda (edge) `(= e ,edge))
                          edge-names)))
       `((assert (forall ((e Edge))
                         (= (inEdgeSet e)
                            (or ,@equalis))))))

     '(newline)
     (comment "assertions to stop smt from creating events")
     (let* ((equalis (map (lambda (event) `(= e ,event))
                          event-names)))

       `((assert (forall ((e Event))
                         (= (inEventSet e)
                            (or ,@equalis))))))
     
     '(newline)
     (comment "assert relations between events in the graph")
     (map (lambda (edge)
            (let ((name (edge-name edge))
                  (src (event->symbol (edge-src edge)))
                  (trg (event->symbol (edge-trg edge)))
		  (type (string->symbol (edge-type edge))))
              `(assert (= ,name (mk-edge ,src ,trg ,type)))))
          edges)

     '(newline)
     (comment "reads abd RNW have to read from an rf edge or from an init event")

     (map (lambda (ev) 
	   `(assert
                    (=> (and (or (= (op ,ev) (as read Operation))

                                 (= (op ,ev) (as read-modify-write Operation)))
                             (not (exists ((e1 Edge))
                                     (and (inEdgeSet e1)
                                          (= (eid (trg e1)) (eid ,ev))
                                          (= (rel e1) (as rf Relation))
                                          ))))
                        (= (val-r ,ev) 0)))
	) event-names)
     )))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let* ((expr (car args))
	(expr (parse-expr expr))
	(edges (flatten (make-edges 0 1000 expr)))
        (constraints (generate-constraints edges)))

    (print-boilerplate)
    (for-each (lambda (e)
                (if (not (eq? e 'newline)) (display e))

                (newline))
              constraints)
    (print-epilogue (car args))
    )

  0)

(start-command main)

