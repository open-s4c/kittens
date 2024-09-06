#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (scheme cxr)
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

(define counter 1)

(define (get-counter)
  (let ((current counter))
    (set! counter (+ counter 1))
    current))

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

(define (all-pairs lst)
  (apply append
    (map (lambda (x)
      (map (lambda (y)
        (list x y))
      lst))
    lst)))


(define (find-group groups x)
    (find (lambda (group) (member x group)) groups))

(define (remove-group groups g)
    (filter (lambda (group) (not (equal? group g))) groups))

(define (simple-uf groups pairs)
  (if (null? pairs)
      groups
      (let* ((pair (car pairs))
	     (a (find-group groups (car pair)))
	     (b (find-group groups (cadr pair)))
	     (new-groups-1 (remove-group groups a))
	     (new-groups-2 (remove-group new-groups-1 b))
	     (new-groups-3 (append new-groups-2 (list (unique (append a b))))))
	
	(simple-uf new-groups-3 (cdr pairs)))))

  (define (helper groups-h rf-pairs-h)
    (if (null? rf-pairs-h)
	groups-h
	(let* ((pair (car rf-pairs-h))
	       (trg1 (caar pair))
	       (trg2 (cadar pair))
	       (src1 (caadr pair))
	       (src2 (cadadr pair))
	       (c (find-group groups-h trg1))
	       (d (find-group groups-h trg2))
	       (flag (eq? c d))
	       (a (find-group groups-h src1))
	       (b (find-group groups-h src2))

	       (new-groups-1 (if flag
			       (remove-group groups-h a)
			       groups-h))
	       
	       (new-groups-2 (if flag
			       (remove-group new-groups-1 b)
			       groups-h))
	       
	       (new-groups-3 (if flag
			       (append new-groups-2 (list (unique (append a b))))
			       groups-h)))
	  
	  (helper new-groups-3 (cdr rf-pairs-h)))))

(define (rf-uf groups rf-pairs)
  ;(display groups)
  ;(newline)
  ;(newline)

  (let* ((new-groups (helper groups rf-pairs)))
    (if (eq? (length new-groups) (length groups))
	groups
	(rf-uf new-groups rf-pairs))))

(define (get-eid-partition events edges is-acyclic)
  (let* ((same-eid-sets (get-same-eid-set edges))
	 (same-eid (if is-acyclic (append same-eid-sets (list (list 1 0))) same-eid-sets))
	 (groups (simple-uf (map list events) same-eid)))
    ;(display "groups after simple-uf:\n")
    ;(display groups)
    (let* ((rf-pairs (get-rf-pairs edges)) 
	   (groups (rf-uf groups rf-pairs)))
      ;(display "rf pairs:\n")
      ;(display rf-pairs)
      ;(newline)
      ;(display "groups after rf-uf:\n")
      ;(display groups)
      ;(newline)
      groups
      )))

(define (get-rf-pairs edges)
  (let* ((rf-edges (filter (lambda (edge) (equal? (edge-type edge) "rf")) edges))
	 (rf-edges (map (lambda (e) (list (edge-src e) (edge-trg e))) rf-edges))
	 (rf-pairs (all-pairs rf-edges))
	 (rf-pairs (map (lambda (pair) (list (list (cadar pair) (cadadr pair)) (list (caar pair) (caadr pair)))) rf-pairs)))
    rf-pairs 
  ))

(define (get-same-eid-set edges)
  (let* ((set-edges (filter (lambda (edge) (or (equal? (edge-type edge) "RMW") (equal? (edge-type edge) "R") (equal? (edge-type edge) "W"))) edges))
	 (pairs (map (lambda (edge) (list (edge-src edge) (edge-trg edge))) set-edges)))
    pairs))

(define (make-edges el er expr)
  (match expr
    (('rel . "fr") (let ((counter (get-counter))) (list
        (edge counter el "rf" (edge->name counter el))
        (edge counter er  "co" (edge->name counter er)))))
    (('rel . rel)
        (list (edge el er rel (edge->name el er))))
    (('seq . rest)
       (let ((counter (get-counter)))   
     (list
                 (make-edges el counter (car rest))
                 (make-edges counter er (cadr rest)))))
    (('isect . rest)

            (list (make-edges el er (car rest))
                  (make-edges el er (cadr rest))))
    (('inv . rest)
        (make-edges er el rest))  ; just swap er and el
    (('self . ('set . rel))
         (list (edge el er rel (edge->name el er))))
    (else "")))

(define (equality-assertion lst field)
  (let* ((first (car lst))
	 (rest (cdr lst))
	 (constraints (map (lambda (x) 
                             `(= (,field ,(string->symbol (string-append "ev" (number->string first))))
				 (,field ,(string->symbol (string-append "ev" (number->string x))))))
			   rest)))
         `((assert (and ,@constraints)))))


(define (distinct-assertion lst field)
  (let* ((constraints (map (lambda (x)
			     `(,field ,(string->symbol (string-append "ev" (number->string x)))))
			   lst)))
         `((assert (distinct ,@constraints)))))

(define (eid-constraints eid-partition field distinct)
  (let* ((multy (filter (lambda (lst) (> (length lst) 1)) eid-partition))
	 (cars (map car eid-partition)))
    (if distinct 
    	(append 
      	  (apply append (map (lambda (el) (equality-assertion el field)) multy))
      	  (distinct-assertion cars field))
    	(apply append (map (lambda (el) (equality-assertion el field)) multy)))
    )
  )

(define (generate-constraints events edges is-acyclic)
  (let* ((event-names (map event->symbol events))
	 (edge-names (map edge-name edges))
 	 (eid-partition (get-eid-partition events edges is-acyclic)))
     (apply append (list
     
     '(newline)
     (comment "event declarations")
     (map (lambda (e) `(declare-const ,e Event))
          event-names)

     (eid-constraints eid-partition 'eid #t)
     (eid-constraints eid-partition 'tid #f)
     (eid-constraints eid-partition 'porder #t)
     (eid-constraints eid-partition 'corder #t)
     (eid-constraints eid-partition 'addr #f)
     (eid-constraints eid-partition 'val-r #f)
     (eid-constraints eid-partition 'val-w #t)
     (eid-constraints eid-partition 'val-e #f)
     (eid-constraints eid-partition 'op #f)
     
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
                    (=> (and (or (= (op ,ev) (as read Operation)) (newline)

                                 (= (op ,ev) (as read-modify-write Operation))) (newline)
                             (not (exists ((e1 Edge)) (newline)
                                     (and (inEdgeSet e1) (newline)
                                          (= (eid (trg e1)) (eid ,ev)) (newline)
                                          (= (rel e1) (as rf Relation))
                                          )))) (newline)
                        (= (val-r ,ev) 0))) 
	) event-names)
     
    
     (map (lambda (e) (append 
     	`(assert (=> (and (= (rel ,e) (as RMW Relation)) (newline) 
			  (inEdgeSet ,e) 
		,@(apply append (map (lambda (e1)
			 `((newline) (not (and (= (rel ,e1) (as rf Relation)) (inEdgeSet ,e1) (newline) 
			   	 (or (and (= (eid (src ,e)) (eid (src ,e1)))  (newline)
			   		  (= (eid (trg ,e)) (eid (trg ,e1))))  (newline)
			   	     (and (= (eid (src ,e)) (eid (src ,e1))) (newline)
			   		  (= (eid (trg ,e)) (eid (trg ,e1))))))))  
			   ) edge-names)))
         (newline) (not (= (val-r (src ,e)) (val-w (trg ,e)))))) '(newline))
	) edge-names)

          ))))

(define (tabbb n)
  (if (eq? n 0)
    ""
    (apply string-append (list "    " (tabbb (- n 1)))) 
  ))

(define (print-constraints-h constraints n)
  (string-append 
    (apply string-append (map (lambda (e)
      (string-append
        (cond 
	    ((and (list? e) (eq? (car e) 'newline)) (string-append "\n" (tabbb n)))
	    ((list? e) (string-append "(" (print-constraints-h e (+ n 1)) ")"))
	    ((and (symbol? e) (eq? e 'newline)) (string-append "\n" (tabbb n)))
	    ((symbol? e) (symbol->string e))
	    ((number? e) (number->string e))
	    ((string? e) e)
	    (else "Not good")
      ) " ")) (but-last constraints)))
    (let ((e (car (reverse constraints))))
	(cond 
	    ((and (list? e) (eq? (car e) 'newline)) (string-append "\n" (tabbb n)))
	    ((list? e) (string-append "(" (print-constraints-h e (+ n 1)) ")"))
	    ((and (symbol? e) (eq? e 'newline)) (string-append "\n" (tabbb n)))
	    ((symbol? e) (symbol->string e))
	    ((number? e) (number->string e))
	    ((string? e) e)
	    (else "Not good")))))

(define (print-constraints constraint)
  (newline)
  (cond 
    ((eq? constraint 'newline) (display "\n"))
    ((string? constraint) (display constraint))
    (else (display (string-append "(" (print-constraints-h constraint 0) ")"))))
  )

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let* ((expr (cadr args))
	 (type (car args))
	 (is-acyclic (equal? "acyclic" type)))
  
  (let* ((expr (parse-expr expr))
	(edges (flatten (make-edges 0 (get-counter) expr)))
        (events (sort (unique (flatten (map (lambda (edge) (list (edge-trg edge) (edge-src edge))) edges)))))
	(constraints (generate-constraints events edges is-acyclic)))
    ;(newline)
    ;(display edges)
    ;(newline)
    (print-boilerplate)
    (for-each print-constraints constraints)  
    (print-epilogue (car args))
    ))

  0)

(start-command main)


