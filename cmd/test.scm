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
       (string-append name-base (number->string (hash-table-ref edges-count name-base)))
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
         (edge el er rel (edge->name el er)))
    (else "hjuj")))

(define (generate-constraints edges)
  (let* ((events (sort (unique (flatten (map (lambda (edge) (list (edge-trg edge) (edge-src edge))) edges)))))
         (event-names (map event->symbol events))
	 (edge-names (map edge-name edges))
	)
    (append
     ;(display event-names)
     ;(display events)

     ;(if-comment maxi-threads "the addresses of reads and writes of either side of a po are different")
     ;(if maxi-threads   
     ;    
     ;        '((assert (forall ((e Edge))
     ;                          (=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
     ;                              (and (< (porder (src e)) (porder (trg e)))
     ;                                   (= (tid (src e)) (tid (trg e)))
     ;                                   (not (= (addr (src e)) (addr (trg e)))))))))
     ;        '((assert (forall ((e Edge))
     ;                          (=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
     ;                              (and (< (porder (src e)) (porder (trg e)))
     ;                                   (= (tid (src e)) (tid (trg e))))))))
     ;)
     '(newline)
     (comment "event declarations")
     
     (map (lambda (e) `(declare-const ,e Event))
          event-names)

     '(newline)
     (comment "edge declarations")
     (map (lambda (e) `(declare-const ,e Edge))
          edge-names)

     
     '(newline)
     (comment "force event fields to look reasonable")
     (map (lambda (e) `(assert (and (>= (tid ,e) 0)
                                    (< (tid ,e) 50)
                                    (>= (corder ,e) 300)
                                    (< (corder ,e) 350)
                                    (>= (porder ,e) 200)
                                    (< (porder ,e) 250)
                                    (>= (addr ,e) 100)
                                    (< (addr ,e) 150))))
         event-names)

     ;'(newline)
     ;(comment "main write events on cycle can only write large values")
     ;(comment "only fr events (INIT events) can write smaller values - 0")
     ;(map (lambda (e)
     ;       `(assert (=>
     ;                 (= (op ,e) (as write Operation))
     ;                 (and
     ;                  (>= (val-w ,e) 10)
     ;                  (< (val-w ,e) ,(+ 10 (+ (length fr-rels) nedges)))))))
     ;     (map event->symbol nnums))

     ;'(newline)
     ;(if-comment fr "fr events can write a 0 for initialisation") 
     ;(map (lambda (e)
     ;       `(assert (and
     ;                 (>= (val-w ,e) 0)
     ;                 (< (val-w ,e) ,(+ 10 (+ (length fr-rels) nedges))))))
     ;     (map event-fr->symbol fr-rels))

     ;'(newline)
     ;(if-comment fr "keep eid of events withing a reasonable (small) range for readability") 
     ;(map (lambda (e)
     ;       `(assert (and (< (eid ,e) ,(+ (length fr-rels) nedges))
     ;                     (>= (eid ,e) 0))))
     ;     (map event-fr->symbol fr-rels)
     ;     )

     ;'(newline)
     ;(if-comment fr "uid is unique/distinct for all events")
     ;(if (> (length fr-rels) 0)
     ;    `((assert (distinct ,@(map (lambda (e)
     ;                                 `(uid ,(event-fr->symbol e)))
     ;                               fr-rels))))
     ;    )

     '(newline)
     (comment "uid is distinct for all events")
     `((assert (distinct ,@(map (lambda (e)
                                  `(uid , e))
                                event-names))))
     ;'(newline)
     ;(if-comment fr "in order to distinguish between main events and fr events, fr events have uid < 0")
     ;(map (lambda (e)
     ;       `(assert (< (uid ,(event-fr->symbol e)) 0)))
     ;     fr-rels)

     ;'(newline)
     ;(comment "in order to distinguish between main events and fr events, main events have uid > 0")
     ;(map (lambda (e)
     ;       `(assert (> (uid ,(event->symbol e)) 0)))
     ;     nnums)

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

    ; those following edges are assert (edge src is this and edge trg is that)

     '(newline)
     (comment "assert relations between events in the main cycle")
     (map (lambda (edge)
            (let ((name (edge-name edge))
                  (src (event->symbol (edge-src edge)))
                  (trg (event->symbol (edge-trg edge)))
		  (type (string->symbol (edge-type edge))))
              `(assert (= ,name (mk-edge ,src ,trg ,type)))))
          edges)

     

     ;'(newline)
     ;(if-comment fr "each fr edge gets a new event. if there is no other event with the same eid, this fr event is an INIT event. this merges rf -> x <- rf edges")
     ;(map (lambda (rel)
     ;       `(assert (=>
     ;                 (not (exists ((e Event))
     ;                              (and
     ;                               (not (= (uid e) (uid ,(event-fr->symbol rel))))
     ;                               (inEventSet e)
     ;                               (= (eid e) (eid ,(event-fr->symbol rel))))))
     ;                 (= 0 (val-w ,(event-fr->symbol rel)))))
     ;       ) fr-rels)

     ;'(newline)
     ;(if-comment maxi-addr "if there are exactly k po-s and 1 non po edge, then all events are on the same thread due to")
     ;(if-comment maxi-addr "explicit po from first to second-to-last event")
     ;(if-comment maxi-addr "if that's not the case, separate all consecutive events that are not connected by a po")
     ;(if-comment maxi-addr "in order to maximise the amount of threads")  

     ;(if maxi-addr	      
     ;        (map (lambda (ev1 ev2)
     ;               `(assert (= (and
     ;                            (not (exists ((ed Edge))
     ;                                         (and
     ;                                          (inEdgeSet ed)
     ;                                          (or
     ;                                           (and (= (src ed) ,ev1)
     ;                                                (= (trg ed) ,ev2)
     ;                                                (= (rel ed) (as po Relation)))
     ;                                           (and (= (src ed) ,ev2)
     ;                                                (= (trg ed) ,ev1)
     ;                                                (= (rel ed) (as po Relation)))))))
     ;                            (not (= (eid ,ev2) (eid ,ev1))))
     ;                           (not (= (tid ,ev2) (tid ,ev1)))
     ;                           ))) (map event->symbol nnums) (map  event->symbol (map (lambda (x) (modulo (+ x 1) nedges)) nnums)))

     ;        '()
     ;    )
     ;;(display (apply string-append (apply append (list (map (lambda (rel) (string-append rel "+")) (but-last rels-input))
     ;;(list (car (reverse rels-input)))))))
     ;`((assert (= rels ,(apply string-append (apply append (list (map (lambda (rel) (string-append rel "+")) (but-last rels-input))
     ;                                                            (list (car (reverse rels-input)))))))))

     )))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let* ((expr (car args))
	(expr (parse-expr expr))
	(edges (flatten (make-edges 0 100 expr)))
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


