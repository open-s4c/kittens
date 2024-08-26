#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (kittens utils)
        (kittens command)
        (srfi 1) ; filter
        (srfi 130))

(define maxi-threads-flag #t)

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

(define (print-epilogue)
  (newline)
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "; ask SMT solver for an answer")
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "(check-sat)")
  (print "(get-model)"))

(define (string/n->symbol str)
  (lambda (n)
    (string->symbol
     (string-append str (number->string n)))))

(define (rotate-list lst)
  (if (null? lst)
    lst
    (append (cdr lst) (list (car lst)))))

(define (count-po-chains lst)
  (define (helper lst in-chain count)
    (cond
      ((null? lst) count)
      ((eq? (car lst) 'po)
       (if in-chain
           (helper (cdr lst) #t count)
           (helper (cdr lst) #t (+ count 1))))
      (else
       (helper (cdr lst) #f count))))
  (helper lst #f 0))

(define (single-po-chain lst)
  (define (helper lst n)
    (if (eq? n 0)
      #f
      (if (= (count-po-chains lst) 1)
        #t
	(helper (rotate-list lst) (- n 1))
      )
    )
  )
  (helper lst (length lst))
)

(define (find-indices lst target)
  (let loop ((lst lst) (index 0) (indices '()))
    (cond
      ((null? lst) (reverse indices))
      ((equal? (car lst) target)
       (loop (cdr lst) (+ index 1) (cons index indices)))
      (else (loop (cdr lst) (+ index 1) indices)))))


(define (convert-rels rels)
  (map (lambda (rel) (if (and (string-prefix? "[" rel) (string-suffix? "]" rel))
                         (string->symbol (substring rel 1 (- (string-length rel) 1)))
                         (string->symbol rel)))
       rels))

(define (generate rels-input)
  (let* ((brcks (filter (lambda (rel) (string-prefix? "[" rel)) rels-input))
	 (no-brcks (filter (lambda (rel) (not (string-prefix? "[" rel))) rels-input))
         (rels (convert-rels rels-input))
         (rels (map (lambda (rel) (if (equal? rel 'rfx) 'rf rel)) rels))

         (nedges (length rels))
         (nnums (seq nedges))
         (fr-rels (find-indices rels 'fr))
         (fr (> (length fr-rels) 0))
         (nfredges (length fr-rels))

         (event->symbol (string/n->symbol "ev"))
         (event-fr->symbol (string/n->symbol "evfr"))
         (edge->symbol (string/n->symbol "ed"))
         (edge-fr->symbol (string/n->symbol "edfr"))
	 (maxi-threads (and maxi-threads-flag (not (single-po-chain no-brcks)))))
    (append
     ;(display (single-po-chain? rels))
     (display maxi-threads)

     (if-comment maxi-threads "the addresses of reads and writes of either side of a po are different")
     (if maxi-threads   
         
             '((assert (forall ((e Edge))
                               (=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
                                   (and (< (porder (src e)) (porder (trg e)))
                                        (= (tid (src e)) (tid (trg e)))
                                        (not (= (addr (src e)) (addr (trg e)))))))))
             '((assert (forall ((e Edge))
                               (=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
                                   (and (< (porder (src e)) (porder (trg e)))
                                        (= (tid (src e)) (tid (trg e))))))))
        )
    '(newline)
     (comment "event declarations")

     (map (lambda (e) `(declare-const ,e Event))
          (map event->symbol nnums))

     '(newline)
     (if-comment fr "event from fr declarations")
     (map (lambda (e) `(declare-const ,e Event))
          (map event-fr->symbol fr-rels))

     '(newline)
     (comment "edge declarations")
     (map (lambda (e) `(declare-const ,e Edge))
          (map edge->symbol nnums))

     '(newline)
     (if-comment fr "edge declarations from fr decomp")
     (map (lambda (e) `(declare-const ,e Edge))
          (map edge-fr->symbol fr-rels))

     (map (lambda (e) `(declare-const ,e Edge))
          (map edge-fr->symbol
               (map (lambda (j) (modulo (+ j 1) nedges)) fr-rels)))

     '(newline)
     (comment "force event fields to look reasonable")
     (map (lambda (e) `(assert (and (>= (tid ,e) 0)
                                    (< (tid ,e) ,(+ (length fr-rels) nedges))
                                    (>= (corder ,e) 300)
                                    (< (corder ,e) ,(+ 300 (+ (length fr-rels) nedges)))
                                    (>= (porder ,e) 200)
                                    (< (porder ,e) ,(+ 200 (+ (length fr-rels) nedges)))
                                    (>= (addr ,e) 100)
                                    (< (addr ,e) ,(+ 100 (+ (length fr-rels) nedges))))))
          (append
           (map event->symbol nnums)
           (map event-fr->symbol fr-rels)))

     '(newline)
     (comment "main write events on cycle can only write large values")
     (comment "only fr events (INIT events) can write smaller values - 0")
     (map (lambda (e)
            `(assert (=>
                      (= (op ,e) (as write Operation))
                      (and
                       (>= (val-w ,e) 10)
                       (< (val-w ,e) ,(+ 10 (+ (length fr-rels) nedges)))))))
          (map event->symbol nnums))

     '(newline)
     (if-comment fr "fr events can write a 0 for initialisation") 
     (map (lambda (e)
            `(assert (and
                      (>= (val-w ,e) 0)
                      (< (val-w ,e) ,(+ 10 (+ (length fr-rels) nedges))))))
          (map event-fr->symbol fr-rels))

     '(newline)
     (if-comment fr "keep eid of events withing a reasonable (small) range for readability") 
     (map (lambda (e)
            `(assert (and (< (eid ,e) ,(+ (length fr-rels) nedges))
                          (>= (eid ,e) 0))))
          (map event-fr->symbol fr-rels)
          )

     '(newline)
     (if-comment fr "uid is unique/distinct for all events")
     (if (> (length fr-rels) 0)
         `((assert (distinct ,@(map (lambda (e)
                                      `(uid ,(event-fr->symbol e)))
                                    fr-rels))))
         )

     '(newline)
     (comment "uid is unique/distinct for all events")
     `((assert (distinct ,@(map (lambda (e)
                                  `(uid ,(event->symbol e)))
                                nnums))))
     '(newline)
     (if-comment fr "in order to distinguish between main events and fr events, fr events have uid < 0")
     (map (lambda (e)
            `(assert (< (uid ,(event-fr->symbol e)) 0)))
          fr-rels)

     '(newline)
     (comment "in order to distinguish between main events and fr events, main events have uid > 0")
     (map (lambda (e)
            `(assert (> (uid ,(event->symbol e)) 0)))
          nnums)

     '(newline)
     (comment "assertions to stop smt from creating edges")
     (comment "without inSet the solver will create edges to make the latter forall assertions fail")
     (let* ((equalis (map (lambda (edge) `(= e ,edge))
                          (map edge->symbol nnums)))

            (equalis-fr (map (lambda (edge) `(= e ,edge))
                             (map edge-fr->symbol (apply append (map (lambda (x) `(,x ,(modulo (+ x 1) nedges))) fr-rels))))))
       `((assert (forall ((e Edge))
                         (= (inEdgeSet e)
                            (or ,@equalis ,@equalis-fr))))))

     '(newline)
     (comment "assertions to stop smt from creating events")
     (let* ((equalis (map (lambda (event) `(= e ,event))
                          (map event->symbol nnums)))

            (equalis-fr (map (lambda (event) `(= e ,event))
                             (map event-fr->symbol fr-rels))))
       `((assert (forall ((e Event))
                         (= (inEventSet e)
                            (or ,@equalis ,@equalis-fr))))))

     '(newline)
     (comment "assert relations between events in the main cycle")
     (map (lambda (rel i)
            (let ((edge (edge->symbol i))
                  (ev/i (event->symbol i))
                  (ev/j (event->symbol (modulo (+ 1 i) nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          rels
          nnums)

     '(newline)
     (if-comment fr "each fr edge gets a new co edge")

     (map (lambda (rel i)
            (let ((edge (edge-fr->symbol (modulo (+ 1 i) nedges)))
                  (ev/i (event-fr->symbol i))
                  (ev/j (event->symbol (modulo (+ 1 i) nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          (map (lambda (x) 'co) fr-rels)
          fr-rels)

     '(newline)
     (if-comment fr "each fr edge gets a new rf edge")
     (map (lambda (rel i)
            (let ((edge (edge-fr->symbol i))
                  (ev/i (event-fr->symbol i))
                  (ev/j (event->symbol (modulo i nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          (map (lambda (x) 'rf) fr-rels)
          fr-rels)


     '(newline)
     (if-comment fr "each fr edge gets a new event. if there is no other event with the same eid, this fr event is an INIT event. this merges rf -> x <- rf edges")
     (map (lambda (rel)
            `(assert (=>
                      (not (exists ((e Event))
                                   (and
                                    (not (= (uid e) (uid ,(event-fr->symbol rel))))
                                    (inEventSet e)
                                    (= (eid e) (eid ,(event-fr->symbol rel))))))
                      (= 0 (val-w ,(event-fr->symbol rel)))))
            ) fr-rels)

     '(newline)
     (if-comment maxi-threads "if there are exactly k po-s and 1 non po edge, then all events are on the same thread due to")
     (if-comment maxi-threads "explicit po from first to second-to-last event")
     (if-comment maxi-threads "if that's not the case, separate all consecutive events that are not connected by a po")
     (if-comment maxi-threads "in order to maximise the amount of threads")  

     (if maxi-threads 	      
             (map (lambda (ev1 ev2)
                    `(assert (= (and
                                 (not (exists ((ed Edge))
                                              (and
                                               (inEdgeSet ed)
                                               (or
                                                (and (= (src ed) ,ev1)
                                                     (= (trg ed) ,ev2)
                                                     (= (rel ed) (as po Relation)))
                                                (and (= (src ed) ,ev2)
                                                     (= (trg ed) ,ev1)
                                                     (= (rel ed) (as po Relation)))))))
                                 (not (= (eid ,ev2) (eid ,ev1))))
                                (not (= (tid ,ev2) (tid ,ev1)))
                                ))) (map event->symbol nnums) (map  event->symbol (map (lambda (x) (modulo (+ x 1) nedges)) nnums)))

             '()
         )
     ;(display (apply string-append (apply append (list (map (lambda (rel) (string-append rel "+")) (but-last rels-input))
     ;(list (car (reverse rels-input)))))))
     `((assert (= rels ,(apply string-append (apply append (list (map (lambda (rel) (string-append rel "+")) (but-last rels-input))
                                                                 (list (car (reverse rels-input)))))))))

     )))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let ((edges (generate args)))
    (print-boilerplate)
    (for-each (lambda (e)
                (if (not (eq? e 'newline)) (display e))

                (newline))
              edges)
    (print-epilogue)
    )

  0)

(start-command main)


