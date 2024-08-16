#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (kittens utils)
        (kittens command))

(define (usage)
  (print "grill <edge> ..."))

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

(define (find-indices lst target)
  (let loop ((lst lst) (index 0) (indices '()))
    (cond
      ((null? lst) (reverse indices))
      ((equal? (car lst) target)
       (loop (cdr lst) (+ index 1) (cons index indices)))
      (else (loop (cdr lst) (+ index 1) indices)))))

(define (generate rels)
  (let* ((rels (map string->symbol rels))
         (nedges (length rels))
         (nnums (seq nedges))
         (fr-rels (find-indices rels 'fr))
         (nfredges (length fr-rels))

         (event->symbol (string/n->symbol "ev"))
         (event-fr->symbol (string/n->symbol "evfr"))
         (edge->symbol (string/n->symbol "ed"))
         (edge-fr->symbol (string/n->symbol "edfr")))

    (append
     ; Constraints for po
     (if (eq? 1 (length (find-indices rels 'po)))
         '((assert (forall ((e Edge))
                           (=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
                               (and (< (porder (src e)) (porder (trg e)))
                                    (= (tid (src e)) (tid (trg e))))))))
         '((assert (forall ((e Edge))
                           (=> (and (= (rel e) (as po Relation)) (inEdgeSet e))
                               (and (< (porder (src e)) (porder (trg e)))
                                    (not (= (addr (src e)) (addr (trg e))))
                                    (= (tid (src e)) (tid (trg e))))))))
         )
     ; event declarations
     (map (lambda (e) `(declare-const ,e Event))
          (map event->symbol nnums))

     ; event from fr declarations
     (map (lambda (e) `(declare-const ,e Event))
          (map event-fr->symbol fr-rels))

     ; edge declarations
     (map (lambda (e) `(declare-const ,e Edge))
          (map edge->symbol nnums))

     ; edge declarations from fr decomp
     (map (lambda (e) `(declare-const ,e Edge))
          (map edge-fr->symbol fr-rels))

     (map (lambda (e) `(declare-const ,e Edge))
          (map edge-fr->symbol
               (map (lambda (j) (modulo (+ j 1) nedges)) fr-rels)))

     ; force event fields to look reasonable
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

     (map (lambda (e)
            `(assert (=>
                      (= (op ,e) (as write Operation))
                      (and
                       (>= (val ,e) 10)
                       (< (val ,e) ,(+ 10 (+ (length fr-rels) nedges)))))))
          (map event->symbol nnums))

     (map (lambda (e)
            `(assert (and
                      (>= (val ,e) 0)
                      (< (val ,e) ,(+ 10 (+ (length fr-rels) nedges))))))
          (map event-fr->symbol fr-rels))

     ; enforce event ids
     (map (lambda (e id)
            `(assert (= (eid ,e) ,id)))
          (map event->symbol nnums)
          nnums)

     (map (lambda (e)
            `(assert (and (< (eid ,e) ,(+ (length fr-rels) nedges))
                          (>= (eid ,e) 0))))
          (map event-fr->symbol fr-rels)
          )

     `((assert (distinct ,@(map (lambda (e)
                                  `(uid ,(event-fr->symbol e)))
                                fr-rels))))
     `((assert (distinct ,@(map (lambda (e)
                                  `(uid ,(event->symbol e)))
                                nnums))))

     (map (lambda (e)
            `(assert (< (uid ,(event-fr->symbol e)) 0)))
          fr-rels)

     (map (lambda (e)
            `(assert (> (uid ,(event->symbol e)) 0)))
          nnums)

     ; assertions to stop smt from creating edges
     ; without inSet the solver will create edges to make the latter forall assertions fail
     (let* ((equalis (map (lambda (edge) `(= e ,edge))
                          (map edge->symbol nnums)))

            (equalis-fr (map (lambda (edge) `(= e ,edge))
                             (map edge-fr->symbol (apply append (map (lambda (x) `(,x ,(modulo (+ x 1) nedges))) fr-rels))))))
       `((assert (forall ((e Edge))
                         (= (inEdgeSet e)
                            (or ,@equalis ,@equalis-fr))))))

     (let* ((equalis (map (lambda (event) `(= e ,event))
                          (map event->symbol nnums)))

            (equalis-fr (map (lambda (event) `(= e ,event))
                             (map event-fr->symbol fr-rels))))
       `((assert (forall ((e Event))
                         (= (inEventSet e)
                            (or ,@equalis ,@equalis-fr))))))

     ; assert relations
     (map (lambda (rel i)
            (let ((edge (edge->symbol i))
                  (ev/i (event->symbol i))
                  (ev/j (event->symbol (modulo (+ 1 i) nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          rels
          nnums)
     (map (lambda (rel i)
            (let ((edge (edge-fr->symbol (modulo (+ 1 i) nedges)))
                  (ev/i (event-fr->symbol i))
                  (ev/j (event->symbol (modulo (+ 1 i) nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          (map (lambda (x) 'co) fr-rels)
          fr-rels)

     (map (lambda (rel i)
            (let ((edge (edge-fr->symbol i))
                  (ev/i (event-fr->symbol i))
                  (ev/j (event->symbol (modulo i nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          (map (lambda (x) 'rf) fr-rels)
          fr-rels)

     (map (lambda (rel)
            `(assert (=>
                      (not (exists ((e Event))
                                   (and
                                    (not (= (uid e) (uid ,(event-fr->symbol rel))))
                                    (inEventSet e)
                                    (= (eid e) (eid ,(event-fr->symbol rel))))))
                      (= 0 (val ,(event-fr->symbol rel)))))
            ) fr-rels)
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

     )))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let ((edges (generate args)))
    (print-boilerplate)
    (for-each (lambda (e)
                (display e)
                (newline))
              edges)
    (print-epilogue)
    )

  0)

(main (command-args))


