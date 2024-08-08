#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (kittens command))

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

(define (generate rels)
  (let* ((rels (map string->symbol rels))
         (nedges (length rels))
         (nnums (seq nedges))
         (event->symbol (string/n->symbol "ev"))
         (edge->symbol (string/n->symbol "ed")))
    (append

     ; event declarations
     (map (lambda (e) `(declare-const ,e Event))
          (map event->symbol nnums))

     ; edge declarations
     (map (lambda (e) `(declare-const ,e Edge))
          (map edge->symbol nnums))

     ; help tids and addresses to look reasonable
     (map (lambda (e) `(assert (and (>= (tid ,e) 0)
                                    (< (tid ,e) ,nedges)
                                    (>= (addr ,e) 100)
                                    (< (addr ,e) ,(+ 100 nedges)))))
          (map event->symbol nnums))

     ; enforce event ids
     (map (lambda (e id)
            `(assert (= (eid ,e) ,id)))
          (map event->symbol nnums)
          nnums)

     ; assertions to force smt to find a solution
     (let ((equalis (map (lambda (edge) `(= e ,edge))
                         (map edge->symbol nnums))))
       `((assert (forall ((e Edge))
                         (= (inSet e)
                            (or ,@equalis))))))

     ; assert relations
     (map (lambda (rel i)
            (let ((edge (edge->symbol i))
                  (ev/i (event->symbol i))
                  (ev/j (event->symbol (modulo (+ 1 i) nedges))))
              `(assert (= ,edge (mk-edge ,ev/i ,ev/j ,rel)))))
          rels
          nnums)


     )))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let ((edges (generate args)))
    (print-boilerplate)
    (for-each (lambda (e)
                (display e)
                (newline))
              edges)
    (print-epilogue))

  0)

(main (command-args))

