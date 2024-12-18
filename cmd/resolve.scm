#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (scheme cxr)
        (kittens cat)
        (kittens utils)
        (kittens command)
        (kittens generator)
        (kittens match)
        (kittens debug)
        (srfi 69)
        (srfi 95) ; sort
        (srfi 1) ; filter
        (srfi 130))

(define (usage)
  (print "grill <edge> ..."))

;; Expression parse
(define (parse-expr str)
  (let ((str-gen (str-generator str)))
    (let ((token-gen (token-generator (parse-or-die lexer str-gen))))
      (parse-or-die expr-parser token-gen))))

;; Definition of edge record
(define-record-type
  edge-record
  (edge src dst type name)
  edge?
  (src edge-src)
  (dst edge-dst)
  (type edge-type)
  (name edge-name))

;; Counter for naming events
(define counter 1)

;; Every time a new event is created we increment the counter
(define (get-counter)
  (let ((current counter))
    (set! counter (+ counter 1))
    current))

;; There might be multiple edges between the same pair of events
;; Thus we name edges ed-a-b-n
;; a is src ev
;; b is dst ev
;; n is number of occurances before this edge is created
(define edges-count (make-hash-table))

;; Helper to display comments in constraints file kittens.smt
(define (comment str)
  (string->symbol (string-append "; " str)))

;; Helper to display comments conditionally
(define (if-comment cd str)
  (if cd (comment str) '()))

;; Helper to get all elements of a list besides the last element
(define (but-last xs) (reverse (cdr (reverse xs))))

;; Printing boilerplate constraints
(define (print-boilerplate)
  (print "; PROLOG")
  (for-each pretty-print
            '((set-option :opt.priority box)

              (declare-datatype
               Mark ((SC)
                     (REL)
                     (ACQ)
                     (RLX)
                     (PLAIN)))

              (declare-datatype
               Operation ((F)
                          (R)
                          (W)
                          (XCHG)
                          (CMPXCHG)
                          (ADD)
                          (SUB)
                          (OR)
                          (XOR)
                          (AND)
                          (MAX)
                          (GET-ADD)
                          (ADD-GET)))

              (declare-fun writing (Operation) Bool)
              (assert (not (writing R)))
              (assert (not (writing F)))

              (declare-fun writing-only (Operation) Bool)
              (assert (writing-only W))
              (assert (writing-only ADD))
              (assert (writing-only SUB))
              (assert (writing-only OR))
              (assert (writing-only XOR))
              (assert (writing-only AND))
              (assert (writing-only MAX))

              (declare-fun reading (Operation) Bool)
              (assert (forall ((op Operation))
                              (=> (reading op)
                                  (and (not (= op F))
                                       (not (writing-only op))))))

              (declare-datatype
               Event ((mk-event
                       (eid Int)
                       (tid Int)
                       (op Operation)
                       (addr Int)
                       (rval Int)
                       (wval Int)
                       (mark Mark))))

              (declare-datatype
               Relation ((rf)
                         (co)
                         (po)
                         (po-addr)
                         (po-data)
                         (po-ctrl)
                         (self)))

              (declare-datatype
               Edge ((mk-edge
                      (rel Relation)
                      (src Int)
                      (dst Int))))

              (declare-const rels String)))
  (newline)
  (print "; BODY"))

;; Printing epilogue of constraints
(define (print-epilogue name)
  (newline)
  (print "; EPILOG")

  ;; Keep the name of the kitten as a constraint to pass on to roast.scm
  (pretty-print `(assert (= rels ,name )))
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "; ask SMT solver for an answer")
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "(check-sat)")
  (print "(get-model)"))

;; Conversion from event to symbolic representation
(define (event->symbol event)
  (string->symbol
   (string-append "ev" (number->string event))))

(define (edge->symbol id)
  (string->symbol (string-append "ed" (number->string id))))

;; Helper method to recursively flatten a list
;; ((a b (c (d))) e (f ((g))) h ((i) j (k))) => (a b c d e f g h i j k)
(define (flatten lst)
  (cond
    ((null? lst) '())
    ((not (pair? lst)) lst)
    ((not (pair? (car lst)))
     (cons (car lst) (flatten (cdr lst))))
    (else
     (append (flatten (car lst)) (flatten (cdr lst))))))

;; Helper method to generate all n^2 pairs between a collection of n items
(define (all-pairs lst)
  (apply append (map (lambda (x) (map (lambda (y) (list x y)) lst)) lst)))

(define (contains-any lst1 lst2)
  (> (length (intersect lst1 lst2)) 0))

(define (intersect lst1 lst2)
  (filter (lambda (x) (member x lst2)) lst1))
;; All of the listed edge names are renamed
(define (rename-relation rel)
  (let ((rel (string-downcase rel)))
    (match rel
           ("addr" "po-addr")
           ("data" "po-data")
           ("ctrl" "po-ctrl")
           (else rel))))

;; Recursively construct all edges
(define (make-edges el er expr)
  (match expr
         ;; Create a new event adn recursively create edges on the left and right
         (('seq . rest)
          (let ((counter (get-counter)))
            (list
             (make-edges el counter (car rest))
             (make-edges counter er (cadr rest)))))

         ;; Make two edges recursively with the same endpoints
         (('isect . rest)
          (list (make-edges el er (car rest))
                (make-edges el er (cadr rest))))

         ;; Create edges recursively with the endpoints swapped
         (('inv . rest)
          (make-edges er el rest))

         ;; Base cases
         (('self . ('set . rel))
          (list (edge el er "self" rel)))
         (('rel . rel)
          (list (edge el er (rename-relation rel) rel)))
         (('not 'self . ('set . rel))
          (list (edge el er (string-append "not-" (rename-relation rel)) rel)))
         (('not 'rel . rel)
          (list (edge el er (string-append "not-" (rename-relation rel)) rel)))

         ;; Should not happen
         (else (error "unexpected relation" expr))))


(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let* ((expr (cadr args))
         (type (car args))
         (is-acyclic (equal? "acyclic" type)))

    (let* ((expr (parse-expr expr))
           (edges (flatten (make-edges 0 (get-counter) expr))))

      (define (extract-events edges)
        (let ((pairs (map (lambda (edge)
                            (list (edge-dst edge) (edge-src edge)))
                          edges)))
          (sort (unique (flatten pairs)))))

      (define (append-observers edges)
        (apply append edges
               (map (lambda (e)
                      (if (equal? "co" (edge-type e))
                          (let ((o2 (get-counter))
                                (o1 (get-counter)))
                            (list
                             (edge (edge-src e) o1 "rf" "observe-co-src")
                             (edge (edge-dst e) o2 "rf" "observe-co-dst")
                             (edge o1 o2 "po" "observe-po")))
                          '())) edges)))

      (define (assert-cmp cmp field ed)
        (let ((f1 (if (pair? field) (car field) field))
              (f2 (if (pair? field) (cdr field) field))
              (src (event->symbol (edge-src ed)))
              (dst (event->symbol (edge-dst ed))))
          `(assert (,cmp (,f1 ,src) (,f2 ,dst)))))

      (define (assert-diff field ed)
        (assert-cmp 'distinct field ed))

      (define (assert-same field ed)
        (assert-cmp '= field ed))

      (define (assert-reading eid)
        (let ((ev (event->symbol eid)))
          `(assert (reading (op ,ev)))))

      (define (assert-read eid)
        (let ((ev (event->symbol eid)))
          `(assert (= (op ,ev) read))))

      (define (assert-writing eid)
        (let ((ev (event->symbol eid)))
          `(assert (writing (op ,ev)))))

      (define (edge-constraints ed edn)
        (match (edge-type ed)
               ("rf" (list
                      (assert-diff 'eid ed)
                      (assert-diff 'tid ed)
                      (assert-same 'addr ed)
                      (assert-same '(wval . rval) ed)
                      (assert-writing (edge-src ed))
                      (if (memv (edge-name ed) '("observe-co-src" "observe-co-dst"))
                          (assert-read (edge-dst ed))
                          (assert-reading (edge-dst ed)))))
               ("co" (list
                      (assert-diff 'eid ed)
                      (assert-diff 'tid ed)
                      (assert-same 'addr ed)
                      (assert-writing (edge-src ed))
                      (assert-writing (edge-dst ed))))
               ("po" (list
                      (assert-diff 'eid ed)
                      (assert-same 'tid ed)))
               ("po-data" (list
                           (assert-diff 'eid ed)
                           (assert-same 'tid ed)
                           (assert-same '(rval . wval) ed)
                           (assert-reading (edge-src ed))
                           (assert-writing (edge-dst ed))))
               ("po-addr" (list
                           (assert-diff 'eid ed)
                           (assert-same 'tid ed)
                           (assert-same '(rval . addr) ed)
                           (assert-reading (edge-src ed))))
               ("po-ctrl" (list
                           (assert-diff 'eid ed)
                           (assert-same 'tid ed)
                           (assert-diff 'addr ed)
                           (assert-reading (edge-src ed))
                           (assert-writing (edge-dst ed))))
               ("self" (list
                        `(assert (= ,(car edn) ,(cdr edn)))
                        (cond ((memv (edge-name ed) '("SC" "REL" "ACQ" "RLX"))
                               `(assert (= (mark ,(car edn)) ,(edge-name ed))))
                              ((equal? (edge-name ed) "PLAIN")
                               `(assert (or (= (op ,(car edn)) R)
                                            (= (op ,(car edn)) W))))
                              (else
                               `(assert (= (op ,(car edn)) ,(edge-name ed)))))))

               (_ (error "unknown edge type" (edge-type ed)))))

      (let* ((edges (append-observers edges))
             (events (extract-events edges)))

        (print-boilerplate)

        (for-each (lambda (ev)
                    (pretty-print `(declare-const ,(event->symbol ev) Event)))
                  events)
        (for-each (lambda (ed cnt)
                    (let ((edn (edge->symbol cnt))
                          (src (event->symbol (edge-src ed)))
                          (dst (event->symbol (edge-dst ed)))
                          (rel (string->symbol (edge-type ed))))
                      (newline)
                      (display "; ")
                      (pretty-print ed)
                      (pretty-print `(declare-const ,edn Edge))
                      (pretty-print `(assert (and (= (src ,edn) (eid ,src))
                                                  (= (dst ,edn) (eid ,dst))
                                                  (= (rel ,edn) ,rel))))
                      (apply pretty-print (edge-constraints ed `(,src . ,dst)))))
                  edges
                  (seq (length edges)))

        (when is-acyclic
          (let ((first (event->symbol 0))
                (last (event->symbol 1)))
            (print `(assert (= ,first ,last)))))

        (print-epilogue (string-append (car args) " " (cadr args)))
        )))



  0)

(start-command main)
