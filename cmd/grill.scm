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

;; Flag to maximise threads
(define maxi-threads-flag #t)

;; Flag to maximise addresses
(define maxi-addr-flag #t)

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
               Operation ((read)
                          (write)
                          (xchg)
                          (cmpxchg)
                          (rmw)
                          (fence)))

              (declare-datatype
               Relation ((rf)
                         (co)
                         (po)
                         (po-addr)
                         (po-data)
                         (po-ctrl)
                         (self)))

              (declare-datatype
               Mark ((SC)
                     (REL)
                     (ACQ)
                     (RLX)
                     (PLAIN)))

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

(define (edge->symbol/old ed)
  (edge->name (edge-src ed) (edge-dst ed)))

;; Conversion from edge to symbolic representation
(define (edge->name el er)
  (string->symbol
   (let ((name-base (string-append "ed" (number->string el) "-" (number->string er))))

     ;; Base edge names are stored in a hash table and the number of their occurances is counted
     (if (hash-table-exists? edges-count name-base)
         (hash-table-set! edges-count name-base (+ (hash-table-ref edges-count name-base) 1))
         (hash-table-set! edges-count name-base 1))

     (string-append name-base "-" (number->string (hash-table-ref edges-count name-base))))))

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

(define (get-before-addr-chain addr-src edges)
  (letrec
    ((helper
      (lambda (current-chain expected-type)
        (let* ((current-node (car current-chain))
               (valid-edges
                (filter (lambda
                          (edge)
                          (and
                           (equal?
                            (edge-type
                             edge)
                            expected-type)
                           (equal?
                            (edge-dst
                             edge)
                            current-node)))
                        edges)))
          (if (null? valid-edges)
              (list
               current-chain)
              (apply
               append
               (map
                (lambda
                  (edge)
                  (helper
                   (cons
                    (edge-src
                     edge)
                    current-chain)
                   (if
                    (equal?
                     expected-type
                     "rf")
                    "data-dep"
                    "rf")))
                valid-edges)))))))
    (helper
     (list
      addr-src)
     "rf")))

(define (get-before-addr-chains edges eid-partition)
  (let* ((addr-src-list (get-addr-src-list edges eid-partition))
         (chains (map (lambda (addr-src) (get-before-addr-chain addr-src edges))
                      addr-src-list)))
    (apply append chains)
    )
  )

(define (get-addr-src-list edges eid-partition)
  (let* ((addr-src-direct (map edge-src (filter (lambda (edge) (equal? (edge-type edge) "addr-dep")) edges)))
         (events-from-partition (filter (lambda (eid-set) (contains-any eid-set addr-src-direct)) eid-partition)))
    ;(print addr-src-direct)
    (apply append events-from-partition)))

(define (contains-any lst1 lst2)
  (> (length (intersect lst1 lst2)) 0))

(define (intersect lst1 lst2)
  (filter (lambda (x) (member x lst2)) lst1))

;; Observer threads can only observe writes that are adjacent to co edges
;; First we collect all events that are either the src or dst of a co edge
;; Then we propagate to all events with the same eid based on the eid-partition
(define (update-co-properties events group-mates edges)

  ;; Initialize list for "co" events
  (define co-events '())

  ;; Property map for "co" edges
  (define (property-map co-events ev)
    (if (member ev co-events) #t #f))

  ;; Helper function to find to which eid group an event belongs to
  (define (find-group event groups)
    (filter (lambda (group) (member event group)) groups))

  ;; Collecting affected events based on "co" edges
  (for-each (lambda (edge)
              (let ((src (edge-src edge))
                    (dst (edge-dst edge))
                    (type (edge-type edge)))
                (when (equal? type "co")
                  (set! co-events (cons src (cons dst co-events))))))
            edges)

  ;; Collect all affected "co" group-mates
  (let ((all-co (apply append (map (lambda (co-ev) (car (find-group co-ev group-mates))) co-events))))
    (map (lambda (ev) (cons ev (property-map all-co ev))) events)))

;; In exists condition we should only assert reads that are dst of rf edges
;; First we collect all events that are dst of a rf edge
;; Then we propagate to all events with the same eid based on the eid-partition
(define (update-rf-properties events group-mates edges)

  ;; Initialize list for "rf" events
  (define rf-events '())

  ;; Property map for "rf" edges
  (define (property-map rf-events ev)
    (if (member ev rf-events) #t #f))

  ;; Helper function to find to which eid group an event belongs to
  (define (find-group event groups)
    (filter (lambda (group) (member event group)) groups))

  ;; Collecting affected events based on "rf" edges
  (for-each (lambda (edge)
              (let ((dst (edge-dst edge))
                    (type (edge-type edge)))
                (when (equal? type "rf")
                  (set! rf-events (cons dst rf-events)))))
            edges)
  ;(print rf-events)
  ;; Collect all affected "rf" group-mates
  (let ((all-rf (apply append (map (lambda (rf-ev) (car (find-group rf-ev
                                                                    group-mates))) rf-events))))
    (map (lambda (ev) (cons ev (property-map all-rf ev))) events)))


;; Different events write/read from different locations based on their types and edges they are connected to
;; This information is stored in the arg field
;; If an event comes after a data edge it has the data arg
;; If an event comes after an addr edge it has the addr arg
;; If an event comes after the first part of a ctrl (ctrl-a) it has the ctrl arg

;; First we collect all events that are dst of data/addr/ctrl edges
;; Then we propagate to all events with the same eid b ased on the eid-partition
(define (update-dep-properties events group-mates edges)

  ;; Initialize lists for different properties
  (define data-events '())
  (define addr-events '())
  (define ctrl-events '())

  ;; Propert map that tells us to what arg each event is mapped
  (define (property-map data-events addr-events ctrl-events ev)
    (cond
      ((member ev data-events) "data")
      ((member ev addr-events) "addr")
      ((member ev ctrl-events) "ctrl")
      (else "reg")))

  ;; Helper function to find to which eid group an event belongs to
  (define (find-group event groups)
    (filter (lambda (group) (member event group)) groups))

  ;; Collecting affected events based on edge types
  (for-each (lambda (edge)
              (let ((dst (edge-dst edge))
                    (type (edge-type edge)))
                (cond
                  ((equal? type "data-dep")
                   (set! data-events (cons dst data-events)))
                  ((equal? type "addr-dep")
                   (set! addr-events (cons dst addr-events)))
                  ((equal? type "ctrl-a-dep")
                   (set! ctrl-events (cons dst ctrl-events))))))
            edges)

  ;; We propagate affected events' arguments to all events with the same eid
  (let ((all-data (apply append (map (lambda (data-ev) (car (find-group data-ev group-mates))) data-events)))
        (all-addr (apply append (map (lambda (addr-ev) (car (find-group addr-ev group-mates))) addr-events)))
        (all-ctrl (apply append (map (lambda (ctrl-ev) (car (find-group ctrl-ev group-mates))) ctrl-events))))
    (map (lambda (ev) (cons ev (property-map all-data all-addr all-ctrl ev))) events)))


;; Edges of the form [X] connect an event with its self therefore they have the same eid
;; If the acyclic keyword is used, the first and last event have the same eid
;; We get the eid groups after performing simple uf
;; The we collect all pairs of rf edges
;; Finally, we use the rf-uf procedure to get the eid-partition of all events
(define (get-eid-partition events edges is-acyclic)
  (let* ((same-eid-sets (get-same-eid-set edges))
         (same-eid (if is-acyclic (append same-eid-sets (list (list 1 0))) same-eid-sets))
         (groups (simple-uf (map list events) same-eid))
         (rf-pairs (get-rf-pairs edges))
         (groups (rf-uf groups rf-pairs)))
    groups))

;; Helper method to get all pairs of rf edges
;; First we collect all edges with type rf
;; We transform them in a simplified form (we only care about src and dst)
;; We get all pairs
(define (get-rf-pairs edges)
  (let* ((rf-edges (filter (lambda (edge) (equal? (edge-type edge) "rf")) edges))
         (rf-edges (map (lambda (e) (list (edge-src e) (edge-dst e))) rf-edges))
         (rf-pairs (all-pairs rf-edges))
         (rf-pairs (map (lambda (pair) (list (list (cadar pair) (cadadr pair)) (list (caar pair) (caadr pair)))) rf-pairs)))
    rf-pairs
    ))

;; All of the listed edges are of the type [X]
;; For each one of them, we create a pair of the src and dst
(define (get-same-eid-set edges)
  (let* ((set-edges (filter (lambda (edge) (or
                                            (equal? (edge-type edge) "xchg")
                                            (equal? (edge-type edge) "faa")
                                            (equal? (edge-type edge) "cas-s")
                                            (equal? (edge-type edge) "cas-f")
                                            (equal? (edge-type edge) "r")
                                            (equal? (edge-type edge) "w")
                                            (equal? (edge-type edge) "b")
                                            (equal? (edge-type edge) "f")
                                            (equal? (edge-type edge) "sc")
                                            (equal? (edge-type edge) "acq")
                                            (equal? (edge-type edge) "release")
                                            (equal? (edge-type edge) "rlx")
                                            (equal? (edge-type edge) "rel-acq")
                                            (equal? (edge-type edge) "plain"))) edges))
         (pairs (map (lambda (edge) (list (edge-src edge) (edge-dst edge))) set-edges)))
    pairs))

;; All of the listed edge names are renamed
(define (rename-relation rel)
  (let ((rel (string-downcase rel)))
    (match rel
           ("addr" "po-addr")
           ("data" "po-data")
           ("ctrl" "po-ctrl")
           ("ctrl-a" "ctrl-a-dep")
           ("ctrl-b" "ctrl-b-dep")
           ("rel" "release")
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
          (list (edge el er (rename-relation rel) (edge->name el er))))
         (('not 'self . ('set . rel))
          (list (edge el er (string-append "not-" (rename-relation rel)) (edge->name el er))))
         (('not 'rel . rel)
          (list (edge el er (string-append "not-" (rename-relation rel)) (edge->name el er))))

         ;; Should not happen
         (else "")))

;; Assert that all the passed event have the specified field same
(define (equality-assertion lst field)
  (let* ((first (car lst))
         (rest (cdr lst))
         (constraints (map (lambda (x)
                             `(= (,field ,(string->symbol (string-append "ev" (number->string first))))
                                 (,field ,(string->symbol (string-append "ev" (number->string x))))))
                           rest)))
    `((assert (and ,@constraints)))))


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
          `(assert (or (= (op ,ev) read)
                       (= (op ,ev) xchg)))))

      (define (assert-read eid)
        (let ((ev (event->symbol eid)))
          `(assert (= (op ,ev) read))))

      (define (assert-writing eid)
        (let ((ev (event->symbol eid)))
          `(assert (or (= (op ,ev) write)
                       (= (op ,ev) xchg)))))

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
                        (cond ((equal? (edge-name ed) "W")
                               `(assert (= (op ,(car edn)) write)))
                              ((equal? (edge-name ed) "R")
                               `(assert (= (op ,(car edn)) read)))
                              ((equal? (edge-name ed) "XCHG")
                               `(assert (= (op ,(car edn)) xchg)))
                              ((equal? (edge-name ed) "REL")
                               `(assert (= (mark ,(car edn)) REL)))
                              (else (error "unknown attr" (edge-name ed))))))

               (_ (error "unknown edge type" (edge-type ed)))))

      (let* ((edges (append-observers edges))
             (events (extract-events edges)))
        ;(pretty-print edges)
        ;(pretty-print events)

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
