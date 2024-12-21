#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

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
  (edge src trg type name)
  edge?
  (src edge-src)
  (trg edge-trg)
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
;; b is trg ev
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
  (with-input-from-file
   "boilerplate.smt2"
   (lambda ()
     (let loop ((line (read-line)))
       (unless (eof-object? line)
         (write-string line)
         (newline)
         (loop (read-line)))))))

;; Printing epilogue of constraints
(define (print-epilogue name)
  (newline)

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

;; Conversion from edge to symbolic representation
(define (edge->name el er)
  (string->symbol
   (let ((name-base (string-append "ed" (number->string el) "-" (number->string er))))

     ;; Base edge names are stored in a hash table and the number of their occurances is counted
     (if (hash-table-exists? edges-count name-base)
         (hash-table-set! edges-count name-base (+ (hash-table-ref edges-count name-base) 1))
         (hash-table-set! edges-count name-base 1)
         )
     (string-append name-base "-" (number->string (hash-table-ref edges-count name-base)))
     )))

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
  (apply append
         (map (lambda (x)
                (map (lambda (y)
                       (list x y))
                     lst))
              lst)))

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
                            (edge-trg
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
;; First we collect all events that are either the src or trg of a co edge
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
                    (trg (edge-trg edge))
                    (type (edge-type edge)))
                (when (equal? type "co")
                  (set! co-events (cons src (cons trg co-events))))))
            edges)

  ;; Collect all affected "co" group-mates
  (let ((all-co (apply append (map (lambda (co-ev) (car (find-group co-ev group-mates))) co-events))))
    (map (lambda (ev) (cons ev (property-map all-co ev))) events)))

;; In exists condition we should only assert reads that are trg of rf edges
;; First we collect all events that are trg of a rf edge
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
              (let ((trg (edge-trg edge))
                    (type (edge-type edge)))
                (when (equal? type "rf")
                  (set! rf-events (cons trg rf-events)))))
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

;; First we collect all events that are trg of data/addr/ctrl edges
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
              (let ((trg (edge-trg edge))
                    (type (edge-type edge)))
                (cond
                  ((equal? type "data-dep")
                   (set! data-events (cons trg data-events)))
                  ((equal? type "addr-dep")
                   (set! addr-events (cons trg addr-events)))
                  ((equal? type "ctrl-a-dep")
                   (set! ctrl-events (cons trg ctrl-events))))))
            edges)

  ;; We propagate affected events' arguments to all events with the same eid
  (let ((all-data (apply append (map (lambda (data-ev) (car (find-group data-ev group-mates))) data-events)))
        (all-addr (apply append (map (lambda (addr-ev) (car (find-group addr-ev group-mates))) addr-events)))
        (all-ctrl (apply append (map (lambda (ctrl-ev) (car (find-group ctrl-ev group-mates))) ctrl-events))))
    (map (lambda (ev) (cons ev (property-map all-data all-addr all-ctrl ev))) events)))

;; Given all events and pairs describing their connections, we want to find the sets of events that have common connections
;; In other words we want the Union Find
;; Here a simple recursive version is implemented
;; 1. Take the next pair (connects two events)
;; 2. Find the groups the two events of the pair belong to
;; 3. Remove both groups
;; 4. Add the union of the two groups
;; Repeat untill no more pairs
(define (simple-uf groups pairs)

  ;; Helper method to find the group an event belnogs to
  (define (find-group groups x)
    (find (lambda (group) (member x group)) groups))

  ;; Helper method to remove a given group
  (define (remove-group groups g)
    (filter (lambda (group) (not (equal? group g))) groups))

  ;; Main body of the procedure
  (if (null? pairs)
      groups
      (let* ((pair (car pairs))
             (a (find-group groups (car pair)))
             (b (find-group groups (cadr pair)))
             (new-groups-1 (remove-group groups a))
             (new-groups-2 (remove-group new-groups-1 b))
             (new-groups-3 (append new-groups-2 (list (unique (append a b))))))

        ;; Call again without the pair that was just processed
        (simple-uf new-groups-3 (cdr pairs)))))

;; If two rf edges point to the same event, then their sources must have the same eid
;; a -rf-> b <-rf- c   =>   a is the same as c
;; Given a set of pairs of rf edges, we want to find all events with the same eid
;; We check if the targets of a pair of rf edges belong to the same eid group
;; If they do, we add the sources to the same group in a similiar manner to simple-uf
;; In case they don't, we don't discard the pair, since the targets might be added later
;; The helper method executes a procedure similiar to simple-uf
;; Helper is called again and again, untill the resulting eid-set remains the same between two calls
;; Once the set is unchanged, we know that no other pair of rf edges can affect the eid set
(define (rf-uf groups rf-pairs)

  ;; Helper method to find the group an event belnogs to
  (define (find-group groups x)
    (find (lambda (group) (member x group)) groups))

  ;; Helper method to remove a given group
  (define (remove-group groups g)
    (filter (lambda (group) (not (equal? group g))) groups))

  ;; Similiar to simple-uf
  ;; Only difference is we check if the two trg belong to the same eid-group
  ;; If they do, we do the same as simple-uf
  ;; If they don't, we discard this pair and continue
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
               (new-groups-1 (if flag
                                 (remove-group groups-h a)
                                 groups-h))

               (new-groups-2 (if flag
                                 (remove-group new-groups-1 b)
                                 groups-h))

               (new-groups-3 (if flag
                                 (append new-groups-2 (list (unique (append a b))))
                                 groups-h)))

          ;; Call again without the pair that was just processed
          (helper new-groups-3 (cdr rf-pairs-h)))))

  ;; Main body of the procedure
  ;; Call helper with the same set of rf-pairs untill no change has occurred between consecutive calls
  ;; At this point we know that nothing more will change
  (let* ((new-groups (helper groups rf-pairs)))
    (if (eq? (length new-groups) (length groups))
        groups
        (rf-uf new-groups rf-pairs))))

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
;; We transform them in a simplified form (we only care about src and trg)
;; We get all pairs
(define (get-rf-pairs edges)
  (let* ((rf-edges (filter (lambda (edge) (equal? (edge-type edge) "rf")) edges))

         (rf-edges (map (lambda (e) (list (edge-src e) (edge-trg e))) rf-edges))
         (rf-pairs (all-pairs rf-edges))
         (rf-pairs (map (lambda (pair) (list (list (cadar pair) (cadadr pair)) (list (caar pair) (caadr pair)))) rf-pairs)))
    rf-pairs
    ))

;; All of the listed edges are of the type [X]
;; For each one of them, we create a pair of the src and trg
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
         (pairs (map (lambda (edge) (list (edge-src edge) (edge-trg edge))) set-edges)))
    pairs))

;; All of the listed edge names are renamed
(define (rename-relation rel)
  (let ((rel (string-downcase rel)))
    (match rel
           ("addr" "addr-dep")
           ("data" "data-dep")
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
          (list (edge el er (rename-relation rel) (edge->name el er))))
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

;; Assert that all the passed event have the specified field different
(define (distinct-assertion lst field)
  (let* ((constraints (map (lambda (x)
                             `(,field ,(string->symbol (string-append "ev" (number->string x)))))
                           lst)))
    `((assert (distinct ,@constraints)))))

;; For the passed field and distinct flag assert that all events are the same/different based on the eid-partition
;; All events within an eid group have the specified field the same
;; If distinct is true, all events from separate groups have the specified field different
(define (eid-constraints eid-partition field distinct)
  (let* ((eid-partition (filter (lambda (lst) (> (length lst) 0)) eid-partition))
         (multy (filter (lambda (lst) (> (length lst) 1)) eid-partition))
         (cars (map car eid-partition)))
    (if distinct
        (append
         (apply append (map (lambda (el) (equality-assertion el field)) multy))
         (distinct-assertion cars field))
        (apply append (map (lambda (el) (equality-assertion el field)) multy)))))

;; Generate assertions that events have the obs attribute true or false
(define (exists-condition-constraints co-prop field)
  (map (lambda (ev-pair)
         (let ((event (car ev-pair))
               (value (cdr ev-pair)))
           (if value
               `(assert (,field ,(string->symbol (string-append "ev" (number->string event)))))
               `(assert (not (,field ,(string->symbol (string-append "ev" (number->string event))))))))
         ) co-prop))

;; Generate assertions for the argument type of constraints (data/addr/ctrl/reg)
(define (dep-constraints marked-events)
  (map (lambda (ev)
         `(assert (= (as ,(string->symbol (cdr ev)) Argument) (arg ,(event->symbol (car ev)))))
         ) marked-events))

;; Writes should have all distinct write values
;; Unless an event is the target of a data dependency, then the two write can potentially have the same write value
;; For instance w1 rf;data w2  w1 and w2 must write the same value
;; Therefore we remove those events when generating the write value constraints
(define (remove-data-dep-writes eid-partition data-trg)
  (map (lambda (group)
         (filter (lambda (el)
                   (not (member el data-trg)))
                 group))
       eid-partition))

;; Main procedure that generates all non-boilerplate constraints
(define (generate-constraints events edges is-acyclic)
  (let* ((event-names (map event->symbol events))
         (edge-names (map edge-name edges))
         (eid-partition (get-eid-partition events edges is-acyclic))
         (co-events (update-co-properties events eid-partition edges))
         (rf-trg-events (update-rf-properties events eid-partition edges))
         (marked-events (update-dep-properties events eid-partition edges))
         (no-addr-partition (remove-data-dep-writes eid-partition (map car (filter (lambda (ev) (equal? (cdr ev) "data")) marked-events))))
         (addr-src-list (get-addr-src-list edges eid-partition))
         (before-addr-chains (get-before-addr-chains edges eid-partition))
         (beginings-of-chains (unique (map car before-addr-chains)))
         (continuations-of-chains (unique (apply append (map cdr before-addr-chains))))
         (not-on-chain (filter (lambda (ev) (not (or (member ev beginings-of-chains) (member ev continuations-of-chains)))) events))
         (addr-partition (remove-data-dep-writes eid-partition (map car (filter (lambda (ev) (not (equal? (cdr ev) "data"))) marked-events)))))
    ;(print rf-trg-events)
    ;  (print addr-src-list)
    ;   (print before-addr-chains)
    ;   (print beginings-of-chains)
    ;   (print continuations-of-chains)
    ;   (print not-on-chain)
    (apply append (list


                   (comment "event declarations")
                   (map (lambda (e) `(declare-const ,e Event))
                        event-names)
                   (eid-constraints eid-partition 'eid #t)
                   (eid-constraints eid-partition 'tid #f)
                   (eid-constraints eid-partition 'porder #t)
                   (eid-constraints eid-partition 'corder #t)
                   (eid-constraints eid-partition 'addr #f)
                   (eid-constraints eid-partition 'val-r #f)
                   (eid-constraints no-addr-partition 'val-w #t)
                   (eid-constraints addr-partition 'val-w #f)
                   (eid-constraints eid-partition 'val-e #f)
                   (eid-constraints eid-partition 'val-d #f)
                   (eid-constraints eid-partition 'op #f)
                   (eid-constraints eid-partition 'marker1 #f)
                   (eid-constraints eid-partition 'marker2 #f)
                   (eid-constraints eid-partition 'rmw-type #f)
                   (eid-constraints eid-partition 'arg #f)
                   (eid-constraints eid-partition 'obs #f)

                   (comment "events have different arg based on preceding edge")
                   (dep-constraints marked-events)

                   (comment "only events connected to co should be on observer thread")
                   (exists-condition-constraints co-events 'obs)

                   (comment "only reads that are target of rf should be in exists condition")
                   (exists-condition-constraints rf-trg-events 'ass)

                   (map (lambda (e) `(assert (= (chain-type ,(event->symbol e)) (as start-chain Addr-Chain))))
                        beginings-of-chains)

                   (map (lambda (e) `(assert (= (chain-type ,(event->symbol e)) (as middle-chain Addr-Chain))))
                        continuations-of-chains)

                   (map (lambda (e) `(assert (= (chain-type ,(event->symbol e)) (as no-chain Addr-Chain))))
                        not-on-chain)

                   (comment "edge declarations")
                   (map (lambda (e) `(declare-const ,e Edge))
                        edge-names)

                   (comment "uid is distinct for all events")
                   `((assert (distinct ,@(map (lambda (e)
                                                `(uid , e))
                                              event-names))))

                   (comment "assertions to stop smt from creating edges")
                   (comment "without inSet the solver will create edges to make the latter forall assertions fail")
                   (let* ((equalis (map (lambda (edge) `(= e ,edge))
                                        edge-names)))
                     `((assert (forall ((e Edge))
                                       (= (inEdgeSet e)
                                          (or ,@equalis))))))

                   (comment "assertions to stop smt from creating events")
                   (let* ((equalis (map (lambda (event) `(= e ,event))
                                        event-names)))

                     `((assert (forall ((e Event))
                                       (= (inEventSet e)
                                          (or ,@equalis))))))


                   (comment "assert relations between events in the graph")
                   (map (lambda (edge)
                          (let ((name (edge-name edge))
                                (src (event->symbol (edge-src edge)))
                                (trg (event->symbol (edge-trg edge)))
                                (type (string->symbol (edge-type edge))))
                            `(assert (= ,name (mk-edge ,src ,trg ,type)))))
                        edges)


                   (comment "reads and RNW have to read from an rf edge or from an init event")
                   (map (lambda (ev)
                          ;; IF the type of an edge is read or read-modify write
                          `(assert
                            (=> (and (or (= (op ,ev) (as read Operation))
                                         (= (op ,ev) (as read-modify-write Operation)))
                                     ;; and there doesn't exist an rf edge pointing to this event
                                     (not (exists ((e1 Edge))
                                                  (and (inEdgeSet e1)
                                                       (= (eid (trg e1)) (eid ,ev))
                                                       (= (rel e1) (as rf Relation))
                                                       ))))
                                ;; THEN the value that the event reads is 0 (INIT)
                                (= (val-r ,ev) 0)))
                          ) event-names)

                   (comment "RMW events must write a different value than the one they read")
                   (map (lambda (e) (append
                                     ;; IF the edge is faa/xchg/cas-s/cas-f
                                     `(assert (=> (and (or
                                                        (= (rel ,e) (as faa Relation))
                                                        (= (rel ,e) (as xchg Relation))
                                                        (= (rel ,e) (as cas-s Relation))
                                                        (= (rel ,e) (as cas-f Relation)))
                                                       ;; and it it in the edge set
                                                       (inEdgeSet ,e)
                                                       ;; and there does not exist an edge such that
                                                       ,@(apply append (map (lambda (e1)
                                                                              ;; it is an rf edge
                                                                              `( (not (and (= (rel ,e1) (as rf Relation)) (inEdgeSet ,e1)
                                                                                           ;; pointing from the src to the trg of the RMW
                                                                                           ;; or the other way around
                                                                                           (or (and (= (eid (src ,e)) (eid (src ,e1)))
                                                                                                    (= (eid (trg ,e)) (eid (trg ,e1))))
                                                                                               (and (= (eid (src ,e)) (eid (src ,e1)))
                                                                                                    (= (eid (trg ,e)) (eid (trg ,e1))))))))
                                                                              ) edge-names)))
                                                  ;; THEN the value it reads must be different than the value it writes
                                                  (not (= (val-r (src ,e)) (val-w (trg ,e)))))) )
                          ) edge-names)
                   ))))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")

  (let* ((expr (cadr args))
         (type (car args))
         (is-acyclic (equal? "acyclic" type)))

    (let* ((expr (parse-expr expr))
           (edges (flatten (make-edges 0 (get-counter) expr)))
           (events (sort (unique (flatten (map (lambda (edge) (list (edge-trg edge) (edge-src edge))) edges)))))
           (constraints (generate-constraints events edges is-acyclic)))

      (print-boilerplate)
      (for-each pretty-print constraints)
      (print-epilogue (string-append (car args) " " (cadr args)))
      ))
  0)

(start-command main)
