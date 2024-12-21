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

;; Definition of edge record
(define-record-type
  edge-record
  (edge src dst type attr)
  edge?
  (src edge-src)
  (dst edge-dst)
  (type edge-type)
  (attr edge-attr))

;; Counter for naming events
(define counter 1)

;; Every time a new event is created we increment the counter
(define (get-counter)
  (let ((current counter))
    (set! counter (+ counter 1))
    current))

;; Print separator comment
(define (print-separator cmt)
  (newline)
  (apply print "; " (map (lambda (_) "-") (seq 78)))
  (print "; " cmt)
  (apply print "; " (map (lambda (_) "-") (seq 78))))

(define (print-smt-config)
  (print '(set-option :opt.priority box)))

;; Printing boilerplate
(define (print-boilerplate)
  (for-each pretty-print
            '((declare-const kitten String)

              (declare-datatype
               Mark ((SC)
                     (REL)
                     (ACQ)
                     (RLX)
                     (Plain)))

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
                                  (and (not (writing-only op))
                                       (not (= op F))))))

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
                      (dst Int)))))))

;; Printing epilogue of constraints
(define (print-epilogue name)
  ;; Keep the name of the kitten as a constraint to pass on to roast.scm
  (pretty-print `(assert (= kitten ,name )))
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

;; Helper to get all elements of a list besides the last element
(define (but-last xs) (reverse (cdr (reverse xs))))

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
            (list (make-edges el counter (car rest))
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

(define (extract-events edges)
  (let ((pairs (map (lambda (edge) (list (edge-dst edge) (edge-src edge)))
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

(define (assert-cmp cmp field eid-src eid-dst)
  (let ((f1 (if (pair? field) (car field) field))
        (f2 (if (pair? field) (cdr field) field))
        (src (event->symbol eid-src))
        (dst (event->symbol eid-dst)))
    `(assert (,cmp (,f1 ,src) (,f2 ,dst)))))

(define (assert-diff field eid-src eid-dst)
  (assert-cmp 'distinct field  eid-src eid-dst))

(define (assert-same field eid-src eid-dst)
  (assert-cmp '= field eid-src eid-dst))

(define (assert-reading eid)
  (let ((ev (event->symbol eid)))
    `(assert (reading (op ,ev)))))

(define (assert-plain-read eid)
  (let ((ev (event->symbol eid)))
    `(assert (and (= (op ,ev) R)
                  (= (mark ,ev) Plain)))))

(define (assert-writing eid)
  (let ((ev (event->symbol eid)))
    `(assert (writing (op ,ev)))))

(define (assert-memory eid)
  (let ((ev (event->symbol eid)))
    `(assert (or (writing (op ,ev))
                 (reading (op ,ev))))))

(define (memory-order? x)
  (member x '("SC" "REL" "ACQ" "RLX")))

(define (plain? x)
  (member x '("PLAIN" "Plain")))

(define (operation? x)
  (and (not (memory-order? x))
       (not (plain? x))))

(define (observer? x)
  (member x '("observe-co-src" "observe-co-dst")))

(define (assert-same-events eid1 eid2)
  (let ((ev1 (event->symbol eid1))
        (ev2 (event->symbol eid2)))
    `(assert (= ,ev1 ,ev2))))

(define (assert-value k eid v)
  (let ((ev (event->symbol eid)))
    `(assert (= (,k ,ev) ,(string->symbol v)))))

(define (assert-plain eid)
  (let ((ev (event->symbol eid)))
    `(assert (and (or (= (op ,ev) R)
                      (= (op ,ev) W))
                  (= (mark ,ev) Plain)))))

(define (edge-constraints ed)
  (let ((src (edge-src ed))
        (dst (edge-dst ed))
        (type (edge-type ed))
        (attr (edge-attr ed)))
    (match type
           ("rf" (list (assert-diff 'eid src dst)
                       (assert-diff 'tid src dst)
                       (assert-same 'addr src dst)
                       (assert-same '(wval . rval) src dst)
                       (assert-writing src)
                       (if (observer? attr)
                           (assert-plain-read dst)
                           (assert-reading dst))))
           ("co" (list (assert-diff 'eid src dst)
                       (assert-diff 'tid src dst)
                       (assert-same 'addr src dst)
                       (assert-writing src)
                       (assert-writing dst)))
           ("po" (list (assert-diff 'eid src dst)
                       (assert-same 'tid src dst)))
           ("po-data" (list (assert-diff 'eid src dst)
                            (assert-same 'tid src dst)
                            (assert-same '(rval . wval) src dst)
                            (assert-reading src)
                            (assert-writing dst)))
           ("po-addr" (list (assert-diff 'eid src dst)
                            (assert-same 'tid src dst)
                            (assert-same '(rval . addr) src dst)
                            (assert-reading src)
                            (assert-memory dst)))
           ("po-ctrl" (list (assert-diff 'eid src dst)
                            (assert-same 'tid src dst)
                            (assert-diff 'addr src dst)
                            (assert-reading src)
                            (assert-writing dst)))
           ("self" (list (assert-same-events src dst)
                         (cond ((memory-order? attr)
                                (assert-value 'mark src attr))
                               ((plain? attr)
                                (assert-plain src))
                               ((operation? attr)
                                (assert-value 'op src attr))
                               (else (error "unexpected attr" attr)))))
           (_ (error "unknown edge type" type)))))

;; core logic of resolve
(define (run type texpr)
  (let* ((is-acyclic (equal? "acyclic" type))
         (expr (parse-expr texpr))
         (edges (flatten (make-edges 0 (get-counter) expr)))
         (edges (append-observers edges))
         (events (extract-events edges)))

    (print-smt-config)
    (print-separator "Type definitions")
    (print-boilerplate)

    (print-separator "Constraints")
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
                  (apply pretty-print (edge-constraints ed))))
              edges
              (seq (length edges)))

    (when is-acyclic
      (let ((first (event->symbol 0))
            (last (event->symbol 1)))
        (print `(assert (= ,first ,last)))))

    (print-separator "Request model from SMT")
    (print-epilogue (string-append type " " texpr))))

(define (main args)
  (die-unless (not (zero? (length args))) "edge list")
  (run (car args) (cadr args))
  0)

(start-command main)
