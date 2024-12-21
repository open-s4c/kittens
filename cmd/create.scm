#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(import (scheme base)
        (scheme file)
        (scheme read)
        (scheme eval)
        (srfi 1)
        (srfi 69) ; hash
        (srfi 95) ; sort
        (kittens match)
        (kittens utils)
        (kittens debug)
        (kittens command))

(define type "a")

(define size_flag #t)

(define size (if size_flag "int" "long"))

(define (usage)
  (print "bake <z3 model>"))

; ------------------------------------------------------------------------------
; main
; ------------------------------------------------------------------------------
(define (main args)
  (die-unless (= (length args) 1) "model file missing")

  (let* ((fn (car args))
         (file-content (if (equal? "-in" fn)
                           (begin (read) (read))
                           (with-input-from-file fn
                            (lambda () (read) (read))))))
    (run3 file-content))
  0)

; ------------------------------------------------------------------------------
; support
; ------------------------------------------------------------------------------
(define-record-type
  event-record
  (event eid tid op addr rval wval mark)
  event?
  (eid event-eid)
  (tid event-tid)
  (op event-op)
  (addr event-addr)
  (rval event-rval)
  (wval event-wval)
  (mark event-mark))

(define (get-test-name expr)
  (match expr
         (('model . defs)
          (apply string-append (map get-test-name defs)))
         (('define-fun _ _ 'String str)
          str)
         (else "")))

(define (but-last xs) (reverse (cdr (reverse xs))))

(define (extract-instances type model)
  (filter values
          (map (lambda (x)
                 (match x
                        (('define-fun name _ T val)
                         (if (eq? T type) (cons name val) #f))
                        (_ #f))) model)))

(define (shared-ptr addr)
  (string-append "(shrd + " (number->string addr) ")"))

(define (local-ptr addr)
  (string-append "(priv + " (number->string addr) ")"))

(define (cond-on addr)
  (string-append "if (*" (local-ptr addr) ") "))

(define (value-name val)
  (string-append "v" (number->string val)))

; ------------------------------------------------------------------------------
; generator
; ------------------------------------------------------------------------------
(define (split-per-process evs)
  (let iter ((evs evs) (groups '()))
    (if (null? evs)
        groups
        (iter (cdr evs)
              (let ((ev (car evs)))
                (if (null? groups)
                    (list (list ev))
                    (let* ((group (car groups))
                           (lastev (car group)))
                      (if (= (event-tid ev) (event-tid lastev))
                          (cons (cons ev group) (cdr groups))
                          (cons (list ev) groups)))))))))

(define (create-litmus model)
  (let* ((model (cons 'model model))
         (name (get-test-name model))
         (po-map (make-hash-table))
         (addr-dep (make-hash-table))
         (data-dep (make-hash-table))
         (ctrl-dep (make-hash-table))
         (rf-dst '())
         (edges (extract-instances 'Edge model))
         (events (extract-instances 'Event model)))

    (define (resolve-deps ev)
      (let* ((eid (event-eid ev))
             (addr (if (hash-table-exists? addr-dep eid)
                       `(ref priv ,(hash-table-ref addr-dep eid))
                       `(ref shrd ,(event-addr ev))))
             (wval (if (hash-table-exists? data-dep eid)
                       `(deref priv ,(hash-table-ref data-dep eid))
                       (event-wval ev)))
             (cnd (if (hash-table-exists? ctrl-dep eid)
                      `(onlyif (deref shrd ,(hash-table-ref ctrl-dep eid)))
                      #f)))
        `(code ,cnd ,(event (event-eid ev)
                            (event-tid ev)
                            (event-op ev)
                            addr
                            (event-rval ev)
                            wval
                            (event-mark ev)))))

    ; populate maps and lists
    (for-each (lambda (e)
                (match e
                       ((_ 'mk-edge 'rf _ dst)
                        (set! rf-dst (cons dst rf-dst)))
                       ((_ 'mk-edge type src dst)
                        (when (memv type '(po po-addr po-data po-ctrl))
                          (hash-table-set! po-map src dst)
                          (match type
                                 ('po-addr (hash-table-set! addr-dep dst src))
                                 ('po-data (hash-table-set! data-dep dst src))
                                 ('po-ctrl (hash-table-set! ctrl-dep dst src))
                                 (_ #f))))
                       (_ #f)))
              edges)


    ;; extract events
    (let* ((process-id (let ((pid-count 0))
                         (lambda ()
                           (let ((cur pid-count))
                             (set! pid-count (+ 1 cur))
                             cur))))

           (evs (sort events (lambda (l r) (> (list-ref l 3) (list-ref r 3)))))
           (uevs (map (lambda (ev) (apply event (cdr ev)))
                      (unique (map cdr evs))))

           ; split events in per-thread group, sort events within threads
           (events-per-thread
            (map (lambda (events)
                   (sort events
                         (lambda (l r)
                           (let ((eid-l (event-eid l)) (eid-r (event-eid r)))
                             (and (hash-table-exists? po-map eid-l)
                                  (= eid-r (hash-table-ref po-map eid-l)))))))
                 (split-per-process uevs))))

      ; litmus structure
      `(litmus
        (header ,(string-append "C \"" name "\""))

        (comments)

        (initialization
         ,@(let* ((ma (apply max (append
                                  (map event-wval uevs)
                                  (map event-rval uevs)
                                  (map event-tid uevs)
                                  (map event-addr uevs))))
                  (addrs (map (lambda (i)
                                (string-append "addr" (number->string i)))
                              (seq ma))))
             (list `(declare-array "priv" ,ma)
                   `(declare-array "shrd" ,ma))))

        ,@(let* ((processes events-per-thread))
            (map (lambda (p)
                   `(process (id ,(process-id))
                             (signature (priv shrd))
                             ,@(map resolve-deps p)))
                 processes))

        ; created the events of each thread, now let's create the exists
        (exists
         ,@(let* ((rf-evs ; select only events that are dst of rf edges
                   (filter (lambda (ev)
                             (member (event-eid ev) rf-dst))
                           uevs)))

             ; create assertions for each of them based on priv memory
             (map (lambda (ev)
                    `(exists= '(deref/array priv ,(event-eid ev))
                              ',(value-name (event-rval ev))))
                  rf-evs)))))))

; ------------------------------------------------------------------------------
; syntax
; ------------------------------------------------------------------------------
(define-syntax declare-array
  (syntax-rules ()
    ((declare-array NAME LEN)
     (let* ((vals (map value-name (seq LEN)))
            (vals (string-join vals ",")))
       (string-append "\tint " NAME
                      "[" (number->string LEN) "] = {" vals "};")))))

(define-syntax ref
  (syntax-rules ()
    ((ref VAR IDX)
     (begin
       (string-append (symbol->string 'VAR) " + "
                      (number->string IDX))))))

(define-syntax deref/array
  (syntax-rules ()
    ((deref/array VAR IDX)
     (begin
       (string-append (symbol->string 'VAR) "["
                      (number->string IDX) "]")))))

(define-syntax deref
  (syntax-rules ()
    ((deref VAR IDX)
     (string-append "*(" (ref VAR IDX)  ")"))
    ((deref STR)
     (string-append "*(" STR ")"))))

(define (eval-rval ev)
  (let ((rval (eval (event-rval ev))))
    (if (number? rval)
        (value-name rval)
        rval)))

(define (eval-wval ev)
  (let ((wval (eval (event-wval ev))))
    (if (number? wval)
        (value-name wval)
        wval)))

(define (eval-mo mo)
  (match mo
         ('SC "memory_order_seq_cst")
         ('REL "memory_order_release")
         ('ACQ "memory_order_acquire")
         ('RLX "memory_order_relaxed")
         (_ (error "unexpected memory order" mo))))

(define (pevent ev)
  (match (cons (event-op ev) (event-mark ev))
         ('(R . Plain)
          (string-append
           (deref priv (event-eid ev)) " = "
           (deref (eval (event-addr ev)))))

         (`(R . ,mo)
          (string-append
           (deref priv (event-eid ev)) " = "
           "atomic_load_explicit("
           (eval (event-addr ev)) ", "
           (eval-mo mo) ")"))

         ('(W . Plain)
          (string-append
           (eval (event-addr ev)) " = "
           (eval-wval ev)))

         (`(W . ,mo)
          (string-append
           "atomic_store_explicit("
           (eval (event-addr ev)) ", "
           (eval-wval ev) ", "
           (eval-mo mo) ")"))

         (`(XCHG . ,mo)
          (string-append
           (deref priv (event-eid ev)) " = "
           "atomic_exchange_explicit("
           (eval (event-addr ev)) ", "
           (eval-wval ev) ", "
           (eval-mo mo) ")"))

         (_ (error "unexpected event"
                   (cons (event-op ev) (event-mark ev))))))

(define (string-line ev . xs)
  (apply string-append
         #;(string-append "E" (number->string (event-eid ev)) ":")
         "\t" xs))

(define-syntax code
  (syntax-rules ()
    ((code #f EVENT)
     (string-line EVENT (pevent EVENT) ";"))
    ((code (onlyif CND) EVENT)
     (string-line EVENT "if (" CND ") " (pevent EVENT) ";"))))

(define-syntax process
  (syntax-rules ()
    ((process (id ID) SIG CODE ...)
     (begin
       (display "P")
       (display (number->string ID))
       (display " (int *priv, int *shrd) ")
       (print "{")
       (print CODE) ...
       (print "}")
       (newline)))))

(define-syntax initialization
  (syntax-rules ()
    ((initialization DECL ...)
     (begin
       (print "{")
       (print DECL) ...
       (print "}")
       (newline)))))

(define (comments . xs)
  (when (not (null? xs))
    (print "/*")
    (for-each print xs)
    (print "*/")
    (newline)))


(define-syntax header
  (syntax-rules ()
    ((header txt)
     (begin (print txt) (newline)))))

(define-syntax exists=
  (syntax-rules ()
    ((exists= A B)
     (print "\t/\\ " (eval A) " == " (eval B)))))

(define-syntax exists
  (syntax-rules ()
    ((exists PRED ...)
     (begin
       (print "exists (1==1")
       PRED ...
       (print ")")
       (newline)))))

(define-syntax litmus
  (syntax-rules ()
    ((litmus ID COMMENTS INIT PROC ... EXISTS)
     (begin ID COMMENTS INIT PROC ... EXISTS))))


(define (run3 x)
  (let ((litmus (create-litmus x)))
    ;(pretty-print litmus)
    (eval litmus)))

; ------------------------------------------------------------------------------
; end
; ------------------------------------------------------------------------------
(start-command main)
