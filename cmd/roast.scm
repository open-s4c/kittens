#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme base)
        (scheme file)
        (scheme read)
        (srfi 1)
        (srfi 95) ; sort
        (kittens match)
        (kittens utils)
        (kittens command))

(define type "a")

(define size_flag #t)

(define size (if size_flag "int" "long"))

(define explicit-init-events #t)

(define in-branch #f)

(define (get-in-branch)
  (if in-branch
      (begin
        (set! in-branch #f)
        #t)
      #f))

(define (usage)
  (print "roast <z3 model>"))

(define-record-type
  event-record
  (event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg obs ass chain)
  event?
  (uid event-uid)
  (eid event-eid)
  (tid event-tid)
  (po event-po)
  (co event-co)
  (addr event-addr)
  (val-r event-val-r)
  (val-w event-val-w)
  (val-e event-val-e)
  (val-d event-val-d)
  (op event-op)
  (rmw-type event-rmw-type)
  (marker1 event-marker1)
  (marker2 event-marker2)
  (arg event-arg)
  (obs event-obs)
  (ass event-ass)
  (chain event-chain))

(define (extract-event-records expr)
  (match expr
         (('model . defs)
          (apply append (map extract-event-records defs)))
         (('define-fun _ _ 'Event ev)
          (extract-event-records ev))
         (('mk-event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg obs ass chain)
          (list (event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg obs ass chain)))
         (else '())))

(define (get-test-name expr)
  (match expr
         (('model . defs)
          (apply string-append (map get-test-name defs)))
         (('define-fun _ _ 'String str)
          str)
         (else "")))

(define (count predicate collection)
  (let loop ((coll collection) (cnt 0))
    (cond
      ((null? coll) cnt)
      ((predicate (car coll))
       (loop (cdr coll) (+ 1 cnt)))
      (else
       (loop (cdr coll) cnt)))))

(define (but-last xs) (reverse (cdr (reverse xs))))

(define (get-tids event-records)
  (unique (map (lambda (x) (event-tid x)) event-records)))

(define (get-write-addresses event-writes)
  (unique (map (lambda (x) (event-addr x)) event-writes)))

(define (records-per-tid event-records tids)
  (map (lambda (tid)
         (filter (lambda (event-record)
                   (eq? (event-tid event-record) tid))
                 event-records))
       tids))

(define (get-writes-per-addr write-events write-addresses)
  (map (lambda (address)
         (filter (lambda (event-record)
                   (eq? (event-addr event-record) address))
                 write-events))
       write-addresses))

(define (get-t-number tid tid-list)
  (number->string (count (lambda (t) (< t tid)) tid-list)))

(define (get-read-t-number event event-records-per-tid)
  (number->string (
                   count (lambda (ev) (and (or (eq? (event-op ev) 'read-modify-write) (eq? (event-op
                                                                                            ev) 'read)) (< (event-po ev) (event-po event)))) event-records-per-tid)))

(define (get-temp-t-number event event-records-per-tid)
  (number->string (
                   count (lambda (ev) (and (eq? (event-rmw-type ev) 'CAS) (< (event-po ev) (event-po event)))) event-records-per-tid)))

(define (get-var-name addr)
  (string-append "v" (number->string addr)))

(define (get-init-var-name addr)
  (string-append "addr" (number->string addr)))

(define (sort-records event-records-all-groups criteria)
  (map (lambda (event-records-one-group) (sort event-records-one-group (lambda (l r) (< (criteria l) (criteria r))))) event-records-all-groups))

(define (prepare-observed-events to-observe)
  (map (lambda (ev) (event (event-uid ev)
                           (event-eid ev)
                           (event-addr ev); tid
                           (event-co ev) ; in observer thread programme order is same as coherence order
                           (event-co ev)
                           (event-addr ev)
                           (event-val-w ev) ; make the read value be the written value
                           (event-val-w ev)
                           (event-val-e ev)
                           (event-val-d ev)
                           'read
                           (event-rmw-type ev)
                           'SC
                           'Plain
                           'false
                           (event-obs ev)
                           (event-ass ev)
                           (event-chain ev)
                           )) to-observe))

(define (get-event-type event)
  (match (event-marker1 event)
         ('Plain (string-append "volatile " size))
         (else (string-append "atomic_" size))))

(define (get-mem-order event number)
  (let ((getter (if (= number 1) event-marker1 event-marker2)))
    (match (getter event)
           ('RLX "memory_order_relaxed")
           ('REL "memory_order_release")
           ('SC "memory_order_seq_cst")
           ('REL-ACQ "memory_order_acq_rel")
           ('ACQ "memory_order_acquire")
           (else "LALALALLA"))))

(define (get-read-loc event event-records-per-tid)
  (if (or (eq? (event-arg event) 'addr)
          (eq? (event-arg event) 'ctrl))
      (string-append
       (if (eq? 'Plain (event-marker1 event))
           ;(string-append "(" size " *)r")
           (string-append "r")
           (string-append "(atomic_" size " *)r")
           )
       (number->string (- (string->number (get-read-t-number event event-records-per-tid)) 1)))
      (string-append
       (if (eq? 'Plain (event-marker1 event))
           ""
           (string-append "(atomic_" size " *)")
           )
       (get-var-name (event-addr event)))))

(define (get-write-val event event-records-per-tid)
  (if (and (eq? (event-op event) 'write)
           (eq? (event-chain event) 'start-chain))
      (string-append
       "v"
       (number->string (event-val-w event)))
      (if (or (eq? (event-arg event) 'data)
              (eq? (event-arg event) 'ctrl))
          (string-append
           "r"
           (number->string (- (string->number (get-read-t-number event event-records-per-tid)) 1)))
          (number->string (event-val-w event))
          )
      )
  )

(define (print-event-read event event-records-per-tid)
  (match (event-marker1 event)
         ('Plain
          (string-append
           (string-append size " r")
           (get-read-t-number event event-records-per-tid)
           " = *"
           (get-read-loc event event-records-per-tid)
           ";"
           ))

         (else
          (string-append
           (string-append size " r")
           (get-read-t-number event event-records-per-tid)
           " = atomic_load_explicit("
           (get-read-loc event event-records-per-tid)
           ", "
           (get-mem-order event 1)
           ");"
           ))))

(define (print-event-write event event-records-per-tid)
  (match (event-marker1 event)
         ('Plain
          (string-append
           "*"
           (get-read-loc event event-records-per-tid)
           " = "
           (get-write-val event event-records-per-tid)
           ";"
           ))
         (else
          (string-append
           "atomic_store_explicit("
           (get-read-loc event event-records-per-tid)
           ", "
           (get-write-val event event-records-per-tid)
           ", "
           (get-mem-order event 1)
           ");"
           ))))


(define (print-event-RMW event event-records-per-tid)
  (match (event-rmw-type event)
         ('XCHG
          (print-event-XCHG event event-records-per-tid))
         ('FAA
          (print-event-FAA event event-records-per-tid))
         ('CAS
          (print-event-CAS event event-records-per-tid))))

(define (print-event-XCHG event event-records-per-tid)
  (string-append
   (string-append size " r")
   (get-read-t-number event event-records-per-tid)
   " = atomic_exchange_explicit("
   (get-read-loc event event-records-per-tid)
   ", "
   (get-write-val event event-records-per-tid)
   ","
   (get-mem-order event 1)
   ");"
   ))

(define (print-event-FAA event event-records-per-tid)
  "")

(define (print-event-CAS event event-records-per-tid)
  (string-append
   (string-append size " temp_e")
   (get-temp-t-number event event-records-per-tid)
   " = "
   ;(if (eq? 'addr (event-arg event))
   ;(string-append "*" (get-read-loc event event-records-per-tid))
   (number->string (event-val-e event))
   ;)
   ";\n    "
   "atomic_compare_exchange_strong_explicit("
   (get-read-loc event event-records-per-tid)  ; obj
   ", &temp_e"				       ; expected
   (get-temp-t-number event event-records-per-tid)
   ;(get-write-val event event-records-per-tid)
   ", "
   (if (eq? 'data (event-arg event))
       (get-write-val event event-records-per-tid)
       (number->string (event-val-d event))        ; desired
       )
   ", "
   (get-mem-order event 1)
   ", "
   (get-mem-order event 2)
   ");\n    "
   (string-append size " r")
   (get-read-t-number event event-records-per-tid)
   " = temp_e"
   (get-temp-t-number event event-records-per-tid)
   ";"
   ))

(define (print-event-branch event event-records-per-tid)
  (begin
    (set! in-branch #t)
    (string-append
     "if ("
     (get-read-loc event event-records-per-tid)
     ") {"
     )))

(define (print-event-fence event)
  (string-append
   "atomic_thread_fence("
   (get-mem-order event 1)
   ");"
   ))

(define (print-event event event-records-per-tid)
  (let ((branch (get-in-branch)))
    (apply string-append `(
                           ,(if branch "    " "")
                           "    "
                           ,(match (event-op event)
                                   ('read
                                    (print-event-read event event-records-per-tid))
                                   ('write
                                    (print-event-write event event-records-per-tid))
                                   ('fence
                                    (print-event-fence event))
                                   ('branch
                                    (print-event-branch event event-records-per-tid))
                                   (else
                                    (print-event-RMW event event-records-per-tid)))
                           ,(if branch "\n    }" "")
                           ))))

(define (generate-thread-body event-records-per-tid)
  (apply string-append
         `(,@(apply append (map (lambda (event)
                                  (list (print-event event event-records-per-tid) "\n"))
                                event-records-per-tid)))))

(define (generate-thread-code events-one-tid tid-list)
  (apply string-append `(
                         ,(generate-thread-signature events-one-tid tid-list)
                         " {\n"
                         ,(generate-thread-body events-one-tid)
                         "}\n")))

(define (generate-header name events)
  (apply string-append `(
                         "C "
                         ;"\""
                         ,name
                         ;"\""
                         "\n"
                         "Some Very Useful Information\n"
                         "{\n"
                         ,(generate-preamble events)
                         "}\n\n"
                         )))

(define (process-events events)
  (let* ((unique-addrs (unique (map event-addr events)))
         (events-by-addr (map (lambda (addr)
                                (filter (lambda (ev) (eq? (event-addr ev) addr)) events))
                              unique-addrs))
         (filtered-events (map (lambda (event-list)
                                 (let ((filtered (filter (lambda (ev)
                                                           (and (eq? (event-op ev) 'read)
                                                                (eq? (event-chain ev) 'start-chain)))
                                                         event-list)))
                                   (if (null? filtered) event-list filtered)))
                               events-by-addr)))
    (map car filtered-events)))

(define (generate-preamble events)
  ;(let ((addr-normal (unique (filter (lambda (ev) (or (eq? (event-op ev) 'write) (not (eq? (event-chain ev) 'start-chain)))) events)))
  ;      (addr-fancy (unique (filter (lambda (ev) (and (eq? (event-op ev) 'read) (eq? (event-chain ev) 'start-chain))) events))))

  (let ((events (process-events events)))
    ;; here check if read/write as well as start-chain.
    (apply string-append (map (lambda (ev) (string-append
                                            "  "
                                            (get-var-name (event-addr ev))
                                            "="
                                            (if (and (eq? (event-op ev) 'read)
                                                     (eq? (event-chain ev) 'start-chain))
                                                (string-append
                                                 "v"
                                                 (number->string (event-val-r ev)))
                                                (get-init-var-name (event-addr ev)))
                                            ";\n"
                                            )) events))
    ))
(define (get-event-type-a) (string-append size "* "))

(define (generate-thread-signature event-records-per-tid tid-list)
  (let ((addresses (unique (map (lambda (ev) (event-addr ev)) event-records-per-tid)))
        )
    (apply string-append `(
                           "P"
                           ,(get-t-number (event-tid (car event-records-per-tid)) tid-list)
                           " ("
                           ,@(map (lambda (addr) (string-append (get-event-type-a)
                                                                ;"* "
                                                                (get-var-name addr) ", ")) (but-last addresses))
                           ,(string-append
                             (get-event-type-a)
                             ;"* "
                             (get-var-name (car (reverse addresses))))
                           ")"
                           ))))

(define (generate-assert-one-tid all-events-to-display-one-tid asserted-events-one-tid all-events-to-display-tid-list)
  `(
    ,@(map (lambda (asserted-event) (
                                     apply string-append `(
                                                           ,(get-t-number (event-tid asserted-event) all-events-to-display-tid-list)
                                                           ":r"
                                                           ,(get-read-t-number
                                                             asserted-event
                                                             all-events-to-display-one-tid)
                                                           "="
                                                           ,(if (eq? (event-chain asserted-event) 'middle-chain) "v" "")
                                                           ,(number->string
                                                             (event-val-r asserted-event))
                                                           )
                                     )) asserted-events-one-tid))
  )

(define (generate-assert all-events-to-display-per-tid all-events-to-display-tid-list asserted-events-per-tid asserted-events-tid-list)
  (let* ((all-reads-per-tid (map (lambda (asserted-events-one-tid)
                                   (generate-assert-one-tid
                                    (car (filter (lambda
                                                   (all-events-to-display-one-tid)
                                                   (eq? (event-tid (car
                                                                    all-events-to-display-one-tid))
                                                        (event-tid (car
                                                                    asserted-events-one-tid))))
                                                 all-events-to-display-per-tid))
                                    asserted-events-one-tid
                                    all-events-to-display-tid-list)) asserted-events-per-tid)
                            )

         (all-reads (apply append all-reads-per-tid)))
    (apply string-append `(
                           "exists ("
                           ,(if (> (length all-reads) 1) (apply string-append (map (lambda (read) (string-append read "/\\")) (but-last all-reads))) "")
                           ,(if (> (length all-reads) 0) (car (reverse all-reads)) "")
                           ")\n"
                           ))))

(define (generate-litmus-PC name all-events-to-display-per-tid all-events-to-display-tid-list asserted-events-per-tid asserted-events-tid-list)
  (apply string-append `(
                         ,(generate-header name (unique (apply append all-events-to-display-per-tid)))
                         ,@(apply append (map (lambda (events-one-tid)  (list (generate-thread-code events-one-tid all-events-to-display-tid-list) "\n")) all-events-to-display-per-tid))
                         ,(generate-assert all-events-to-display-per-tid
                                           all-events-to-display-tid-list
                                           asserted-events-per-tid
                                           asserted-events-tid-list)
                         )))

(define (unique-events event-records-all)
  (let* ((eids (unique (map event-eid event-records-all)))
         (unique-events (map (lambda (eid) (car (filter (lambda (ev) (eq? (event-eid ev) eid)) event-records-all))) eids)))
    unique-events))

(define (main args)
  (die-unless (= (length args) 1) "model file missing")

  (let* ((fn (car args))
         (file-content (if (equal? "-" fn)
                           (begin (read) (read))
                           (with-input-from-file fn
                            (lambda ()
                              ;;
                              (read)
                              ;;
                              (read)))))
         (model-from-file (cons 'model file-content)))

    ;; What we need
    ;; 1.  All events separated on threads
    ;; 1.5 List of threads
    ;; 2.  All observed events separated on address (thread)
    ;; 2.5 List of addresses of observedd
    ;; 3.  List of assertions per tid
    ;; 3.5 List of tid for assertions

    (let* ((name (get-test-name model-from-file))
           (event-records-from-file (extract-event-records model-from-file))
           (event-records-all (if explicit-init-events
                                  event-records-from-file
                                  (filter (lambda (ev) (number? (event-uid ev)))
                                          event-records-from-file)))
           (cycle-events (unique-events event-records-all))
           ; No duplicate events in event-records

           (cycle-events-tid-list (get-tids cycle-events))
           (cycle-events-per-tid (records-per-tid cycle-events cycle-events-tid-list))
           ; (cycle-events-per-tid-sorted (sort-records events-per-tid event-po))

           (observed-events (filter (lambda (ev) (eq? 'true (event-obs ev))) cycle-events))
           (observed-events (prepare-observed-events observed-events))

           (observed-events-tid-list (get-tids observed-events))
           (observed-events-per-tid (records-per-tid observed-events observed-events-tid-list))

           (all-events-to-display-per-tid (append cycle-events-per-tid observed-events-per-tid))
           (all-events-to-display-per-tid (sort-records all-events-to-display-per-tid event-po))
           (all-events-to-display-per-tid (sort all-events-to-display-per-tid (lambda (l r) (< (event-tid (car l)) (event-tid (car r))))))
           (all-events-tid-list (append cycle-events-tid-list observed-events-tid-list))

           (asserted-events-from-rf (filter (lambda (ev) (eq? 'true (event-ass ev))) cycle-events))
           (asserted-events (append asserted-events-from-rf observed-events))
           (asserted-events-tid-list (get-tids asserted-events))
           (asserted-events-per-tid (records-per-tid asserted-events asserted-events-tid-list)))
      (display (generate-litmus-PC
                name
                all-events-to-display-per-tid
                all-events-tid-list
                asserted-events-per-tid
                asserted-events-tid-list)))))
(start-command main)
