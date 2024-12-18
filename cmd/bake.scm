#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
(import (scheme base)
        (scheme file)
        (scheme read)
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
  ;  (event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg obs ass chain)
  (event eid tid op addr rval wval mark)
  event?
  (eid event-eid)
  (tid event-tid)
  (op event-op)
  (addr event-addr)
  (rval event-rval)
  (wval event-wval)
  (mark event-mark))

(define (extract-event-records expr)
  (match expr
         (('model . defs)
          (apply append (map extract-event-records defs)))
         (('define-fun _ _ 'Event ev)
          (extract-event-records ev))
         (('mk-event . fields)
          (list (apply event fields)))
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

(define (extract-instances type model)
  (filter values (map (lambda (x)
                        (match x
                               (('define-fun name _ T val)
                                (if (eq? T type)
                                    (cons name val)
                                    #f))
                               (_ #f))) model)))

(define (sort-records event-records-all-groups criteria)
  (map (lambda (event-records-one-group)
         (sort event-records-one-group
               (lambda (l r) (< (criteria l) (criteria r))))) event-records-all-groups))

(define (shared-ptr addr)
  (string-append "(shrd + " (number->string addr) ")"))

(define (local-ptr addr)
  (string-append "(priv + " (number->string addr) ")"))

(define (cond-on addr)
  (string-append "if (*" (local-ptr addr) ") "))


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
                       `(deref 'priv ,(hash-table-ref addr-dep eid))
                       `(deref 'shrd ,(event-addr ev))))
             (wval (if (hash-table-exists? data-dep eid)
                       `(deref 'priv ,(hash-table-ref data-dep eid))
                       (event-wval ev)))
             (cnd (if (hash-table-exists? ctrl-dep eid)
                      `(ifthen (deref 'shrd ,(hash-table-ref ctrl-dep eid)))
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
    (let* ((evs (sort events
                      (lambda (l r)
                        (let ((src (list-ref l 2))
                              (dst (list-ref r 2))
                              (stid (list-ref l 3))
                              (dtid (list-ref r 3)))
                          (or (and  (= stid dtid)
                                    (hash-table-exists? po-map src)
                                    (= dst (hash-table-ref po-map src)))
                              (< stid dtid))))))
           (uevs (map (lambda (ev) (apply event (cdr ev)))
                      (unique (map cdr evs))))
           (thr 0)
           (process-id (let ((pid-count 0))
                         (lambda ()
                           (let ((cur pid-count))
                             (set! pid-count (+ 1 cur))
                             cur)))))

      ; litmus structure
      `(litmus
        (header "C")

        (comments
         (print evs)
         (print (map event-tid uevs) (map event-addr uevs)))

        (initialization
         ,@(let* ((ma (apply max (append (map event-tid uevs) (map event-addr uevs))))
                  (addrs (map (lambda (i) (string-append "addr" (number->string i))) (seq ma))))
             (list `(declare-array "priv" ,ma)
                   `(declare-array "shrd" ,ma))))

        ,@(let* ((processes (split-per-process uevs)))
            (map (lambda (p)
                   `(process (id ,(process-id))
                             (signature (priv shrd))
                             ,@(map resolve-deps p)))
                 processes))

        ; created the events of each thread, now let's create the exists
        (exists
         ,(let* ((rf-evs ; select only events that are dst of rf edges
                  (filter (lambda (ev) (member (event-eid ev) rf-dst)) uevs)))

            ; create assertions for each of them based on priv memory
            (map (lambda (ev) `(= (priv ,(event-eid ev)) ,(event-rval ev)))
                 rf-evs)))))))

(define (run3 x)
  (pretty-print (create-litmus x)))


(define (main args)
  (die-unless (= (length args) 1) "model file missing")

  (let* ((fn (car args))
         (file-content (if (equal? "-in" fn)
                           (begin (read) (read))
                           (with-input-from-file fn
                            (lambda () (read) (read))))))
    (run3 file-content))
  0)

(start-command main)
