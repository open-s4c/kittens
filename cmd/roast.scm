#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
(import (scheme base)
        (scheme file)
        (scheme read)
        (srfi 1)
        (srfi 95) ; sort
        (kittens match)
        (kittens utils)
        (kittens command))

(define type "a")

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
  (event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg)
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
  (arg event-arg))

(define (extract-event-records expr)
  (match expr
         (('model . defs)
          (apply append (map extract-event-records defs)))
         (('define-fun _ _ 'Event ev)
          (extract-event-records ev))
         (('mk-event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg) 
          (list (event uid eid tid po co addr val-r val-w val-e val-d op rmw-type marker1 marker2 arg)))
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

(define (get-var-name addr)
  (string-append "v" (number->string addr)))

(define (sort-records event-records criteria)
  (map (lambda (event-record) (sort event-record (lambda (l r) (< (criteria l) (criteria r))))) event-records))

(define (get-event-type event)
  (match (event-marker1 event)
         ('Plain "volatile long")
         (else "atomic_long")))

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
       "r"
       (number->string (- (string->number (get-read-t-number event event-records-per-tid)) 1)))
      (string-append
	(if (eq? 'Plain (event-marker1 event))
	 ""
	 "(atomic_long *)"
	)
       (get-var-name (event-addr event)))))

(define (get-store-val event event-records-per-tid)
  (if (or (eq? (event-arg event) 'data)
          (eq? (event-arg event) 'ctrl))
      (string-append 
       "r"
       (number->string (- (string->number (get-read-t-number event event-records-per-tid)) 1)))
      (number->string (event-val-w event))
      )
  )

(define (print-event-read event event-records-per-tid)
  (match (event-marker1 event)
         ('Plain
          (string-append 
           "long r"
           (get-read-t-number event event-records-per-tid)
           " = *"
           (get-read-loc event event-records-per-tid) 
           ";"
           ))

         (else
          (string-append
           "long r"
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
           (get-store-val event event-records-per-tid)
           ";"
           ))
         (else
          (string-append 
           "atomic_store_explicit("
           (get-read-loc event event-records-per-tid) 
           ", "
           (get-store-val event event-records-per-tid)
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
   "long r"
   (get-read-t-number event event-records-per-tid)
   " = atomic_exchange_explicit("
   (get-read-loc event event-records-per-tid) 
   ", "
   (get-store-val event event-records-per-tid)
   "," 
   (get-mem-order event 1)
   ");"
   ))

(define (print-event-FAA event event-records-per-tid)
  "")

(define (print-event-CAS event event-records-per-tid)
  (string-append 
   "long temp_e = "
   (number->string (event-val-e event))
   ";\n    "
   "atomic_compare_exchange_strong_explicit("
   (get-read-loc event event-records-per-tid) 
   ", &temp_e"
   ;(get-store-val event event-records-per-tid)
   ", " 
   (number->string (event-val-d event))
   ", "
   (get-mem-order event 1)
   ", "
   (get-mem-order event 2)
   ");\n    "
   "long r"
   (get-read-t-number event event-records-per-tid)
   " = temp_e;"
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
                         "{"
                         ,(generate-preamble events)
                         "}\n\n"
                         )))

(define (generate-preamble events)
  (apply string-append (map (lambda (addr) (string-append 
                                          ;"*(long *)"
                                          (get-var-name addr)
                                          ;"=1"
                                          ";"
                                          )) (unique (map event-addr events))))) 

(define (get-event-type-a) "long* ")

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

(define (generate-assert-one-tid event-records-per-tid tid-list)
  `(
    ,@(map (lambda (event) (
                            apply string-append `(
                                                  ,(get-t-number (event-tid event) tid-list)
                                                  ":r"
                                                  ,(get-read-t-number event event-records-per-tid)
                                                  "="
                                                  ,(number->string (event-val-r event)))
                            )) (filter (lambda (event) (or (eq? (event-op event) 'read) 
                                                           (eq? (event-op event)
                                                                'read-modify-write))) event-records-per-tid))))

(define (generate-assert event-records tid-list)
  (let ((all-reads (apply append (map (lambda (event-records-per-tid) (generate-assert-one-tid event-records-per-tid tid-list)) event-records))))
    (apply string-append `(
                           "exists ("
                           ,(apply string-append `(
                                                   ,@(map (lambda (read) (string-append read " /\\ ")) (but-last all-reads))
                                                   ,(car (reverse all-reads))))
                           ")\n"
                           ))))

(define (generate-litmus-PC name events-per-tid-sorted tid-list writes-per-addr-sorted addr-list)
  (apply string-append `(
                         ,(generate-header name (unique (apply append events-per-tid-sorted)))
                         ,@(apply append (map (lambda (events-one-tid)  (list (generate-thread-code   events-one-tid tid-list)  "\n")) events-per-tid-sorted))
                         ,(generate-assert events-per-tid-sorted tid-list)
                         )))

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


    (let* ((name (get-test-name model-from-file))
           (event-records-from-file (extract-event-records model-from-file))
           (event-records-all (if explicit-init-events
                                  event-records-from-file
                                  (filter (lambda (ev) (number? (event-uid ev)))
                                          event-records-from-file)))
           (eids (unique (map event-eid event-records-all)))
           (event-records (map (lambda (eid) (car (filter (lambda (ev) (eq? (event-eid ev) eid)) event-records-all))) eids))

           (tid-list (get-tids event-records))
           (events-per-tid (records-per-tid event-records tid-list))
           (events-per-tid-sorted (sort-records events-per-tid event-po))

           (event-writes (filter (lambda (ev) (or (eq? (event-op ev) 'read-modify-write) (eq? (event-op
                                                                                               ev) 'write))) event-records))
           (addr-list (get-write-addresses event-writes))
           (writes-per-addr (get-writes-per-addr event-writes addr-list))
           (writes-per-addr-sorted (sort-records writes-per-addr event-co))
           (reads-per-addr (map (lambda (events-one-addr)
                                  (map (lambda (ev) (event (event-uid ev)
                                                           (event-eid ev)
                                                           (event-addr ev) ; tid
                                                           (event-co ev) ; in observer thread programme order is same as coherence order
                                                           (event-co ev)
                                                           (event-addr ev)
                                                           (event-val-w ev) ; make the read value be the written value
                                                           (event-val-w ev)
                                                           (event-val-e ev)
                                                           (event-val-d ev)
                                                           'read
                                                           (event-rmw-type ev)
							   `Plain
                                                           'Plain
							   'false
                                                           )) events-one-addr)
                                  )   writes-per-addr-sorted
                                ))
           )

     ; (display (event-uid (car event-records-from-file)))
     ; (display (event-uid (cadr event-records-from-file)))
     ; (display (number? (event-uid (cadr event-records-from-file))))
     ; (display event-records-from-file)
     ; (display (get-test-name model-from-file))
      (display (generate-litmus-PC name
                                   (sort (append events-per-tid-sorted reads-per-addr) (lambda (l r) (< (event-tid (car l)) (event-tid (car r)))))
                                   (append tid-list addr-list)
                                   writes-per-addr-sorted addr-list))

      )))
(start-command main)
