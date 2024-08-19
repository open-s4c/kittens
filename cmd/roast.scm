#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
(import (scheme base)
        (scheme file)
        (scheme read)
        (srfi 1)
        (srfi 95) ; sort
        (kittens match)
        (kittens utils)
        (kittens command))

(define (usage)
  (print "roast <z3 model>"))

(define-record-type
  event-record
  (event uid eid tid po co addr val op)
  event?
  (uid event-uid)
  (eid event-eid)
  (tid event-tid)
  (po event-po)
  (co event-co)
  (addr event-addr)
  (val event-val)
  (op event-op))

(define (extract-event-records expr)
  (match expr
         (('model . defs)
          (apply append (map extract-event-records defs)))
         (('define-fun _ _ 'Event ev)
          (extract-event-records ev))
         (('mk-event uid eid tid po co addr val op)
          (list (event uid eid tid po co addr val op)))
         (else '())))

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

(define (number-to-alphabet-string n)
  (let loop ((n n) (result ""))
    (let* ((quotient (quotient n 26))
           (remainder (modulo n 26))
           (char (integer->char (+ (char->integer #\a) remainder))))
      (if (= quotient 0)
          (string-append (string char) result)
          (loop (- quotient 1) (string-append (string char) result))))))

(define (enumerate-strings n)
  (let loop ((i 0))
    (when (< i n)
      (display i)
      (display " -> ")
      (display (number-to-alphabet-string i))
      (newline)
      (loop (+ i 1)))))

(define (sort-records event-records criteria)
  (map (lambda (event-record) (sort event-record (lambda (l r) (< (criteria l) (criteria r))))) event-records))

(define (generate-thread-signature event-records-per-tid tid-list)
  (let ((addresses (unique (map (lambda (ev) (event-addr ev)) event-records-per-tid))))
  (apply string-append `(
                         "P"
                         ,(get-t-number (event-tid (car event-records-per-tid)) tid-list)
                         " ("
                         ,@(map (lambda (addr) (string-append "volatile int* "
			 (number-to-alphabet-string addr) ", ")) (but-last addresses))
                         ,(string-append "volatile int* " (number-to-alphabet-string (car (reverse addresses))))
                         ")"
                         ))
  ))

(define (count predicate collection)
  (let loop ((coll collection) (cnt 0))
    (cond
      ((null? coll) cnt)
      ((predicate (car coll))
       (loop (cdr coll) (+ 1 cnt)))
      (else
       (loop (cdr coll) cnt)))))

(define (get-t-number tid tid-list)
  (number->string (count (lambda (t) (< t tid)) tid-list)))

(define (get-read-t-number event event-records-per-tid)
  (number->string (
                   count (lambda (ev) (and (eq? (event-op ev) 'read) (< (event-po ev) (event-po event)))) event-records-per-tid
                   )))

(define (print-event event event-records-per-tid)
  (apply string-append `(
                         "    "
                         ,(if (eq? (event-op event) 'read)
                              (apply string-append `(
                                                     ,(string-append "int r"
                                                                     (
                                                                      get-read-t-number event event-records-per-tid
                                                                      )
                                                                     " = *");
                                                     ,(number-to-alphabet-string (event-addr event))
                                                     ";"
                                                     )
                                     )
                              (apply string-append `(
                                                     "*"
                                                     ,(number-to-alphabet-string (event-addr event))
                                                     " = "
                                                     ,(number->string (event-val event))
                                                     ";"
                                                     )
                                     )
                              )
                         ))
  )

(define (generate-thread-body event-records-per-tid)
  (apply string-append `(
                         ,@(apply append (map (lambda (event) (list (print-event event event-records-per-tid) "\n")) event-records-per-tid))
                         ))
  )

(define (generate-thread-code events-one-tid tid-list)
  (apply string-append `(
                         ,(generate-thread-signature events-one-tid tid-list)
                         " {\n"
                         ,(generate-thread-body events-one-tid)
                         "}\n")
         )
  )

(define (generate-header)
  (apply string-append '(
                         "C Name \n"
                         "Some Very Useful Information\n"
                         "{}\n\n"
                         ))
  )

(define (generate-assert-one-tid event-records-per-tid tid-list)
  `(
    ,@(map (lambda (event) (
                            apply string-append `(
                                                  ,(get-t-number (event-tid event) tid-list)
                                                  ":r"
                                                  ,(get-read-t-number event event-records-per-tid)
                                                  "="
                                                  ,(number->string (event-val event)))
                            )) (filter (lambda (event) (eq? (event-op event) 'read))event-records-per-tid)
           )

    )
  )

(define (generate-assert event-records tid-list)
  (let ((all-reads (apply append (map (lambda (event-records-per-tid) (generate-assert-one-tid event-records-per-tid tid-list)) event-records))))
    (apply string-append `(
                           "exists ("
                           ,(apply string-append `(
                                                   ,@(map (lambda (read) (string-append read " /\\ ")) (but-last all-reads))
                                                   ,(car (reverse all-reads))))
                           ")\n"
                           ))))

(define (generate-litmus-PC events-per-tid-sorted tid-list writes-per-addr-sorted addr-list)
  (apply string-append `(
                         ,(generate-header)
                         ,@(apply append (map (lambda (events-one-tid)  (list (generate-thread-code   events-one-tid tid-list)  "\n")) events-per-tid-sorted))
                         ,(generate-assert events-per-tid-sorted tid-list)
                         ))
  )

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

    (let* ((event-records-all (extract-event-records model-from-file))
           (event-records (filter (lambda (ev) (number? (event-uid ev))) event-records-all))

           (tid-list (get-tids event-records))
           (events-per-tid (records-per-tid event-records tid-list))
           (events-per-tid-sorted (sort-records events-per-tid event-po))

           (event-writes (filter (lambda (ev) (eq? (event-op ev) 'write)) event-records))
           (addr-list (get-write-addresses event-writes))
           (writes-per-addr (get-writes-per-addr event-writes addr-list))
           (writes-per-addr-sorted (sort-records writes-per-addr event-co))
           (reads-per-addr (map (lambda (events-one-addr)
                                  (map (lambda (ev) (event (event-uid ev)
                                                           (event-eid ev)
                                                           (event-addr ev)
                                                           (event-co ev) ; in observer thread programme order is same as coherence order
                                                           (event-co ev)
                                                           (event-addr ev)
                                                           (event-val ev)
                                                           'read)) events-one-addr)
                                  )   writes-per-addr-sorted
                                ))
           )                       
      (display (generate-litmus-PC 
				(sort (append events-per-tid-sorted reads-per-addr) (lambda (l r) (< (event-tid (car l)) (event-tid (car r))))) 
				(append tid-list addr-list) 
				writes-per-addr-sorted addr-list))

      )))
