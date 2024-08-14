#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
(import (scheme base)
        (srfi 1)
        (srfi 113)
        ;(srfi 95)
        (kittens match)
        (kittens command))

;(import (scheme small)
 ;       (srfi 193)
  ;      (srfi 166)
   ;     (srfi 113)
    ;    (srfi 128)
     ;   (srfi 1)
      ;  (srfi 95)
       ; (chibi match))

(cond-expand
  (chicken #f)
  (else (import (srfi 113) ; sets
                (srfi 95) ; sort
                (only (srfi 128) symbol-hash))))

(define (usage)
  (print "<kittens> <model file> <cycle length>"))
 

(define-syntax die-unless
  (syntax-rules ()
    ((_ cnd msg)
     (unless cnd
       (print "<kittens> <model file> <cycle length>")
       (newline)
       (error 'argument-error msg 'cnd)))))


(define-record-type 
  event-record 
  (event uid eid tid po co addr val op) 
  event? 
  (uid event-uid) (eid event-eid) (tid event-tid) (po event-po) (co event-co) (addr event-addr) (val event-val) (op event-op))

(define (extract-event-records expr)
  (match expr
    (('model . defs) 
      (apply append (map extract-event-records defs)))
    (('define-fun _ _ 'Event ev) 
      (extract-event-records ev))
    (('mk-event uid eid tid po co adr val op) 
      (list (event uid eid tid po co adr val op)))
    (else '())))

(define (but-last xs) (reverse (cdr (reverse xs))))

(define (get-tids event-records) 
  (let ((tids (set equal? symbol-hash 1024)))
    (begin
    (for-each (lambda (x) (set-adjoin! tids (event-tid x))) event-records)
    ;(display (set->list tids))
    
    (filter (lambda (x) (
	    if (number? x) (and (>= x 0) (< x 50)) #f
	)) (set->list tids)))))

    ;(cdr (but-last (set->list tids))))))

(define (records-per-tid event-records tids)
  (map (lambda (tid) 
                     (filter (lambda (event-record) (eq? (event-tid event-record) tid)) event-records)) tids)
)

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

(define (sort-records event-records)
    (map (lambda (event-record) (sort event-record (lambda (l r) (< (event-po l) (event-po r))))) event-records))

(define (generate-thread-signature event-records-per-tid tid-list)
  (apply string-append `(
    "P"
    ,(get-t-number (event-tid (car event-records-per-tid)) tid-list)
    " ("
    ,@(map (lambda (event) (string-append "volatile int* " (number-to-alphabet-string (event-addr event)) ", ")) (but-last event-records-per-tid))
    ,(string-append "volatile int* " (number-to-alphabet-string (event-addr (car (reverse event-records-per-tid)))))
    ")"
    ))
)

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
                  count (lambda (ev) (and (eq? (event-op ev) 'read) (< (event-eid ev) (event-eid event)))) event-records-per-tid
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

(define (generate-thread-code event-records-per-tid tid-list)
  (apply string-append `(
     ,(generate-thread-signature event-records-per-tid tid-list)
     " {\n"
     ,(generate-thread-body event-records-per-tid)
     "}\n")
  )
)

(define (generate-header)
  (apply string-append '(
    "Test Name \n"
    "Some Very Useful Information\n"
    "{}\n\n"
  ))
)

(define (generate-assert-one-tid event-records-per-tid tid-list)
  ;(display event-records-per-tid)
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

(define (generate-litmus-PC event-records tid-list)
  (apply string-append `(
      ,(generate-header)
      ,@(apply append (map (lambda (event-records-per-tid) (list (generate-thread-code event-records-per-tid tid-list) "\n")) event-records))
      ,(generate-assert event-records tid-list)
    ))
)

(define (main args)
  (die-unless (= (length args) 1) "model file missing")

  (let* ((file-content (with-input-from-file (car args)
			(lambda ()
			    ;;
			    (read)
			    ;;
			    (read))))
	(model-from-file (cons 'model file-content)))
    ;(display my-model)
    ;(newline)
    ;(newline)
    ;(display model-from-file)
    ;(newline)
    ;(newline)
    ;(display (extract-event-records model-from-file))
    (let* ((event-records-all (extract-event-records model-from-file))
	   (event-records (filter (lambda (ev) (number? (event-uid ev))) event-records-all))
           (tids (get-tids event-records))
	   (events-per-tid (records-per-tid event-records tids))
	   (records-sorted (sort-records events-per-tid)))
      ;(display tids)
      ;(display events-per-tid)
      (display (generate-litmus-PC records-sorted tids))         

      ;(display records-sorted)
      ;(display tids)
      ;(display (generate-assert records-sorted tids))
      ;(display (generate-assert-one-tid (car records-sorted)))
      

)))

(main (command-args))
