(import (scheme small)
        (srfi 193)
	(srfi 166)
	(srfi 113)
	(srfi 128)
	(srfi 1)
	(srfi 95)
	(chibi match))


(define-syntax die-unless
  (syntax-rules ()
    ((_ cnd msg)
     (unless cnd
       (print "<kittens> <model file> <cycle length>")
       (newline)
       (error 'argument-error msg 'cnd)))))


(define-record-type 
  event-record 
  (event eid tid addr val op) 
  event? 
  (eid event-eid) (tid event-tid) (addr event-addr) (val event-val) (op event-op))

(define (extract-event-records expr)
  (match expr
    (('model . defs) 
      (map extract-event-records defs))
    (('define-fun _ _ _ ev) (extract-event-records ev))
    (('mk-event eid tid adr val op) (event eid tid adr val op))
    (else (list expr))))

(define (but-last xs) (reverse (cdr (reverse xs))))

(define (get-tids event-records) 
  (let ((tids (set equal? symbol-hash 1024)))
    (begin
    (for-each (lambda (x) (set-adjoin! tids (event-tid x))) event-records)
    (cdr (but-last (set->list tids))))))

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
    (map (lambda (event-record) (sort event-record (lambda (l r) (< (event-eid l) (event-eid r))))) event-records))

(define (generate-signature event-records)
  (apply string-append `(
    "P"
    ,(number->string (event-tid (car event-records)))
    " ("
    ,@(map (lambda (event) (string-append "volatile int* " (number-to-alphabet-string (event-addr event)) ", ")) (but-last event-records))
    ,(string-append "volatile int* " (number-to-alphabet-string (event-addr (car (reverse event-records)))))
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

(define (get-read-t-number event event-records) 
  (apply string-append `(
	       ,(number->string (
                  count (lambda (ev) (and (eq? (event-op ev) 'read) (< (event-eid ev) (event-eid event)))) event-records
 )))))

(define (print-event event event-records)
  (apply string-append `(
    "    "
    ,(if (eq? (event-op event) 'read) 
       (apply string-append `(
         ,(string-append "int r" 
	     (
                get-read-t-number event event-records
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

(define (generate-body event-records)
  (apply string-append `( 
     ,@(apply append (map (lambda (event) (list (print-event event event-records) "\n")) event-records))
  ))
)

(define (generate-thread-code event-records)
  (apply string-append `(
     ,(generate-signature event-records)
     " {\n"
     ,(generate-body event-records)
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

(define (generate-assert event-records)
  (apply string-append `(
    "exists("
    "..."
    ")\n"
  ))
)

(define (generate-litmus-PC event-records)
  (apply string-append `(
      ,(generate-header)
      ,@(apply append (map (lambda (event-records-per-tid) (list (generate-thread-code event-records-per-tid) "\n")) event-records))
      ,(generate-assert event-records)
    ))
)

(define (main args)
  (let ((my-model
		  '(model
		     (define-fun ev1 () Event
		       (mk-event 1 0 102 12 read))
		     (define-fun ev3 () Event
		       (mk-event 3 0 104 16 read))
		     (define-fun ev2 () Event
		       (mk-event 2 0 102 13 write))
		     (define-fun ev0 () Event
		       (mk-event 0 2 103 13 write))
		     (define-fun ev2 () Event
		       (mk-event 5 2 104 13 read))
		     (define-fun ev0 () Event
		       (mk-event 4 2 103 15 write)))))

    (let* ((event-records (extract-event-records my-model))
           (tids (get-tids event-records))
	   (events-per-tid (records-per-tid event-records tids))
	   (records-sorted (sort-records events-per-tid)))
               
      (display (generate-litmus-PC records-sorted))         

)))

(main (command-args))
