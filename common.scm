; ------------------------------------------------------------------------------
; basic helpers
; ------------------------------------------------------------------------------
(define (unique lst)
  (define (iter lst out)
    (if (null? lst)
        out
        (if (member (car lst) out)
            (iter (cdr lst) out)
            (iter (cdr lst) (cons (car lst) out)))))
  (iter lst '()))

(define (seq n)
  (let loop ((i 0) (lst '()))
    (if (= i n) lst (loop (+ i 1) (cons (- n i 1) lst)))))
; ------------------------------------------------------------------------------
; display
; ------------------------------------------------------------------------------

(define (pretty-print . xs)
  (for-each (lambda (x)
              (show (current-output-port) (pretty x)))
            xs))

(define (print . xs)
  (for-each display xs)
  (newline))


; ------------------------------------------------------------------------------
; command support
; ------------------------------------------------------------------------------

(define-syntax die-unless
  (syntax-rules ()
    ((_ cnd msg)
     (unless cnd
       (usage)
       (error 'argument-error msg 'cnd)))))

(define (parse-or-die parser generator)
  (let ((result (parser (base-generator->results generator))))
    (if (parse-result-successful? result)
        (parse-result-semantic-value result)
        (error 'parse-error "parse error"
               (let ((e (parse-result-error result)))
                 (list 'parse-error
                       (parse-position->string (parse-error-position e))
                       (parse-error-expected e)
                       (parse-error-messages e)))))))


; ------------------------------------------------------------------------------
; command support
; ------------------------------------------------------------------------------

(define-record-type litc-proc
  (create/litc-proc id args lines)
  litc-proc?
  (id litc-proc-id)
  (args litc-proc-args)
  (lines litc-proc-lines))

(define (make-litc-proc p-tokens)
  (create/litc-proc
   (number->string (cdr (cadr p-tokens)))
   (cdr (caddr p-tokens))
   (cdr (cadddr p-tokens))))

(define (litc-proc-name p)
  (string-append "P" (litc-proc-id p)))

(define-record-type litc-record
  (litc preamble vars procs exists)
  litc?
  (preamble litc-preamble)
  (vars litc-vars)
  (procs litc-procs)
  (exists litc-exists))

(define (make-litc tokens)
  (let ((preamble (cdr (car tokens)))
        (vars (cdr (cadr tokens)))
        (procs (cdr (caddr tokens)))
        (exists (cdr (cadddr tokens))))

    (litc
     ; preamble
     preamble

     ; vars
     vars

     ; procs
     (map make-litc-proc procs)

     ; exists
     exists
     )))

(define (litc-proc-reads p)
  (let ((pname (litc-proc-name p)))
    (filter (lambda (x) x)
            (map (lambda (line)
                   (match line
                          (('line str) #f)
                          (('local ('decl T V) ('line str) ('rest rst))
                           (list T V (string-append V "_" pname)))))
                 (litc-proc-lines p)))))
