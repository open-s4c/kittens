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
                          (('local ('decl T V rhs) ('line str))
                           (list T V (string-append V "_" pname)))))
                 (litc-proc-lines p)))))
