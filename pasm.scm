#!/usr/bin/env -S chibi-scheme -I.

(import (scheme small)
        (srfi 193) ; command-args
        (pasm parser)
        (pasm utils))

(define (print . xs)
  (for-each display xs)
  (newline))

(define-syntax die-unless
  (syntax-rules ()
    ((_ cnd msg)
     (unless cnd
       (print "<pasm> <objdump file> <func name> [<func name> | ...] ")
       (newline)
       (error 'argument-error msg 'cnd)))))

(define (main args)
  (die-unless (not (null? args)) "input file")

  (let* ((fn (car args))
         (funcs (cdr args))
         (port (open-input-file fn))
         (dump (parser port)))
    (print "input file: " fn)
    (print "proc funcs: " funcs)
    ;(print "dump: " dump)
    (die-unless (not (null? funcs)) "function name")
    (let* ((select-foo (lambda (foo) (select-func dump foo)))
           (procs (map select-foo funcs)))
      (for-each print (apply combine-funcs procs))))
  0)
(main (command-args))
