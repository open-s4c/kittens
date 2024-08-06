#!/usr/bin/env -S chibi-scheme -I.

(import (scheme small)
        (scheme write)
        (srfi 193) ; command-args
        (only (srfi 1) filter)
        (chibi match)
        (srfi 166) ; pretty-print
        (prefix (pasm parser) odump/)
        (prefix (defuse parser) defuse/)
        (rebottled packrat)
        (pasm utils))

(include "cat/generator.scm")
(include "common.scm")

(define (usage)
  (print "pasm <litmus/C file> <objdump file>")
  (newline))

(define (vars-of-proc p)
  (let* ((args (litc-proc-args p))
         (args (append args (litc-proc-reads p))))
    (for-each (lambda (arg i)
                (display "  ")
                (print (litc-proc-id p) ":x" (number->string i)
                       " = " (caddr arg) ";"))
              args
              (seq (length args)))))

(define (print-expr expr)
  (match expr
         (('disj a b)
          (display "(")
          (print-expr a)
          (display " /\\ ")
          (print-expr b)
          (display ")"))
         (('equal ('read-var p v) rhs)
          (display (string-append v "_P" (number->string p) " = " rhs)))
         (('equal ('deref-var v) rhs)
          (display (string-append v " = " rhs)))))

(define (main args)
  (die-unless (not (null? args)) "litmus/C file")
  (die-unless (not (null? (cdr args))) "objdump file")

  (let* ((cfn (car args))
         (dfn (cadr args))
         (cgen (file-generator cfn))
         (dport (open-input-file dfn))
         (litc (make-litc (parse-or-die defuse/parser cgen)))
         (dump (odump/parser dport)))

    ;(print "litmus/C file: " cfn)
    ;(print "objdump file: " dfn)
    (when #f
      (newline)
      (print "## litc: \n" litc)
      (newline)
      (pretty-print "## dump: \n" dump)
      (newline)
      (exit))

    (print "AArch64 " cfn)
    (print "{")
    (for-each vars-of-proc (litc-procs litc))
    (print "}")
    (newline)

    (let* ((select-foo (lambda (foo) (select-func dump foo)))
           (procs (map select-foo (map litc-proc-name (litc-procs litc))))
           (lines (apply combine-funcs procs)))
      (for-each print lines))

    (newline)
    (display "exists ")
    (print-expr (litc-exists litc))
    (newline))
  0)
(main (command-args))
