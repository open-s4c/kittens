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

(define (main args)
  (die-unless (not (null? args)) "litmus/C file")
  (die-unless (not (null? (cdr args))) "objdump file")

  (let* ((cfn (car args))
         (dfn (cadr args))
         (cgen (file-generator cfn))
         (dport (open-input-file dfn))
         (litc (make-litc (parse-or-die defuse/parser cgen)))
         (dump (odump/parser dport)))
    (print "litmus/C file: " cfn)
    (print "objdump file: " dfn)

    (when #t
      (newline)
      (print "## litc: \n" litc)
      (newline)
      (pretty-print "## dump: \n" dump)
      (newline))

    (let* ((select-foo (lambda (foo) (select-func dump foo)))
           (procs (map select-foo (map litc-proc-name (litc-procs litc)))))
      (for-each print (apply combine-funcs procs))))
  0)
(main (command-args))
