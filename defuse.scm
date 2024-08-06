#!/usr/bin/env -S chibi-scheme -I.

(import (scheme small)
        (srfi 193) ; command-args
        (srfi 166) ; formatting
        (srfi 125) ; hash tables
        (only (chibi string) string-join)
        (only (srfi 1) filter)
        (chibi match)
        (rebottled packrat)
        (defuse parser))

(include "common.scm")

(define (usage)
  (print "defuse <c-litmus file>")
  (newline))

(define (print-expr expr)
  (match expr
         (('disj a b)
          (display "(")
          (print-expr a)
          (display " && ")
          (print-expr b)
          (display ")"))
         (('equal ('read-var p v) rhs)
          (display (string-append v "_P" (number->string p) " == " rhs)))
         (('equal ('deref-var v) rhs)
          (display (string-append v " == " rhs)))))

(define (print-proc-lines p)
  (let ((name (litc-proc-name p))
        (lines (litc-proc-lines p)))
    (for-each (lambda (line)
                (print "  "
                       (match line
                              (('line str) str)
                              (('local ('decl T V) ('line str))
                               (string-append
                                str " *" V "_" name " = " V ";")))))
              lines)))

(define (declare-read-var read)
  (let-values (((T V vname) (apply values read)))
    (print T " " vname ";")))

(define (declare-arg-var arg)
  (match arg
         (('volatile T V) (print "volatile " T " " V ";"))
         (('atomic T V) (print "atomic_" T " " V ";"))))

(define (generate-c litc)
  ; print preamble in comment
  (when #t
    (print "/*")
    (for-each print (litc-preamble litc))
    (print "*/"))

  ; add includes
  (newline)
  (print "#include <stdatomic.h>")
  (print "#ifndef RMEM")
  (print "#include <assert.h>")
  (print "#include <pthread.h>")
  (print "#define ensure(COND) assert(COND)")
  (print "#else")
  (print "#define ensure(COND) if (!cond) { asm (\"brk 0xdead\"); }")
  (print "#endif")

  ; define vars if necessary, for now just assert empty
  (when #f
    (print "/*")
    (for-each print (litc-vars litc))
    (print "*/"))

  (let ((procs (litc-procs litc)))
    ; collect all arguments and create variables for the arguments
    (let* ((args (map litc-proc-args procs))   ; take args of each proc
           (args (apply append args)) ; combine all args
           (args (unique args)))      ; remove duplicates

      (newline)
      (print "/* shared variables */")
      (for-each declare-arg-var args))

    ; create a variable for each read
    (newline)
    (print "/* variables to store per-proc reads */")
    (for-each declare-read-var (apply append (map litc-proc-reads procs)))

    ; create one function per proc and use the arguments as global variables
    ; the per-proc-read variables are passed as last arguments
    (newline)
    (print "/* processor functions */")
    (for-each (lambda (p)
                (let* ((stringify-arg
                        (lambda (arg)
                          (match arg
                                 (('volatile T V)
                                  (string-append "  volatile " T "* " V))
                                 (('atomic T V)
                                  (string-append "  atomic_" T "* " V))))))

                  (print "void " (litc-proc-name p) " (")
                  (let* (
                         (all-args (append
                                    (litc-proc-args p)
                                    (map (lambda (read)
                                           (let-values (((T V vname)
                                                         (apply values read)))
                                             `(volatile ,T ,vname)))
                                         (litc-proc-reads p))))
                         (args-str (map stringify-arg all-args)))
                    (print (string-join args-str ",\n"))
                    (display ") "))

                  (print-proc-lines p)
                  (newline)))
              procs)

    (newline)
    (print "/* final assertion */")
    (print "void not_exists(void) {")
    (display "  ensure(!")
    (print-expr (litc-exists litc))
    (print ");")
    (print "}")

    (newline)
    (print "#ifndef RMEM")

    ; generate one pthread function per proc
    (newline)
    (print "/* pthread run functions */")
    (for-each (lambda (p)
                (let ((pid (litc-proc-id p))
                      (pname (litc-proc-name p)))
                  (display (string-append "void *run" pid "(void *_) {"))
                  (display pname)
                  (let* ((all-args
                          (append
                           (litc-proc-args p)
                           (map (lambda (read)
                                  (let-values (((T V vname)
                                                (apply values read)))
                                    `(volatile ,T ,vname)))
                                (litc-proc-reads p))))

                         (args-str (map (lambda (arg)
                                          (match arg
                                                 (('volatile T V) (string-append "&" V))
                                                 (('atomic T V) (string-append "&" V))))
                                        all-args)))
                    (print "(" (string-join args-str ",") "); return 0;}"))))
              procs)

    ; generate main function
    (newline)
    (print "int main(void) {")
    (let ((pids (map litc-proc-id (litc-procs litc))))
      (for-each (lambda (pid)
                  (print "  pthread_t t" pid ";")) pids)
      (for-each (lambda (pid)
                  (print "  pthread_create(&t" pid ", 0, run" pid ", 0);")) pids)
      (for-each (lambda (pid)
                  (print "  pthread_join(t" pid ", 0);")) pids)
      (newline)
      (print "  not_exists();")
      (newline)
      (print "  return 0;")
      (print "}"))
    (print "#endif")))


(define (main args)
  (die-unless (not (null? args)) "input file")
  (let* ((fn (car args))
         (g (file-generator fn))
         (tokens (parse-or-die parser g))
         (litc (make-litc tokens)))
    (generate-c litc))
  0)
(main (command-args))