#!/usr/bin/env -S chibi-scheme -I.

(import (scheme small)
        (srfi 193) ; command-args
        (srfi 166) ; formatting
        (srfi 125) ; hash tables
        (only (chibi string) string-join)
        (chibi match)
        (rebottled packrat)
        (defuse parser))

(define (pretty-print x)
  (show (current-output-port) (pretty x)))

(define (print . xs)
  (for-each display xs)
  (newline))

(define-syntax die-unless
  (syntax-rules ()
    ((_ cnd msg)
     (unless cnd
       (print "<kittens> <model file> <cycle length>")
       (newline)
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

(define (unique lst)
  (define (iter lst out)
    (if (null? lst)
        out
        (if (member (car lst) out)
            (iter (cdr lst) out)
            (iter (cdr lst) (cons (car lst) out)))))
  (iter lst '()))


(define (generate-c tokens)
  ; print preamble in comment
  (let ((preamble (car tokens)))
    (when #t
      (print "/*")
      (for-each print (cdr preamble))
      (print "*/")))

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
  (let ((vars (cadr tokens)))
    (when #f
      (print "/*")
      (for-each print (cdr vars))
      (print "*/")))

  (let ((procs (cdr (caddr tokens))))
    ; collect all arguments and create variables for the arguments
    (let* ((args (map caddr procs))   ; take args of each proc
           (args (map cdr args))      ; remove "args" label
           (args (apply append args)) ; combine all args
           (args (unique args)))      ; remove duplicates

      (newline)
      (print "/* shared variables */")
      (for-each (lambda (arg)
                  (match arg
                         (('volatile T V) (print "volatile " T " " V ";"))
                         (('atomic T V) (print "atomic_" T " " V ";"))))
                args))

    ; create a variable for each read
    (newline)
    (print "/* variables to store per-proc reads */")
    (for-each (lambda (p)
                (let* ((pid (number->string (cdr (cadr p))))
                       (lines (cdr (cadddr p))))
                  (for-each (lambda (line)
                              (match line
                                     (('line str) #f)
                                     (('local ('decl T V) ('line str))
                                      (print
                                       T " " V "_P" pid ";"))))
                            lines)))
              procs)

    ; create one function per proc and use the arguments as global variables
    (newline)
    (print "/* processor functions */")
    (for-each (lambda (p)
                (let* ((pid (number->string (cdr (cadr p))))
                       (args (cdr (caddr p)))
                       (lines (cdr (cadddr p)))
                       (stringify-arg
                        (lambda (arg)
                          (match arg
                                 (('volatile T V)
                                  (string-append "  volatile " T "* " V))
                                 (('atomic T V)
                                  (string-append "  atomic_" T "* " V))))))

                  (print "void " "P" pid " (")
                  (let ((args-str (map stringify-arg args)))
                    (print (string-join args-str ",\n"))
                    (display ") "))

                  (let ((lines-str (map (lambda (line)
                                          (match line
                                                 (('line str) str)
                                                 (('local ('decl T V) ('line str))
                                                  (string-append
                                                   str
                                                   " "
                                                   V "_P" pid " = " V ";"))))
                                        lines)))
                    (print (string-join lines-str "\n  ")))
                  (newline)))
              procs)

    (newline)
    (print "/* final assertion */")
    (print "void not_exists(void) {")
    (display "  ensure(!")
    (let ((exists (cadddr tokens)))
      (define (print-expr expr)
        (match expr
               (('conj a b)
                (display "(")
                (print-expr a)
                (display " && ")
                (print-expr b)
                (display ")"))
               (('equal ('read-var p v) rhs)
                (display (string-append v "_P" (number->string p) " == " rhs)))
               (('equal ('deref-var v) rhs)
                (display (string-append v " == " rhs)))))
      (print-expr (cdr exists)))
    (print ");")
    (print "}")

    (newline)
    (print "#ifndef RMEM")

    ; generate one pthread function per proc
    (newline)
    (print "/* pthread run functions */")
    (for-each (lambda (p)
                (let* ((pid (number->string (cdr (cadr p))))
                       (args (cdr (caddr p))))
                  (display (string-append "void *run" pid "(void *_) {"))
                  (display (string-append "P" pid))
                  (let ((args-str (map (lambda (arg)
                                         (match arg
                                                (('volatile T V) (string-append "&" V))
                                                (('atomic T V) (string-append "&" V))))
                                       args)))
                    (print "(" (string-join args-str ",") "); return 0;}"))))
              procs)

    ; generate main function
    (newline)
    (print "int main(void) {")
    (let ((pids (map (lambda (p) (number->string (cdr (cadr p)))) procs)))
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
         (g (file-generator fn)))
    (let ((tokens (parse-or-die parser g)))
      (generate-c tokens)))
  0)
(main (command-args))
