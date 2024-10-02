#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (srfi 1)
        (scheme cxr)
        (kittens match)
        (kittens generator)
        (kittens utils)
        (kittens litc)
        (kittens command))

(define (usage)
  (print "defuse <c-litmus file>")
  (newline))

(define (print-expr expr)
  ;(print expr)
  ;(print (string? expr))
  ;(print (if (string? expr) (if (equal? "0" (string-ref expr 9)) ":" ".") ""))
  (match expr
         (('disj a b)
          (display "(")
          (print-expr a)
          (display " && ")
          (print-expr b)
          (display ")"))
         (('equal ('read-var p v) rhs)
          (display (string-append v "_P" (number->string p) " == " (if (equal? rhs "0") "(long) &a" rhs))))
         (('equal ('deref-var v) rhs)
          (display (string-append "(*(long*)&" v ") == " (if (equal? rhs "0") "(long) &a" rhs))))))



(define (print-read-declare p)
  (let ((name (litc-proc-name p))
        (lines (litc-proc-lines p)))
    (for-each (lambda (line)
                (match line
                       (('line str) str)
                       (('local ('decl T V _) _)
                        (print "    " T " " V ";"))))
              lines)))

(define (print-proc-lines p)
  (let* ((name (litc-proc-name p))
         (lines (litc-proc-lines p))
         (L (- (length lines) 1)))
    (for-each (lambda (line i)
                (unless (or (and (= i 0) (equal? line '(line "{")))
                            (and (= i L) (equal? line '(line "}"))))
                  (print "    "
                         (match line
                                (('line str) str)
                                (('local ('decl T V rhs) _)
                                 (string-append V " = " rhs))))))
              lines
              (seq (length lines)))))

(define (print-read-lines p)
  (let ((name (litc-proc-name p))
        (lines (litc-proc-lines p)))
    (for-each (lambda (line)
                (match line
                       (('line str) str)
                       (('local ('decl T V _) _)
                        (print "    *" V "_" name " = " V ";"))))
              lines)))

(define (declare-read-var read)
  (let-values (((T V vname) (apply values read)))
    (print T " " vname ";")))

(define (declare-arg-var arg)
  (match arg
         (('volatile T V) (print "volatile " T " " V ";"))
         (('atomic T V) (print "atomic_" T " " V ";"))))

(define (define-proc-function p)
  (print "void " (litc-proc-name p) " (")

  (let* ((stringify-arg
          (lambda (arg)
            (match arg
                   (('volatile T V)
                    (string-append "  volatile " T "* " V))
                   (('atomic T V)
                    (string-append "  atomic_" T "* " V)))))
         (all-args (append
                    (litc-proc-args p)
                    (map (lambda (read)
                           (let-values (((T V vname)
                                         (apply values read)))
                             `(volatile ,T ,vname)))
                         (litc-proc-reads p))))
         (args-str (map stringify-arg all-args)))
    (print (string-join args-str ",\n"))
    (print") {")
    (print-read-declare p)
    (print-proc-lines p)
    (print-read-lines p)
    (print "  }")
    (newline)))
(define (define-run-function p)
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
                                   (('volatile T V)
                                    (string-append
                                     "(volatile " T "*)&" V))
                                   (('atomic T V)
                                    (string-append
                                     "(atomic_" T "*)&" V))))
                          all-args)))
      (print "(" (string-join args-str ",") "); return 0;}"))))

(define (unique-arg lst)
  (define (var-name var)
    (caddr var))
  (define (iter lst out)
    (if (null? lst)
        out
        (let ((names (map var-name out)))
          (if (member (var-name (car lst)) names)
              (iter (cdr lst) out)
              (iter (cdr lst) (cons (car lst) out))))))
  (iter lst '()))

(define (generate-c litc)
  ; print preamble in comment
  (when #t
    (print "/*")
    (for-each print (litc-preamble litc))
    (print "*/"))

  ; add includes
  (newline)
  (print "#include <stdatomic.h>")
  (print "#include <stdio.h>")
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

  (newline)
  (print "/* variable declared to use address as value */")
  (print "int a;")

  (let ((procs (litc-procs litc)))
    ; collect all arguments and create variables for the arguments
    (let* ((args (map litc-proc-args procs))   ; take args of each proc
           (args (apply append args)) ; combine all args
           (args (unique-arg args)))      ; remove duplicates

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
    (for-each define-proc-function procs)

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
    (for-each define-run-function procs)

    ; generate main function
    (newline)
    (print "int main(void) {")
    (let ((pids (map litc-proc-id (litc-procs litc))))
      ;(for-each (lambda (var)
      ;		  (print "  atomic_init(&" var ", (atomic_long) &a);")) (filter (lambda (str) (not (equal? str ""))) (string-split (car (litc-vars litc)) #\;)))
      (for-each (lambda (var) (print "  " var " = (volatile long)&a;"))  (filter (lambda (str) (not (equal? str ""))) (string-split (car (litc-vars litc)) ";")))


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

(start-command main)
