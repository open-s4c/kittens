; -*- scheme -*-

(import (scheme small)
        (chibi test)
        (srfi 166)
        (rebottled packrat))

(include "generic.scm")
(include "proc.scm")
(include "../cat/generator.scm")

(define (code . lines)
  (apply string-append
         (map (lambda (line)
                (string-append line "\n"))
              lines)))


(define (proc-parse str)
  (proc-parser (base-generator->results (str-generator str))))

(define (args-parse str)
  (args-parser (base-generator->results (str-generator str))))

(define (code-parse str)
  (code-parser (base-generator->results (str-generator str))))

(test-group
 "args"
 (let ((r (args-parse "(volatile int* y)")))
   (test-assert (parse-result-successful? r))
   (test '(args (volatile "int" "y")) (parse-result-semantic-value r)))

 (let ((r (args-parse "(volatile int* y, volatile long* x)")))
   (test-assert (parse-result-successful? r))
   (test '(args (volatile "int" "y") (volatile "long" "x"))
         (parse-result-semantic-value r)))


 (let ((r (args-parse "(volatile int* y, atomic_long* x)")))
   (test-assert (parse-result-successful? r))
   (test '(args (volatile "int" "y") (atomic "long" "x"))
         (parse-result-semantic-value r))))

(test-group
 "code"
 (let* ((input (code "x = 1;"))
        (r (code-parse input)))
   (test-assert (parse-result-successful? r))
   (test '(code (line "x = 1;")) (parse-result-semantic-value r)))

 (let* ((input (code "int x = 1;"))
        (r (code-parse input)))
   (test-assert (parse-result-successful? r))
   (test '(code (local (decl "int" "x") (line "int x = 1;")))
         (parse-result-semantic-value r)))

 (let* ((input (code "int x = 1;"
                     "y = x;"))
        (r (code-parse input)))
   (test-assert (parse-result-successful? r))
   (test '(code (local (decl "int" "x") (line "int x = 1;"))
                (line "y = x;"))
         (parse-result-semantic-value r)))

 (let* ((input (code "int x = 1;"
                     "	y = x;"))
        (r (code-parse input)))
   (test-assert (parse-result-successful? r))
   (test '(code (local (decl "int" "x") (line "int x = 1;"))
                (line "y = x;"))
         (parse-result-semantic-value r))))

(test-group
 "proc"
 (let* ((input (code
                "P0 (volatile int* y,volatile int* x) {"
                "	int r0 = *x;"
                "	*y = 1;"
                "}"
                ))
        (r (proc-parse input)))
   (test-assert (parse-result-successful? r))
   (test '(proc (pid . 0)
                (args (volatile "int" "y")
                      (volatile "int" "x"))
                (code (line "{")
                      (local (decl "int" "r0")
                             (line "int r0 = *x;"))
                      (line "*y = 1;")
                      (line "}")))
         (parse-result-semantic-value r)))

 (let* ((input (code
                "P0 (volatile int* y, atomic_int* x) {"
                "	int r0 = *x;"
                "	if (r0 == 0) {"
                "		do();"
                "	}"
                "}"
                ))
        (r (proc-parse input)))
   (test-assert (parse-result-successful? r))
   (test '(proc (pid . 0)
                (args (volatile "int" "y")
                      (atomic "int" "x"))
                (code (line "{")
                      (local (decl "int" "r0")
                             (line "int r0 = *x;"))
                      (line "if (r0 == 0) {")
                      (line "do();")
                      (line "}")
                      (line "}")))
         (parse-result-semantic-value r))))
