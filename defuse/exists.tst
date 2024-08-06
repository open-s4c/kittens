; -*- scheme -*-

(import (scheme small)
        (chibi test)
        (srfi 166)
        (rebottled packrat))

(include "generic.scm")
(include "exists.scm")
(include "../cat/generator.scm")

(define (code . lines)
  (apply string-append
         (map (lambda (line)
                (string-append line "\n"))
              lines)))


(define (var-parse str)
  (var-parser (base-generator->results (str-generator str))))

(define (equal-parse str)
  (equal-parser (base-generator->results (str-generator str))))

(define (expr-parse str)
  (expr-parser (base-generator->results (str-generator str))))

(define (exists-parse str)
  (exists-parser (base-generator->results (str-generator str))))

(test-group
 "var"
 (let ((r (var-parse "[x]")))
   (test-assert (parse-result-successful? r))
   (test '(deref-var "x") (parse-result-semantic-value r)))
 (let ((r (var-parse "2:r1")))
   (test-assert (parse-result-successful? r))
   (test '(read-var 2 "r1") (parse-result-semantic-value r))))

(test-group
 "equality"
 (let ((r (equal-parse "[x]=2")))
   (test-assert (parse-result-successful? r))
   (test '(equal (deref-var "x") "2") (parse-result-semantic-value r)))
 (let ((r (equal-parse "0:r1=3")))
   (test-assert (parse-result-successful? r))
   (test '(equal (read-var 0 "r1") "3") (parse-result-semantic-value r))))

(test-group
 "expr"
 (let ((r (expr-parse "[x]=2")))
   (test-assert (parse-result-successful? r))
   (test '(equal (deref-var "x") "2") (parse-result-semantic-value r)))
 (let ((r (expr-parse "  [x]=2")))
   (test-assert (parse-result-successful? r))
   (test '(equal (deref-var "x") "2") (parse-result-semantic-value r)))
 (let ((r (expr-parse "0:r1=3  /\\     [x]=2")))
   (test-assert (parse-result-successful? r))
   (test '(disj (equal (read-var 0 "r1") "3")
                (equal (deref-var "x") "2"))
         (parse-result-semantic-value r)))
 (let ((r (expr-parse "0:r0=0 /\\ 1:r1=1 /\\ 2:r2=2")))
   (test-assert (parse-result-successful? r))
   (test '(disj (equal (read-var 0 "r0") "0")
                (disj (equal (read-var 1 "r1") "1")
                      (equal (read-var 2 "r2") "2")))
         (parse-result-semantic-value r))))

(test-group
 "exists"
 (let ((r (exists-parse "exists ( 0:r1=1)")))
   (test-assert (parse-result-successful? r))
   (test '(exists . (equal (read-var 0 "r1") "1"))
         (parse-result-semantic-value r)))
 (let ((r (exists-parse "exists ([x]=2 /\\ 0:r1=1)")))
   (test-assert (parse-result-successful? r))
   (test '(exists . (disj (equal (deref-var "x") "2")
                          (equal (read-var 0 "r1") "1")))
         (parse-result-semantic-value r)))
 (let ((r (exists-parse "exists ([x]=2 /\\ 0:r1=1 /\\ [y]=3)")))
   (test-assert (parse-result-successful? r))
   (test '(exists . (disj (equal (deref-var "x") "2")
                          (disj (equal (read-var 0 "r1") "1")
                                (equal (deref-var "y") "3"))))
         (parse-result-semantic-value r)))
 )
