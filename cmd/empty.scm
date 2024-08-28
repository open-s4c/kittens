#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor

(import (scheme base)
        (scheme file)
        (kittens cat)
        (kittens utils)
        (kittens generator)
        (kittens command)
        (srfi 1) ; filter
        (srfi 130))

(define (usage)
  (print "empty <expr>"))

(define (parse-expr str)
  (let ((str-gen (str-generator str)))
    (let ((token-gen (token-generator (parse-or-die lexer str-gen))))
      (parse-or-die expr-parser token-gen))))

(define (main args)
  (die-unless (= 1 (length args)) "<expr>")

  (let* ((expr (car args))
         (expr (parse-expr expr)))
    (print expr)
    (newline))

  0)

(start-command main)


