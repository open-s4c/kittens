#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
;; # explode
;;
;; Expand the axioms in a cat model in a list of kittens (union-free subaxioms).
;;
;; ## Algorithm
;;
;; 1. parse the cat file
;; 2. parse include files
;; 3. collects a map with all let statements
;; 4. for each acyclic axiom (acyclic . expr):
;;    - completely expand expr using map from step 3
;;    - combine expr N times with `;`
;;    - traverse the expression tree in DFS
;;    - print each union-free subexpression (kitten)
;; 5. for each non-acyclic axiom (eg, empty):
;;    - completely expand expr using map from step 3
;;    - traverse the expression tree in DFS
;;    - print each union-free subexpression (kitten)

(import (scheme base)
        (scheme file)
        (scheme cxr)
        (only (srfi 1) filter fold-right)
        (kittens cat)
        (kittens match)
        (kittens utils)
        (kittens debug)
        (kittens generator)
        (kittens command))

(cond-expand ; hash-table
  (chicken (import (srfi 69)))
  (else (import (srfi 125)
                (only (srfi 128) string-hash))))

; ------------------------------------------------------------------------------
; Command
; ------------------------------------------------------------------------------
(define (usage)
  (print "explode <model file> <edges> [<include file> ...]"))

(define (main args)
  (die-unless (>= (length args) 2) "wrong arguments" usage)
  (let* ((fn (car args))
         (len (cadr args))
         (len (string->number len))
         (rest (cddr args)))
    (explode fn len rest)
    0))

; ------------------------------------------------------------------------------
; Explode
; ------------------------------------------------------------------------------
(define (explode fn len rest)
  (let ((port (open-input-file fn)))
    (print "# model file: " fn)
    (print "# cycle len: " len)
    (when (not (null? rest))
      (apply print "# include files: " rest))

    ; Step 1 - tokenization of cat file
    (let ((tokens (tokenize-cat fn)))
      ;(debugf "[TOKENS] " tokens)

      ; Step 2 - parse cat and merge with include files
      (let* ((model1 (parse-cat tokens))
             (model2 (fold-right include-file model1 rest))
             (model (include-files model2)))
        ;(debugf "[STMTS] " (select-statements model 'any))

        ; Step 3 - collect map of let definitons
        (let ((defm (collect-definitions model)))

          ;(debugf "[LET] " defm)

          ; Step 3.1 - empty
          (let ((axioms (select-statements model 'empty))
                (action (lambda (expr)
                          (let ((expr (normalize-expr expr)))
                            (when (validate-expr expr #f)
                              (print-empty expr))))))
            (for-each (lambda (axiom)
                        (visit-expr (expand-expr axiom defm) action))
                      (map cadr axioms)))

          ; Step 3.2 - acyclic
          (let ((axioms (select-statements model 'acyclic))
                (action (lambda (expr)
                          (let ((expr (normalize-expr expr)))
                            (when (validate-expr expr #t)
                              (print-acyclic expr))))))
            (define (seq-expr expr len)
              (cond ((= len 1) expr)
                    ((> len 1)
                     (let ((expr2 (seq-expr expr (- len 1))))
                       `(seq ,expr ,expr2)))
                    (else (error "cannot have length <= 0"))))
            (for-each (lambda (axiom)
                        (let ((axiom (expand-expr (seq-expr axiom len) defm)))
                          (visit-expr axiom action)))
                      (map cadr axioms))))))))

(define (tokenize-cat fn)
  (parse-or-die lexer (file-generator fn)))

(define (parse-cat tokens)
  (parse-or-die model-parser (token-generator tokens)))

(define (include-files model)
  (let ((stmts (caddr model)))
    (define (iter stmts nstmts)
      (if (null? stmts)
          nstmts
          (let ((stmt (car stmts))
                (rest (cdr stmts)))
            (if (eq? 'include (car stmt))
                (if (file-exists? (cadr stmt))
                    (let* ((tks (tokenize-cat (cadr stmt)))
                           (imodel (parse-cat tks))
                           (istmts (caddr imodel)))
                      (iter rest (append istmts nstmts)))
                    (begin
                      (print "# WARNING: cannot include '" (cadr stmt) "'")
                      (iter rest nstmts)))
                (iter rest (cons stmt nstmts))))))
    (list 'model (cadr model) (reverse (iter stmts '())))))

(define (include-file fn model)
  (if (file-exists? fn)
      (let* ((tks (tokenize-cat fn)))
        (let* ((imodel (parse-cat tks))
               (istmts (caddr imodel))
               (stmts (caddr model)))
          (list 'model (cadr model) (append istmts stmts))))
      model))

; ------------------------------------------------------------------------------
; Printing
; ------------------------------------------------------------------------------

(define (print-empty empty)
  (display "empty ")
  (print-stmt empty #f)
  (newline))

(define (print-acyclic acyclic)
  (display "acyclic ")
  (print-stmt acyclic #f)
  (newline))

(define (infix)
  (list 'union 'sadd 'seq 'isect 'diff 'cart))

(define (parentheses out in)
  (match out
         ('self #f)
         ('set #f)
         ('rel #f)
         ('inv (member in (infix)))
         ('not (member in (infix)))
         ('zone (member in (infix)))
         ('kstar (member in (infix)))
         ('aone (member in (infix)))
         ('union #t)
         ('sadd (member in (list 'union)))
         ('seq (member in (list 'union 'sadd)))
         ('isect (member in (list 'union 'sadd 'seq)))
         ('diff (member in (list 'union 'sadd 'seq 'isect)))
         ('cart (member in (list 'union 'sadd 'seq 'isect 'diff)))))

(define (print-stmt stmt br)
  (if br (display "("))
  (match stmt
         (('seq . rest)
          (print-stmt (car rest) (parentheses 'seq (caar rest)))
          (display ";")
          (print-stmt (cadr rest) (parentheses 'seq (caadr rest))))

         (('isect . rest)
          (print-stmt (car rest) (parentheses 'isect (caar rest)))
          (display "&")
          (print-stmt (cadr rest) (parentheses 'isect (caadr rest))))

         (('self . rest)
          (display "[")
          (print-stmt rest (parentheses 'self (car rest)))
          (display "]"))

         (('rel . rest)
          (display (match rest ("rfx" "rf") (else rest))))

         (('set . rest)
          (display rest))

         (('inv . rest)
          (print-stmt rest (parentheses 'inv (car rest)))
          (display "^-1"))

         (('not . rest)
          (display "~")
          (print-stmt rest (parentheses 'not (car rest))))

         (else (display stmt)))
  (if br (display ")")))

; ==============================================================================
(start-command main)

