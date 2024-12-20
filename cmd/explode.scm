#!/usr/bin/env -S chibi-scheme -Ilib -Ivendor
; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

;; explode
;;
;; Assumption
;; - there is no recursive definition
;;   for example `let rf = X;rf;Y` will replace rf only once
;;
;; How to achieve that:
;;   in a let statement defining s, the symbol s should not occur in
;;   the right hand expression. If it does, we rename s as base$s
;;   In the final print we remove all base$ prefixes.
;;   How should we deal with mutually recursive definitions such as:
;;     let a = b
;;     let b = X;a;Y
;;   My take would be to claim these to be disallowed.
;;   An alternative to the previous approach for the let x = x case is to
;;   take the order in which the definitions are done into account and
;;   "overwrite" previous definitions with the following redefinitions.
;;
;; 1- parse the cat file
;; 2- collects a map with all let statements
;; 3- decides whether to do acyclic explode or empty explode
;; 3.1- acyclic explode: concatenate N expressions

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
; Isomorphism filtering
; ------------------------------------------------------------------------------
(define (contains-isomorphism res cycle)
  (define (helper res cycle n)
    (if (eq? n 0)
        #f
        (if (or (member cycle res)
                (member (reverse cycle) res))
            #t
            (helper res (rotate-list cycle) (- n 1)))))
  (helper res cycle (length res)))

(define (rotate-list lst)
  (if (null? lst)
      lst
      (append (cdr lst) (list (car lst)))))

(define (remove-dub res remaining)
  (if (null? remaining)
      res
      (if (contains-isomorphism res (car remaining))
          (remove-dub res (cdr remaining))
          (remove-dub (append res (list (car remaining))) (cdr remaining)))))

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

