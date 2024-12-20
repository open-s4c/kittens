; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme small)
        (kittens cat)
        (kittens test)
        (kittens debug))

(test-group
 "visit-expr"

 (let ((expr '(union (rel . "a") (rel . "b"))))
   (let ((buffer '()))
     (visit-expr expr (lambda (expr)
                        (set! buffer (cons expr buffer))))
     (test '((rel . "a") (rel . "b"))
           (reverse buffer))))

 (let ((expr '(union (union (rel . "x") (rel . "a")) (rel . "b"))))
   (let ((buffer '()))
     (visit-expr expr (lambda (expr)
                        (set! buffer (cons expr buffer))))
     (test '((rel . "x") (rel . "a") (rel . "b"))
           (reverse buffer))))

 (let ((expr '(seq (union (rel . "x") (rel . "a")) (rel . "b"))))
   (let ((buffer '()))
     (visit-expr expr (lambda (expr)
                        (set! buffer (cons expr buffer))))
     (test '((seq (rel . "x") (rel . "b"))
             (seq (rel . "a") (rel . "b")))
           (reverse buffer))))

 (let ((expr '(seq (union (rel . "x") (rel . "a"))
                   (union (rel . "y") (rel . "b")))))
   (let ((buffer '()))
     (visit-expr expr (lambda (expr)
                        (set! buffer (cons expr buffer))))
     (test '((seq (rel . "x") (rel . "y"))
             (seq (rel . "x") (rel . "b"))
             (seq (rel . "a") (rel . "y"))
             (seq (rel . "a") (rel . "b")))
           (reverse buffer))))

 (let ((expr '(seq (self set . "x")
                   (union (rel . "y") (rel . "b")))))
   (let ((buffer '()))
     (visit-expr expr (lambda (expr)
                        (set! buffer (cons expr buffer))))
     (test '((seq (self set . "x") (rel . "y"))
             (seq (self set . "x") (rel . "b")))
           (reverse buffer))))
 )
