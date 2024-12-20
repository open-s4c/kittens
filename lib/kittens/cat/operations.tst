; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme small)
        (kittens cat)
        (kittens test)
        (kittens debug))

(test-group
 "expand-expr"

 (let* ((stmts  '((let "a" (union (rel . "a") (rel . "b")))))
        (defm (collect-definitions/let stmts))
        (expr '(rel . "a")))
   (test '(union (rel . "a") (rel . "b"))
         (expand-expr expr defm)))

 (let* ((stmts  '((let "rf" (seq (self set . "RMW") (rel . "rf")))
                  (let "RMW" (union (set . "CAS") (set . "XCHG")))))
        (defm (collect-definitions/let stmts))
        (expr '(seq (rel . "rf") (rel . "rf"))))

   (test '(seq
           (seq (self union (set . "CAS") (set . "XCHG"))
                (rel . "rf"))
           (seq (self union (set . "CAS") (set . "XCHG"))
                (rel . "rf")))
         (expand-expr expr defm)))
 )

(test-group
 "normalize"
 (let ((expr '(self isect (set . "a") (set . "b"))))
   (let ((expr (normalize-expr expr)))
     (test '(isect (self set . "a")
                   (self set . "b"))
           expr)))
 (let ((expr '(seq (self set . "x")
                   (self isect (set . "a") (set . "b")))))
   (let ((expr (normalize-expr expr)))
     (test '(seq (self set . "x")
                 (isect (self set . "a")
                        (self set . "b")))
           expr)))
 )
