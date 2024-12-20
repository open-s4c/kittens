; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define stmt-parser
  (packrat-parser
   stmt

   (stmt
    ((x <- inc-stmt) x)
    ((x <- let-stmt) x)
    ((x <- acy-stmt) x)
    ((x <- epy-stmt) x))

   (inc-stmt (('include n <- 'string) `(include ,n)))
   (let-stmt (('let n <- 'id '= e <- expr) `(let ,n ,e)))
   (acy-stmt (('acyclic e <- expr 'as n <- 'id) `(acyclic ,e))
             (('acyclic e <- expr) `(acyclic ,e))
             (('acyclic n <- 'id) `(acyclic ,n)))
   (epy-stmt (('empty e <- expr 'as n <- 'id) `(empty ,e))
             (('empty e <- expr) `(empty ,e))
             (('empty n <- 'id) `(empty ,n)))

   (expr ((x <- expr-parser) x))))

