; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define model-parser
  (packrat-parser
   model

   (model ((n <- name s <- stmts) `(model ,n ,s)))

   (name ((n <- 'string) `(name ,n))
         ((n <- 'id) `(name ,n)))

   (stmts ((x <- stmt s <- stmts) (cons x s))
          ((x <- stmt) (list x)))

   (stmt ((x <- stmt-parser) x))))

