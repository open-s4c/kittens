; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens cat)
  (export lexer
          expr-parser
          model-parser

          select-statements
          collect-definitions
          collect-definitions/let
          expand-expr
          visit-expr
          normalize-expr
          validate-expr)

  (import (scheme base)
          (scheme file)
          (scheme cxr)
          (scheme char)
          (only (srfi 1) filter)
          (scheme write)
          (kittens debug)
          (kittens match)
          (kittens packrat))

  (cond-expand ; hash-table
    (chicken (import (srfi 69)))
    (else (import (srfi 125)
                  (only (srfi 128) string-hash))))

  (include "cat/visitor.scm")
  (include "cat/validator.scm")
  (include "cat/operations.scm")
  (include "cat/lexer.scm")
  (include "cat/expr.scm")
  (include "cat/stmt.scm")
  (include "cat/model.scm"))
