; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens cat validator)
  (export seq-valid
          isect-valid
          )
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

  (include "validator.scm")
  )

