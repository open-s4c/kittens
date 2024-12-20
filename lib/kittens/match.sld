; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens match)
  (export match)
  (cond-expand
    (chicken (import matchable))
    (else (import (chibi match)))))
