; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens packrat)
  (export base-generator->results
          make-expected-result
          make-result
          packrat-parser
          parse-error-expected
          parse-error-position
          parse-error-messages
          parse-position->string
          parse-result-error
          parse-result-semantic-value
          parse-result-successful?
          parse-results-next
          parse-results-position
          parse-results-token-kind
          parse-results-token-value
          top-parse-position
          update-parse-position)
  (import (scheme base))
  (cond-expand
    (chicken (import packrat))
    (else (import (rebottled packrat)))))
