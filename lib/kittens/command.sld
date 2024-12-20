; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens command)
  (export die-unless
          parse-or-die
          start-command

          ; reexport
          exit
          display
          newline
          string-join
          string-split
          string-downcase
          string-upcase)

  (import (scheme base)
          (scheme write)
          (scheme char)
          (scheme process-context)
          (only (srfi 130) string-contains string-split)
          (only (srfi 193) command-args command-line)
          (only (srfi 130) string-join)
          (kittens packrat))

  (begin
    (define-syntax die-unless
      (syntax-rules ()
        ((_ cnd msg)
         (unless cnd
           (error 'cnd msg)))
        ((_ cnd msg usage)
         (unless cnd
           (display "USAGE: ")
           (usage)
           (newline)
           (error 'cnd msg)))))

    (define (parse-or-die parser generator)
      (let ((result (parser (base-generator->results generator))))
        (if (parse-result-successful? result)
            (parse-result-semantic-value result)
            (error 'parse-error "parse error"
                   (let ((e (parse-result-error result)))
                     (list 'parse-error
                           (parse-position->string (parse-error-position e))
                           (parse-error-expected e)
                           (parse-error-messages e)))))))

    (define (start-command foo)
      (let ((arg0 (car (command-line))))
        (unless (string-contains arg0 ".tst")
          (foo (command-args)))))))
