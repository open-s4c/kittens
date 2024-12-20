; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens utils)
  (export unique
          seq
          print)

  (import (scheme base)
          (scheme write))
  (begin

    (define (print . xs)
      (for-each display xs)
      (newline))

    ;; Returns unique values of `lst` in the order of first appearence.
    (define (unique lst)
      (define (iter lst out)
        (if (null? lst)
            out
            (if (member (car lst) out)
                (iter (cdr lst) out)
                (iter (cdr lst) (cons (car lst) out)))))

      ; return the reverse other to maintain the original order of the list
      (reverse (iter lst '())))

    ;; Creates this list of integers from 0 to n-1.
    (define (seq n)
      (let loop ((i 0) (lst '()))
        (if (= i n)
            lst
            (loop (+ i 1) (cons (- n i 1) lst)))))))
