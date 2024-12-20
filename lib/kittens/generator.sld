; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define-library (kittens generator)
  (export token-generator
          file-generator
          str-generator)
  (import (scheme base)
          (scheme file)
          (kittens packrat))
  (begin
    (define (token-generator tokens)
      (let ((stream tokens))
        (lambda ()
          (if (null? stream)
              (values #f #f)
              (let ((base-token (car stream)))
                (set! stream (cdr stream))
                (values #f base-token))))))

    (define (file-generator filename)
      (let ((port (open-input-file filename))
            (ateof #f)
            (pos (top-parse-position filename)))
        (lambda ()
          (if ateof
              (values pos #f)
              (let ((x (read-char port)))
                (if (eof-object? x)
                    (begin
                      (set! ateof #t)
                      (values pos #f))
                    (let ((old-pos pos))
                      (set! pos (update-parse-position pos x))
                      (values old-pos (cons x x)))))))))

    (define (str-generator str)
      (let ((input (string->list str))
            (pos (top-parse-position "<?>")))
        (lambda ()
          (if (null? input)
              (values pos #f)
              (let ((x (car input)))
                (set! input (cdr input))
                (let ((old-pos pos))
                  (set! pos (update-parse-position pos x))
                  (values old-pos (cons x x))))))))))
