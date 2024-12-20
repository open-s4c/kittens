; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define lexer
  (packrat-parser
   (begin
     (define (str str)
       (lambda (starting-results)
         (let loop ((pos 0) (results starting-results))
           (if (= pos (string-length str))
               (make-result str results)
               (let ((ch (parse-results-token-value results)))
                 (if (and ch (char=? ch (string-ref str pos)))
                     (loop (+ pos 1) (parse-results-next results))
                     (make-expected-result
                      (parse-results-position starting-results) str)))))))


     ; according to pg.X whatever
     ; TODO: first char must be text?
     (define (cat-id starting-results)
       (let loop ((acc '()) (results starting-results))
         (let ((ch (parse-results-token-value results)))
           (cond
             ((and ch (null? acc) (memv ch '(#\space #\tab #\newline)))
              (loop acc (parse-results-next results)))
             ((and ch (char-ci>=? ch #\A) (char-ci<=? ch #\Z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\a) (char-ci<=? ch #\z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\0) (char-ci<=? ch #\9))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (memv ch '( #\_ #\. #\- )))
              (loop (cons ch acc) (parse-results-next results)))
             ((null? acc)
              (make-expected-result
               (parse-results-position starting-results) 'id))
             (else
              (make-result (list->string (reverse acc)) results))))))

     (define (cat-string results)
       (let loop ((acc '()) (results results))
         (let ((ch (parse-results-token-value results)))
           (cond
             ((and ch (char-ci>=? ch #\A) (char-ci<=? ch #\Z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\a) (char-ci<=? ch #\z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\0) (char-ci<=? ch #\9))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (memv ch '( #\_ #\. #\- #\space #\tab)))
              (loop (cons ch acc) (parse-results-next results)))
             (else
              (make-result (list->string (reverse acc)) results))))))

     (define (skip rule)
       (lambda (results)
         (rule (parse-results-next results))))
     file)

   (file ((x <- tokens) x))

   (tokens ((blurb x <- token blurb xs <- tokens) (cons x xs))
           ((x <- token blurb xs <- tokens) (cons x xs))
           ((x <- token xs <- tokens) (cons x xs))
           ((x <- token) (list x)))

   (blurb ((blurb/one blurb) '())
          ((blurb/one) '()))

   (blurb/one ((comment/) '())
              ((comment*) '())
              ((comment/*) '())
              (('#\space) '())
              (('#\tab) '())
              (('#\newline) '()))

   (token (((str "=")) '(=))
          (((str "(")) '(oparen))
          (((str ")")) '(cparen))
          (((str "[")) '(obrack))
          (((str "]")) '(cbrack))
          (((str "{")) '(obrace))
          (((str "}")) '(cbrace))
          (((str "|")) '(union))
          (((str "&")) '(isect))
          (((str ";")) '(seq))
          (((str ",")) '(comma))
          (((str "*")) '(cart))
          (((str "~")) '(not))
          (((str "^-1")) '(inv))

          (((str "let")) '(let))
          (((str "show")) '(show))
          (((str "include")) '(include))
          (((str "acyclic")) '(acyclic))
          (((str "empty")) '(empty))
          (((str "as")) '(as))
          (('#\" x <- cat-string '#\") (cons 'string x))
          ((x <- cat-id) (cons 'id x)))

   (comment/ (((str "//") x <- comment//) x))
   (comment// (('#\newline) 'comment)
              (((skip comment//)) 'comment))

   (comment* (((str "(*") x <- comment**) x))
   (comment** (((str "*)")) 'comment)
              (((skip comment**)) 'comment))

   (comment/* (((str "/*") x <- comment*/) x))
   (comment*/ (((str "*/")) 'comment)
              (((skip comment*/)) 'comment))

   (return (() 'empty))))
