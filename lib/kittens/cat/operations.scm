; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define (normalize-expr expr)
  (match expr
         (('union left right)
          `(union ,(normalize-expr left)
                  ,(normalize-expr right)))

         (('isect left right)
          `(isect ,(normalize-expr left)
                  ,(normalize-expr right)))

         (('seq left right)
          `(seq ,(normalize-expr left)
                ,(normalize-expr right)))

         (('not . expr)
          `(not . ,(normalize-expr expr)))

         (('cart ('set . left) ('set . right))
          `(seq (self set . ,left)
                (seq (rel . "all")
                     (self set . ,right))))

         (('self . expr)
          (match expr
                 (('not . expr)
                  (normalize-expr `(not self . ,expr)))

                 (('isect left right)
                  (normalize-expr
                   `(isect (self . ,left)
                           (self . ,right))))
                 (else
                  `(self . ,(normalize-expr expr)))))

         (('inv . expr)
          `(inv . ,(normalize-expr expr)))

         (else expr)))

(define (model-statements model)
  (caddr model))

;; returns a list of statements of the given type (sym)
(define (select-statements model sym)
  (filter (lambda (stmt) (eq? sym (car stmt)))
          (model-statements model)))

(define (collect-definitions/let stmts)
  (let ((defm (make-hash-table equal? string-hash 1024)))
    (for-each (lambda (stmt)
                (let ((label (cadr stmt))
                      (expr (caddr stmt)))
                  (hash-table-set! defm label expr)))
              stmts)
    defm))

;; returns a map of definitions from given a list of statements
;; statements starting with 'let are processed into the map, other
;; statements are ignored
(define (collect-definitions model)
  (collect-definitions/let (select-statements model 'let)))

;; expands an expression given a definitons-map
(define (expand-expr expr defm)
  (match expr
         (('union left right)
          (let* ((left (expand-expr left defm))
                 (right (expand-expr right defm)))
            (list 'union left right)))

         (('seq left right)
          (let* ((left (expand-expr left defm))
                 (right (expand-expr right defm)))
            (list 'seq left right)))

         (('cart left right)
          (let* ((left (expand-expr left defm))
                 (right (expand-expr right defm)))
            (list 'cart left right)))

         (('isect left right)
          (let* ((left (expand-expr left defm))
                 (right (expand-expr right defm)))
            (list 'isect left right)))

         (('inv . expr)
          (cons 'inv (expand-expr expr defm)))

         (('self . expr)
          (cons 'self (expand-expr expr defm)))

         (('not . expr)
          (cons 'not (expand-expr expr defm)))

         (('rel . label)
          (if (hash-table-exists? defm label)
              ; remove the label from the defm to avoid recursion.
              (let ((expr (hash-table-ref defm label)))
                (hash-table-delete! defm label)
                (let ((expd (expand-expr expr defm)))
                  (hash-table-set! defm label expr)
                  expd))
              expr))

         (('set . label)
          (if (hash-table-exists? defm label)
              ; remove the label from the defm to avoid recursion.
              (let ((expr (hash-table-ref defm label)))
                (hash-table-delete! defm label)
                (let ((expd (expand-expr expr defm)))
                  (hash-table-set! defm label expr)
                  expd))
              expr))

         (else (error "Unrecognized expression type" expr))))
