; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define (visit-not expr action)
  ;(display "-- ")
  ;(display expr)
  ;(newline)
  (match expr
         ; DeMorgan's law
         (('isect left right)
          (visit-expr (cons 'not left)
                      (lambda (left)
                        (visit-expr (cons 'not right)
                                    (lambda (right)
                                      (action `(union ,left ,right)))))))
         ; DeMorgan's law
         (('union left right)
          (visit-expr (cons 'not left)
                      (lambda (left)
                        (visit-expr (cons 'not right)
                                    (lambda (right)
                                      (action `(isect ,left ,right)))))))
         (('seq left right)
          (visit-expr left
                      (lambda (left)
                        (visit-expr right
                                    (lambda (right)
                                      (action `(union (seq (not ,left) ,right)
                                                      (union (seq ,left (not ,right))
                                                             (seq (not ,left) (not ,right))))))))))
         ; cancel not not
         (('not . expr) (visit-expr expr action))

         ; continue
         (else (visit-expr expr (lambda (expr)
                                  (action `(not . ,expr)))))))


(define (visit-expr expr action)

  (define (push-back action op . rights)
    (cond ((null? rights) ; unary operation
           (lambda (expr) (action `(,op . ,expr))))

          ((= 1 (length rights)) ; binary operation
           (lambda (left)
             ; explore the right side before calling action
             (visit-expr (car rights)
                         ; once right and left side done, call action
                         (lambda (right) (action `(,op ,left ,right))))))

          (else (error "operations have at most two operands"))))

  (match expr
         (('union left right)
          (visit-expr left action)
          (visit-expr right action))

         (('seq left right)
          (visit-expr left (push-back action `seq right)))

         (('isect left right)
          (visit-expr left (push-back action `isect right)))

         (('cart left right)
          (visit-expr left (push-back action `cart right)))

         (('inv . expr)
          (visit-expr expr (push-back action `inv)))

         (('self . expr)
          (visit-expr expr (push-back action `self)))

         (('not . expr)
          (visit-not expr action))

         (('rel . label) (action expr))

         (('set . label) (action expr))

         (else (error "Unrecognized expression type" expr))))
