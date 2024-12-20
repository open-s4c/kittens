; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(define (seq-check left right)
  (match (list left right)
         ((('self 'set . "R") ('self 'set . "W")) #f)
         ((('self 'set . "W") ('self 'set . "R")) #f)
         ((('self 'set . "R") ('rel . "rf")) #f)
         ((('self 'set . "R") ('rel . "co")) #f)
         ((('rel . "rf") ('self 'set . "W")) #f)
         ((('rel . "co") ('self 'set . "R")) #f)
         ((('self 'set . A) ('self 'set . B))
          (let ((barriers '("RLX" "ACQ" "REL" "SC"))
                (ops '("W" "R" "XCHG" "CAS-S" "CAS-F")))
            (and (or (not (member A ops))
                     (not (member B ops))
                     (equal? A B))
                 (or (not (member A barriers))
                     (not (member B barriers))
                     (equal? A B)))))
         ((('self 'set . _) ('isect l r))
          (and (seq-valid `(seq ,left ,l))
               (seq-valid `(seq ,left ,r))))
         ((('isect l r) ('self 'set . _))
          (and (seq-valid `(seq ,l ,right))
               (seq-valid `(seq ,r ,right))))
         (else #t)))

(define (seq-left expr)
  (match expr
         (('seq left right) (seq-left left))
         (else expr)))

(define (seq-right expr)
  (match expr
         (('seq left right) (seq-right right))
         (else expr)))

(define (seq-valid expr)
  (match expr
         (('seq left right)
          (and (seq-valid left)
               (seq-valid right)
               (seq-check (seq-right left) (seq-left right))))

         (else #t)))

(define (isect-check left right)
  (match (list left right)
         ((('self 'set . "W") ('self 'set . "ACQ")) #f)
         ((('self 'set . "R") ('self 'set . "REL")) #f)
         ((('self 'set . "W") ('self 'set . "F")) #f)
         ((('self 'set . "R") ('self 'set . "F")) #f)
         (else #t)))

(define (isect-left expr)
  (match expr
         (('isect left right) (isect-left left))
         (else expr)))

(define (isect-right expr)
  (match expr
         (('isect left right) (isect-right right))
         (else expr)))

(define (isect-valid expr)
  (match expr
         (('self . expr) (isect-valid expr))

         (('isect left right)
          (and (isect-valid left)
               (isect-valid right)
               (isect-check (isect-right left) (isect-left right))))

         (('seq left right)
          (and (isect-valid left)
               (isect-valid right)))

         (else #t)))

(define (validate-expr expr is-cycle)
  (and (seq-valid expr)
       (isect-valid expr)
       (or (not is-cycle)
           (seq-valid `(seq ,(seq-right expr)
                            ,(seq-left expr))))))

