; -*- scheme -*-

(import (scheme small)
    (rebottled packrat)
    (chibi test))

(include "generator.scm")
(test-begin "generator")

(define g (str-generator "ABCD"))

(let-values (((p r) (g)))
	(test #\A (car r)))

(let-values (((p r) (begin (g) (g) (g))))
	(test #\D (car r)))

(test-end)

