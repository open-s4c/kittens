; -*- scheme -*-

(import (scheme small)
        (rebottled packrat)
        (chibi test)
        (kittens generator))

(test-group
 "generator"
 (define g (str-generator "ABCD"))

 (let-values (((p r) (g)))
   (test #\A (car r)))

 (let-values (((p r) (begin (g) (g) (g))))
   (test #\D (car r))))

