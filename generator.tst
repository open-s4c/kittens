; -*- scheme -*-

(import (scheme small)
        (rebottled packrat)
        (kittens generator)
        (kittens test))

(test-group
 "generator"
 (define g (str-generator "ABCD"))

 (let-values (((p r) (g)))
   (test #\A (car r)))

 (let-values (((p r) (begin (g) (g) (g))))
   (test #\D (car r))))

