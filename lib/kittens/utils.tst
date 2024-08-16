(import (scheme base)
        (kittens test)
        (kittens utils))

(test-group
 "unique"
 (test '(4 3 2 1) (unique '(1 1 2 3 2 4 4 1))))
