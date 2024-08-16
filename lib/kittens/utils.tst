(import (scheme base)
        (kittens test)
        (kittens utils))

(test-group
 "unique"
 (test '(1 2 3 4) (unique '(1 1 2 3 2 4 4 1))))
