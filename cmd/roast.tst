(import (scheme base)
        (kittens match)
        (kittens utils)
        (kittens command)
        (kittens test))

(include "roast.scm")


(test-group
 "events"
 (let ((ev (event 0 1000 0 4 5 6 7 8 9 0)))
   (test 1000 (event-eid ev))))

(test-group
 "get-tids"

 (test
  '(1 2 3)
  (get-tids (list (event #f #f 1 4 5 6 7 8 9 0)
                  (event #f #f 2 4 5 6 7 8 9 0)
                  (event #f #f 3 4 5 6 7 8 9 0))))

 (test
  '(2 3 1)
  (get-tids (list (event #f #f 2 #f #f #f #f #f #f #f)
                  (event #f #f 3 #f #f #f #f #f #f #f)
                  (event #f #f 2 #f #f #f #f #f #f #f)
                  (event #f #f 1 #f #f #f #f #f #f #f)))))
