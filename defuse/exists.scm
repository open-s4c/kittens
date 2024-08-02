
;; exists (0:r0=0 /\ 1:r0=0)

(define var-parser
  (packrat-parser
   var
   (var ((p <- c-id '#\: x <- c-id)
         `(read-var ,(string->number p) ,x))
        (('#\[ x <- c-id '#\])
         `(deref-var ,x)))))

(define equal-parser
  (packrat-parser
   equal
   (equal ((x <- var-parser '#\= y <- c-id) `(equal ,x ,y)))))

(define expr-parser
  (packrat-parser
   expr
   (expr ((x <- equal (str "/\\") xs <- expr) `(conj ,x ,xs))
         ((x <- equal) x))
   (equal ((blank x <- equal-parser) x)
          ((x <- equal-parser) x))))

(define exists-parser
  (packrat-parser
   exists
   (exists (((str "exists") (str "(") x <- expr (str ")")) `(exists . ,x)))
   (expr ((x <- expr-parser) x))))
