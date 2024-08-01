(define parser
  (packrat-parser
   (begin

     (define (str str)
       (lambda (starting-results)
         (let loop ((pos 0) (results starting-results))
           (if (= pos (string-length str))
               (make-result str results)
               (let ((ch (parse-results-token-value results)))
                 (if (and ch (char=? ch (string-ref str pos)))
                     (loop (+ pos 1) (parse-results-next results))
                     (make-expected-result
                      (parse-results-position starting-results) str)))))))


     ; according to pg.X whatever
     ; TODO: first char must be text?
     (define (cat-id starting-results)
       (let loop ((acc '()) (results starting-results))
         (let ((ch (parse-results-token-value results)))
           (cond
             ((and ch (null? acc) (memv ch '(#\space #\tab #\newline)))
              (loop acc (parse-results-next results)))
             ((and ch (char-ci>=? ch #\A) (char-ci<=? ch #\Z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\a) (char-ci<=? ch #\z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\0) (char-ci<=? ch #\9))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (memv ch '( #\_ #\. #\- )))
              (loop (cons ch acc) (parse-results-next results)))
             ((null? acc)
              (make-expected-result
               (parse-results-position starting-results) 'id))
             (else
              (make-result (list->string (reverse acc)) results))))))

     (define (line starting-results)
       (let loop ((acc '()) (results starting-results))
         (let ((ch (parse-results-token-value results)))
           (cond
             ((and ch (memv ch '(#\newline)))
              (make-result (list->string (reverse acc)) results))
             ((and ch (not (memv ch '(#\{ #\}))))
              (loop (cons ch acc) (parse-results-next results)))
             (else
              (make-expected-result
               (parse-results-position starting-results) 'line))))))

     (define (cat-string results)
       (let loop ((acc '()) (results results))
         (let ((ch (parse-results-token-value results)))
           (cond
             ((and ch (char-ci>=? ch #\A) (char-ci<=? ch #\Z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\a) (char-ci<=? ch #\z))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (char-ci>=? ch #\0) (char-ci<=? ch #\9))
              (loop (cons ch acc) (parse-results-next results)))
             ((and ch (memv ch '( #\_ #\. #\- #\space #\tab)))
              (loop (cons ch acc) (parse-results-next results)))
             (else
              (make-result (list->string (reverse acc)) results))))))

     (define (skip rule)
       (lambda (results)
         (rule (parse-results-next results))))
     file)

   (file ((p <- preamble
             v <- vars
             x <- procs
             e <- exists)
          (list `(preamble . ,p)
                `(vars ,v)
                `(procs ,@x)
                e)))

   (preamble ((x <- preamble-line '#\newline xs <- preamble) (cons x xs))
             ((x <- preamble-line '#\newline) (list x)))

   (preamble-line ((x <- line) x))

   (vars ((v <- (string-enclosed #\{ #\})) v))

   ;(exists (('#\newline (str "exists") x <- (string-enclosed #\( #\))) `(exists . ,x)))
   (exists ((blank x <- exists-parser) x))

   (procs ((x <- proc xs <- procs) (cons x xs))
          ((x <- proc) (list x)))

   (proc ((blurb x <- proc-parser) x)
         ((x <- proc-parser) x))

   (lines ((x <- line '#\newline xs <- lines) (cons x xs))
          ((x <- line) (list x)))

   (blurb ((blurb/one blurb) '())
          ((blurb/one) '()))

   (blurb/one ((comment/) '())
              ((comment/*) '())
              (('#\space) '())
              (('#\tab) '())
              (('#\newline) '()))

   (comment/ (((str "//") x <- comment//) x))
   (comment// (('#\newline) 'comment)
              (((skip comment//)) 'comment))

   (comment* (((str "(*") x <- comment**) x))
   (comment** (((str "*)")) 'comment)
              (((skip comment**)) 'comment))

   (comment/* (((str "/*") x <- comment*/) x))
   (comment*/ (((str "*/")) 'comment)
              (((skip comment*/)) 'comment))

   (return (() 'empty))
   ))
