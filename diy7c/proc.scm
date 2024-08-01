(define (str str)
  (lambda (starting-results)
    (let loop ((pos 0)
               (first-pos 0)
               (results starting-results))
      (if (= (- pos first-pos) (string-length str))
          (make-result str results)
          (let ((ch (parse-results-token-value results)))
            (cond  ((and ch (= pos first-pos) (memv ch '(#\space #\tab)))
                    (loop (+ pos 1) (+ 1 first-pos) (parse-results-next results)))
                   ((and ch (char=? ch (string-ref str (- pos first-pos))))
                    (loop (+ pos 1) first-pos (parse-results-next results)))
                   (else
                    (make-expected-result
                     (parse-results-position starting-results) str))))))))

(define (c-id starting-results)
  (let loop ((acc '()) (results starting-results))
    (let ((ch (parse-results-token-value results)))
      (cond
        ((and ch (null? acc) (memv ch '(#\space #\tab)))
         (loop acc (parse-results-next results)))
        ((and ch (char-ci>=? ch #\A) (char-ci<=? ch #\Z))
         (loop (cons ch acc) (parse-results-next results)))
        ((and ch (char-ci>=? ch #\a) (char-ci<=? ch #\z))
         (loop (cons ch acc) (parse-results-next results)))
        ((and ch (char-ci>=? ch #\0) (char-ci<=? ch #\9))
         (loop (cons ch acc) (parse-results-next results)))
        ((and ch (memv ch '( #\_ )))
         (loop (cons ch acc) (parse-results-next results)))
        ((null? acc)
         (make-expected-result
          (parse-results-position starting-results) 'id))
        (else
         (make-result (list->string (reverse acc)) results))))))

(define (natural starting-results)
  (let loop ((acc '()) (results starting-results))
    (let ((ch (parse-results-token-value results)))
      (cond
        ((and ch (memv ch '(#\space #\tab #\newline)))
         (make-result (string->number (list->string (reverse acc)))
                      results))
        ((and ch (char-ci>=? ch #\0) (char-ci<=? ch #\9))
         (loop (cons ch acc) (parse-results-next results)))
        (else
         (make-expected-result
          (parse-results-position starting-results) 'natural))))))

(define (string-enclosed start-char end-char)
  (lambda (starting-results)
    (let loop ((acc '()) (results starting-results))
      (let ((ch (parse-results-token-value results)))
        (cond
          ((and ch (null? acc) (memv ch '(#\space #\tab #\newline)))
           (loop acc (parse-results-next results)))
          ((and ch (null? acc) (char-ci=? start-char))
           (loop (cons ch acc) (parse-results-next results)))
          ((and ch (char-ci=? ch end-char))
           (let* ((acc (reverse acc))
                  (acc (cdr acc))) ; remove start-char
             (make-result (list->string acc)
                          (parse-results-next results))))
          ((and ch (not (null? acc)))
           (loop (cons ch acc) (parse-results-next results)))
          (else
           (make-expected-result
            (parse-results-position starting-results)
            'string-enclosed)))))))

(define (string-upto-char . end-chars)
  (lambda (starting-results)
    (let loop ((acc '()) (results starting-results))
      (let ((ch (parse-results-token-value results)))
        (cond
          ((and ch (null? acc) (memv ch '(#\space #\tab)))
           (loop acc (parse-results-next results)))
          ((or (not ch) (memv ch end-chars))
           (if (null? acc)
               (make-expected-result
                (parse-results-position starting-results)
                'string-upto-char)
               (make-result (list->string (reverse acc)) results)))
          (else (loop (cons ch acc) (parse-results-next results))))))))

(define blank
  (packrat-parser
   blank

   (blank ((blank/one blank) '())
          ((blank/one) '()))

   (blank/one (('#\space) '())
              (('#\tab) '())
              (('#\newline) '()))))

(define args-parser
  (packrat-parser
   args

   (args (('#\( xs <- args) (cons 'args xs))
         ((x <- arg '#\, xs <- args) (cons x xs))
         ((x <- arg '#\)) (list x)))

   (arg (((str "volatile") T <- c-id '#\* n <- c-id) `(volatile ,T ,n))
        (((str "atomic_") T <- c-id '#\* n <- c-id) `(atomic ,T ,n))
        )))

(define code-parser
  (packrat-parser
   code

   (code ((x <- lines) (cons 'code x)))

   (lines ((x <- line (str "\n") xs <- lines) (cons x xs))
          ((x <- line) (list x)))

   (line
    ; line with a variable declaration
    ((T <- c-id v <- c-id (str "=") '#\space
        rest <- (string-upto-char #\newline))
     (let* ((linestr (string-append T " " v " = " rest))
            (ln `(line ,linestr))
            (dl `(decl ,T ,v)))
       `(local ,dl ,ln)))

    ; any other kind of line
    ((x <- (string-upto-char #\newline)) `(line ,x)))
   ))

(define proc-parser
  (packrat-parser
   proc

   (proc ((p <- pid blank
             a <- args-parser blank
             c <- code-parser) `(proc ,p ,a ,c)))

   (pid (('#\P i <- natural) `(pid . ,i)))

   ))
