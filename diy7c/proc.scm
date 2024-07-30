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
        (if (memv ch end-chars)
            (make-result (list->string (reverse acc)) results)
            (loop (cons ch acc) (parse-results-next results)))))))

(define proc-parser
  (packrat-parser
   proc

   (proc ((p <- pid
             (string-upto-char #\() '#\(
             a <- args '#\)
             c <- (string-enclosed #\{ #\}))
          (list p `(args ,@a) `(code ,c))))

   (pid (('#\P i <- natural) `(pid . ,i)))

   (args ;(('#\( x <- arg '#\)) x)
    ;(('#\( x <- arg '#\, xs <- args '#\)) (cons x xs))
    ((x <- arg '#\, xs <- args) (cons x xs))
    ((x <- arg) (list x)))

   (arg ((x <- (string-upto-char #\, #\))) x))))
