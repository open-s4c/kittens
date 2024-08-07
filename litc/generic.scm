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
          ((and ch (null? acc) (char-ci=? ch start-char))
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
