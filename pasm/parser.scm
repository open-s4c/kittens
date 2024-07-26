(define (new-reader parser)
  (define (generator p)
    (let ((ateof #f)
          (pos (top-parse-position "<?>")))
      (lambda ()
        (if ateof
            (values pos #f)
            (let ((x (read-char p)))
              (if (eof-object? x)
                  (begin
                    (set! ateof #t)
                    (values pos #f))
                  (let ((old-pos pos))
                    (set! pos (update-parse-position pos x))
                    (values old-pos (cons x x)))))))))

  (define (read-any p)
    (let ((result (parser (base-generator->results (generator p)))))
      (if (parse-result-successful? result)
          (parse-result-semantic-value result)
          (error 'parse-error "parse error"
                 (let ((e (parse-result-error result)))
                   (list 'parse-error
                         (parse-position->string (parse-error-position e))
                         (parse-error-expected e)
                         (parse-error-messages e)))))))
  (lambda maybe-port
    (read-any (if (pair? maybe-port)
                  (car maybe-port)
                  (current-input-port)))))


(define parser/internal
  (packrat-parser
   (begin




     (define (token str)
       (lambda (starting-results)
         (let loop ((pos 0) (results starting-results))
           (if (= pos (string-length str))
               (make-result str results)
               (let ((ch (parse-results-token-value results)))
                 (if (and ch (char=? ch (string-ref str pos)))
                     (loop (+ pos 1) (parse-results-next results))
                     (make-expected-result
                      (parse-results-position starting-results) str)))))))

     (define (identifier results)
       (let loop ((acc '()) (results results))
         (let ((ch (parse-results-token-value results)))
           (cond
             ((or (not ch) (memv ch '(\#[ #\] #\, #\: #\space #\newline #\tab #\< #\>)))
                  (make-result (list->string (reverse acc)) results))
              (else
               (loop (cons ch acc) (parse-results-next results)))))))

       (define (blank results)
         (let ((ch (parse-results-token-value results)))
           (cond ((and ch (memv ch '(#\space #\tab)))
                  (blank (parse-results-next results)))
                 ;((and ch (memv ch '(#\newline #\return)))
                 ; (return results))
                 (else (return results)))))

       (define (nl results)
         (let ((ch (parse-results-token-value results)))
           (cond
             ((and ch (memv ch '(#\newline #\return)))
              (return (parse-results-next results)))
             (else (return results)))))

       (define (to-nl results)
         (if (memv (parse-results-token-value results) '(#\newline #\return))
             (blank results)
             (to-nl (parse-results-next results))))

       prog)

     (prog ((nothing preamble nothing
                     distext nothing
                     funcs <- functions)
            funcs))

     (nothing ((blank nl nl nl nl) 'nothing)
              ((blank nl nl) 'nothing)
              ((blank nl) 'nothing)
              ((nl nl nl) 'nothing)
              ((nl nl) 'nothing)
              ((nl) 'nothing))

     (preamble ((file <- identifier '#\: blank identifier blank identifier blank fmt <- identifier)
                (cons file fmt)))
     (distext ((identifier blank identifier blank identifier blank .text <- identifier '#\:)
               (unless (equal? .text ".text")
                 (error "not at .text"))
               'nothing))

     (functions ((func <- function nl nl funcs <- functions) (cons func funcs))
                ((func <- function) (list func)))

     (function ((name <- fname lns <- lines) (list name lns)))

     (fname ((identifier blank '#\< name <- identifier '#\> '#\: nl)
             name))

     (lines ((ln <- line nl lns <- lines) (cons ln lns))
            ((ln <- line) (list ln)))

     (line ((blank lab <- label blank
                   code <- identifier blank
                   mnm <- identifier blank
                   args <- arglist)
            (list lab mnm args))
           ((blank lab <- label
                   code <- identifier
                   mnm <- identifier)
            (list lab mnm))
           )

     (arglist ((args <- arglist/ne) args)
              (() '()))
     (arglist/ne ((arg <- argval '#\, blank args <- arglist/ne) (cons arg args))
                 ((arg <- argval blank '#\< identifier '#\> blank comment) (list arg))
                 ((arg <- argval blank '#\< identifier '#\>) (list arg))
                 ((arg <- argval blank comment) (list arg))
                 ((arg <- argval) (list arg)))

     (argval ((arg <- argoff) arg)
             ((arg <- identifier) arg))

     (argoff (( '#\[ arg <- identifier '#\, blank off <- identifier '#\])
              (cons arg off))
             (( '#\[ arg <- identifier '#\])
              (cons arg 0)))


     (label ((lab <- identifier '#\:) lab))

     (comment (((token "//") b <- to-nl) b)
              (() 'whitespace))

     (return (() 'empty))
     ))

  (define parser (new-reader parser/internal))
