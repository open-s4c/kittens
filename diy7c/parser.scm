(define (parse-or-die parser generator)
  (let ((result (parser (base-generator->results generator))))
    (if (parse-result-successful? result)
        (parse-result-semantic-value result)
        (error 'parse-error "parse error"
               (let ((e (parse-result-error result)))
                 (list 'parse-error
                       (parse-position->string (parse-error-position e))
                       (parse-error-expected e)
                       (parse-error-messages e)))))))

(define (tokenize-cat filename)
  (let ((g (file-generator filename)))
    (parse-or-die lexer g)))
