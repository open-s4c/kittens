(define-library (pasm utils)
  (export select-func
          print-func
          combine-funcs
          export-func)
  (import (scheme small)
          (only (srfi 1) filter))
  (begin
    (define (select-func dump name)
      (let ((res (filter (lambda (func) (string=? (car func) name))
                         dump)))
        (when (null? res)
          (error 'select-func
                 (string-append "no function with name '" name "'")))
        (car res)))

    (define (print-func func)
      (display "--------------------------------\n")
      (display "function ")
      (display (car func))
      (newline)
      (newline)
      (for-each (lambda (y)
                  (display y)
                  (newline))
                (cadr func))
      (newline))



    (define (seq n)
      (let loop ((i 0) (lst '()))
        (if (= i n) lst (loop (+ i 1) (cons (- n i) lst)))))

    (define (widen-string str n)
      (let ((miss (- n (string-length str))))
        (string-append
         str
         (if (negative? miss) "" (make-string miss #\space)))))

    (define (export-func func)
      (let ((lines (cadr func)))
        (define (export-args args)
          (apply string-append
                 (map (lambda (arg last)
                        (string-append
                         (cond ((pair? arg)
                                (string-append
                                 "["
                                 (car arg)
                                 (if (and (number? (cdr arg))
                                          (zero? (cdr arg)))
                                     "]"
                                     (string-append ", " (cdr arg) "]"))))
                               (else arg))
                         (if last "" ", ")))
                      args
                      (map (lambda (i) (= i (length args)))
                           (seq (length args))))))
        (map (lambda (x)
               (string-append
                (widen-string (string-append (car x) ":") 6)
                (widen-string (cadr x) 8)
                (let ((args (caddr x)))
                  (when (not (null? args))
                    (export-args args)))))
             lines)))

    (define (widen-code lines)
      (let ((wid (apply max (map string-length lines))))
        (map (lambda (line)
               (widen-string line wid))
             lines)))

    (define (combine-funcs . funcs)
      (let* ((flines (map export-func funcs))

             ; introduce the processor name to each function
             (flines (map (lambda (lines id)
                            (cons (string-append "P" (number->string id) ":")
                                  (cons "" lines)))
                          flines
                          (seq (length flines))))
             (flines (map widen-code flines)))

        ; add | to all but first processor
        (let ((flines (cons (car flines)
                            (map (lambda (lines)
                                   (map (lambda (line) (string-append " | " line))
                                        lines))
                                 (cdr flines)))))

          ; join all processors line-by-line and append ";" to the line
          (apply map (lambda (x . xs)
                       (string-append (apply string-append x xs) " ;"))
                 flines))))

    ; --------------------------------------------------------------------------
    ))
