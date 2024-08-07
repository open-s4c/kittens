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
    (if (= i n) lst (loop (+ i 1) (cons (- n i 1) lst)))))

(define (widen-string str n)
  (let ((miss (- n (string-length str))))
    (string-append
     str
     (if (negative? miss) "" (make-string miss #\space)))))


; remove any unused label and put used labels in exclusive lines
(define (fix-labels func)
  (let ((lines (cadr func)))

    ; 1. find which labels to keep
    #;(let ((keep '("118")))
        ; 2.  remove unused labels
        (define (remove-label line)
          (if (not (member (car line) keep))
              (cons "" (cdr line))
              line))
        (set! lines (map remove-label lines)))

    ; 3.  rename labels
    (define (rename-label line)
      (if (not (equal? "" (car line)))
          (cons (string-append "LC" (car line)) (cdr line))
          line))
    (set! lines (map rename-label lines))

    ; 4. rename targets
    (define (rename-targets line)
      (let ((mnm (cadr line)))
        (cond ((member mnm '("cbnz" "cbz"))
               (list (car line)
                     mnm
                     (let* ((args (caddr line))
                            (target (string-append "LC" (cadr args))))
                       (list (car args) target))))
              ((member mnm '("b" "b.ne" "b.eq"))
               (list (car line)
                     mnm
                     (let* ((args (caddr line))
                            (target (string-append "LC" (car args))))
                       (list target))))
              (else line))))
    (set! lines (map rename-targets lines))

    ; 5. separate used labels in exclusive lines
    (define (separate-label lines)
      (if (null? lines)
          '()
          (let ((cur (car lines))
                (rst (cdr lines)))
            (if (string=? "" (car cur))
                (cons cur (separate-label rst))
                (cons (list (car cur) "" '()) ; label line
                      (cons (cons "" (cdr cur)) ; current line w/o label
                            (separate-label rst)))))))
    (set! lines (separate-label lines))

    ; 6. remove nop and ret instructions
    (define (remove-nop-ret lst res)
      (if (null? lst)
          res
          (let* ((line (car lst))
                 (mnm (cadr line)))
            (if (member mnm '("ret" "nop"))
                (remove-nop-ret (cdr lst) res)
                (remove-nop-ret (cdr lst) (cons line res))))))
    (set! lines (reverse (remove-nop-ret lines '())))

    ; 7. done fixing labels, return function name and fixed lines
    (list (car func) lines)))

(define (export-func func)
  (let* ((func (fix-labels func))
         (lines (cadr func)))

    (define (export-args args)
      (apply string-append
             (map (lambda (arg last)
                    (string-append
                     (cond ((null? arg) "")
                           ((pair? arg)
                            (string-append
                             "["
                             (car arg)
                             (if (and (number? (cdr arg))
                                      (zero? (cdr arg)))
                                 "]"
                                 (string-append ", " (cdr arg) "]"))))
                           ((list? arg)
                            (string-append
                             "["
                             (car arg)
                             (if (and (number? (cadr arg))
                                      (zero? (cadr arg)))
                                 "]"
                                 (string-append ", " (cadr arg) "]"))
                             (if (= 2 (length arg)) "" (caddr arg))))
                           (else arg))
                     (if last "" ", ")))
                  args
                  (map (lambda (i) (= i (- (length args) 1)))
                       (seq (length args))))))
    (map (lambda (x)
           (string-append
            ; label
            (widen-string
             (string-append (car x)
                            (if (string=? "" (car x)) "" ":"))
             6)
            ; mnemonic
            (widen-string (cadr x) 8)
            ;arguments
            (let ((args (caddr x)))
              (export-args args))))
         lines)))

(define (widen-code lines)
  (let ((wid (apply max (map string-length lines))))
    (map (lambda (line)
           (widen-string line wid))
         lines)))

(define (combine-funcs . funcs)
  (let* ((flines (map export-func funcs))
         (longest (apply max (map length flines)))

         ; stretch code with empty lines
         (flines
          (map (lambda (lines)
                 (append lines (make-list (- longest (length lines)) "")))
               flines))

         ; introduce the processor name to each function
         (flines (map (lambda (lines id)
                        (cons (string-append "P" (number->string id))
                              lines))
                      flines
                      (seq (length flines))))
         (flines (map widen-code flines)))

    ; add | to all but first processor
    (let ((flines (cons (car flines)
                        (map (lambda (lines)
                               (map (lambda (line)
                                      (string-append " | " line))
                                    lines))
                             (cdr flines)))))

      ; join all processors line-by-line and append ";" to the line
      (apply map (lambda (x . xs)
                   (string-append (apply string-append x xs) " ;"))
             flines))))
