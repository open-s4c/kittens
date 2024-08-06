; The combined parsers in this file should be able to parse processes from
; litmus/C. They usually look as follows
;
;    P0 (volatile int* x, atomic_long* y) {
;        int r0 = *x;
;        *x = atomic_read(y);
;    }

(define args-parser
  (packrat-parser
   args

   (args (('#\( xs <- args) (cons 'args xs))
         ((x <- arg '#\, xs <- args) (cons x xs))
         ((x <- arg '#\)) (list x)))

   (arg (((str "volatile") T <- c-id '#\* n <- c-id) `(volatile ,T ,n))
        (((str "atomic_") T <- c-id '#\* n <- c-id) `(atomic ,T ,n)))))

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
    ((x <- (string-upto-char #\newline)) `(line ,x)))))

(define proc-parser
  (packrat-parser
   proc

   (proc ((p <- pid blank
             a <- args-parser blank
             c <- code-parser) `(proc ,p ,a ,c)))

   (pid (('#\P i <- natural) `(pid . ,i)))))
