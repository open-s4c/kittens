(define-library (diy7c parser)
  (export lexer
          file-generator
          )
  (import (scheme base)
          (scheme file)
          (scheme char)
          (rebottled packrat))
  (include "proc.scm")
  (include "lexer.scm")
  (include "../cat/generator.scm")
  (include "parser.scm"))
