(define-library (defuse parser)
  (export parser
          file-generator
          )
  (import (scheme base)
          (scheme file)
          (scheme char)
          (rebottled packrat))
  (include "generic.scm")
  (include "proc.scm")
  (include "exists.scm")
  (include "parser.scm")
  (include "../cat/generator.scm"))
