(define-library (kittens odump)
  (export parser
          select-func
          print-func
          combine-funcs
          export-func)
  (import (scheme small)
          (rebottled packrat)
          (only (srfi 1) filter))
  (include "odump/parser.scm")
  (include "odump/utils.scm"))
