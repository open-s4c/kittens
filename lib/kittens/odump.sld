(define-library (kittens odump)
  (export parser
          select-func
          combine-funcs
          export-func)

  (import (scheme base)
          (scheme cxr)
          (scheme char)
          (kittens packrat)
          (kittens utils)
          (only (srfi 1) filter))

  (include "odump/parser.scm")
  (include "odump/utils.scm"))
