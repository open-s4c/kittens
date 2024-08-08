(define-library (kittens litc)
  (export parser

          ; litc record type
          litc-exists
          litc-preamble
          litc-proc-args
          litc-proc-id
          litc-proc-lines
          litc-proc-name
          litc-proc-reads
          litc-procs
          litc-vars
          make-litc)
  (import (scheme base)
          (scheme file)
          (scheme char)
          (scheme cxr)
          (kittens match)
          (kittens packrat)
          (only (srfi 1) filter))
  (include "litc/generic.scm")
  (include "litc/proc.scm")
  (include "litc/exists.scm")
  (include "litc/parser.scm")
  (include "litc/support.scm"))