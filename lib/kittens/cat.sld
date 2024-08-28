(define-library (kittens cat)
  (export lexer
          expr-parser
          model-parser)
  (import (scheme base)
          (scheme file)
          (scheme char)
          (kittens packrat))
  (include "cat/lexer.scm")
  (include "cat/expr.scm")
  (include "cat/stmt.scm")
  (include "cat/model.scm"))
