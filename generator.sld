(define-library (kittens generator)
  (export token-generator
          file-generator
          str-generator)
  (import (scheme base)
          (scheme file)
          (rebottled packrat))
  (include "generator.scm"))
