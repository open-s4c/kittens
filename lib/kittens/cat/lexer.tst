; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme small)
        (kittens packrat)
        (kittens generator)
        (kittens test))

(include "lexer.scm")

(test-begin "lexer")

(define (parse g)
  (lexer (base-generator->results g)))

(let ((input "NOT_SC"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "NOT_SC")) (parse-result-semantic-value r)))

(let ((input "A B 3.c"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "A B 3.c "))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "A B   3.c"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "   A B 3.c"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((id . "A") (id . "B") (id . "3.c")) (parse-result-semantic-value r)))

(let ((input "[XCHG]&((([W];rf;[R]);co)&ext);(co&ext)"))
  (define r (parse (str-generator input)))
  (test-assert (parse-result-successful? r))
  (test '((obrack) (id . "XCHG") (cbrack) (isect) (oparen) (oparen) (oparen) (obrack) (id . "W") (cbrack) (seq) (id . "rf") (seq) (obrack) (id . "R") (cbrack) (cparen) (seq) (id . "co") (cparen) (isect) (id . "ext") (cparen) (seq) (oparen) (id . "co") (isect) (id . "ext") (cparen))
        (parse-result-semantic-value r)))

(let ((text (code-append



             "(** Atomicity **) "
             "empty rmw & (fre;coe) "

             "(** SC per location **) "
             "acyclic co | rf | fr | po-loc "

             "let Marked = RLX | ACQ | REL | SC "
             "let Plain = Marked "
             "let Acq = ACQ | (SC & R) "
             "let Rel = REL | (SC & W) "

             "(** Ordering **) "
             "let dep = ctrl | addr | data "

             "(* Barrier Ordered-Before: barrier ordering rules *) "
             "let bob = [ACQ];po | po;[REL] | [SC];po;[SC] | po;[SC & F];po | [R];po;[ACQ & F];po | po;[REL & F];po;[W & Marked] "

             "(* Preserved Program-Order: these are never visibly reordered by compiler and hardware. "
             "   Includes both barrier ordering, and dependency ordering + same-address ordering *) "
             "let ppo = bob | [Marked];(dep | coi | fri);[W & Marked] "

             "let WRF-ppo = po;[REL & F];po;[W & Plain] | [Marked];(ctrl | addr);[W & Plain] "

             "let hb = ppo | WRF-ppo | rfe | fre | coe "
             "acyclic hb "



             )))
  (define r (parse (str-generator text)))
  (test-assert (parse-result-successful? r))
  (test '((empty) (id . "rmw") (isect) (oparen) (id . "fre") (seq) (id . "coe") (cparen) (acyclic) (id . "co") (union) (id . "rf") (union) (id . "fr") (union) (id . "po-loc") (let) (id . "Marked") (=) (id . "RLX") (union) (id . "ACQ") (union) (id . "REL") (union) (id . "SC") (let) (id . "Plain") (=) (id . "Marked") (let) (id . "Acq") (=) (id . "ACQ") (union) (oparen) (id . "SC") (isect) (id . "R") (cparen) (let) (id . "Rel") (=) (id . "REL") (union) (oparen) (id . "SC") (isect) (id . "W") (cparen) (let) (id . "dep") (=) (id . "ctrl") (union) (id . "addr") (union) (id . "data") (let) (id . "bob") (=) (obrack) (id . "ACQ") (cbrack) (seq) (id . "po") (union) (id . "po") (seq) (obrack) (id . "REL") (cbrack) (union) (obrack) (id . "SC") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "SC") (cbrack) (union) (id . "po") (seq) (obrack) (id . "SC") (isect) (id . "F") (cbrack) (seq) (id . "po") (union) (obrack) (id . "R") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "ACQ") (isect) (id . "F") (cbrack) (seq) (id . "po") (union) (id . "po") (seq) (obrack) (id . "REL") (isect) (id . "F") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "W") (isect) (id . "Marked") (cbrack) (let) (id . "ppo") (=) (id . "bob") (union) (obrack) (id . "Marked") (cbrack) (seq) (oparen) (id . "dep") (union) (id . "coi") (union) (id . "fri") (cparen) (seq) (obrack) (id . "W") (isect) (id . "Marked") (cbrack) (let) (id . "WRF-ppo") (=) (id . "po") (seq) (obrack) (id . "REL") (isect) (id . "F") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "W") (isect) (id . "Plain") (cbrack) (union) (obrack) (id . "Marked") (cbrack) (seq) (oparen) (id . "ctrl") (union) (id . "addr") (cparen) (seq) (obrack) (id . "W") (isect) (id . "Plain") (cbrack) (let) (id . "hb") (=) (id . "ppo") (union) (id . "WRF-ppo") (union) (id . "rfe") (union) (id . "fre") (union) (id . "coe") (acyclic) (id . "hb"))


        (parse-result-semantic-value r)))






(let ((text (code-append
             "SC"
             "include cos.cat"
             "// a nice comment"
             "another"
             ""
             "(* something useless *)"
             "final"
             ""
             "(* here is a comment with multiple lines i"
             "and it also will terminate using // in the same line as *)"
             ""
             "include \"string.cat\" //"
             ""
             "( A )[ ~A|B ^-1 ] (* hello world *)"
             ""
             "let hx = [R] D"
             ""
             "acyclic hb"
             )))
  (define r (parse (str-generator text)))
  (test-assert (parse-result-successful? r))
  (test '((id . "SC")
          (include) (id . "cos.cat")
          (id . "another") (id . "final")
          (include) (string . "string.cat")
          (oparen) (id . "A") (cparen)
          (obrack) (not) (id . "A") (union) (id . "B") (inv) (cbrack)
          (let) (id . "hx") (=)
          (obrack) (id . "R") (cbrack) (id ."D")
          (acyclic) (id . "hb"))
        (parse-result-semantic-value r)))

(test-end)
