; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme small)
        (kittens packrat)
        (kittens generator)
        (kittens test))

(include "expr.scm")
(include "stmt.scm")

(test-begin "stmt")

(let* ((g (token-generator
           ; let hb = (A | B)*C | R
           '((let) (id . hb) (=) (oparen) (set . A) (union)
                   (set . B) (cparen) (cart) (set . C) (union) (rel . R))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let hb (union (cart (union (set . A) (set . B))
                              (set . C))
                        (rel . R)))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '((empty) (id . "rmw") (isect) (oparen) (id . "fre") (seq) (id . "coe") (cparen) (acyclic) (id . "co") (union) (id . "rf") (union) (id . "fr") (union) (id . "po-loc"))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(empty (isect (rel . "rmw") (seq (rel . "fre") (rel . "coe"))))
        (parse-result-semantic-value r)))


(let* ((g (token-generator
           '((let) (id . "Marked") (=) (id . "RLX") (union) (id . "ACQ") (union) (id . "REL") (union) (id . "SC") (let) (id . "Plain") (=) (id . "Marked"))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "Marked" (union (set . "RLX") (union (set . "ACQ") (union (set . "REL") (set . "SC")))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '((let) (id . "Acq") (=) (id . "ACQ") (union) (oparen) (id . "SC") (isect) (id . "R") (cparen))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "Acq" (union (set . "ACQ") (isect (set . "SC") (set . "R"))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '(
             (let) (id . "Rel") (=) (id . "REL") (union) (oparen) (id . "SC") (isect) (id . "W") (cparen)
             )))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "Rel" (union (set . "REL") (isect (set . "SC") (set . "W"))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '(
             (let) (id . "dep") (=) (id . "ctrl") (union) (id . "addr") (union) (id . "data")
             )))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "dep" (union (rel . "ctrl") (union (rel . "addr") (rel . "data"))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '(
             (let) (id . "bob") (=) (obrack) (id . "ACQ") (cbrack) (seq) (id . "po") (union) (id . "po") (seq) (obrack) (id . "REL") (cbrack) (union) (obrack) (id . "SC") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "SC") (cbrack) (union) (id . "po") (seq) (obrack) (id . "SC") (isect) (id . "F") (cbrack) (seq) (id . "po") (union) (obrack) (id . "R") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "ACQ") (isect) (id . "F") (cbrack) (seq) (id . "po") (union) (id . "po") (seq) (obrack) (id . "REL") (isect) (id . "F") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "W") (isect) (id . "Marked") (cbrack)
             )))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "bob" (union (seq (self set . "ACQ") (rel . "po")) (union (seq (rel . "po") (self set . "REL")) (union (seq (self set . "SC") (seq (rel . "po") (self set . "SC"))) (union (seq (rel . "po") (seq (self isect (set . "SC") (set . "F")) (rel . "po"))) (union (seq (self set . "R") (seq (rel . "po") (seq (self isect (set . "ACQ") (set . "F")) (rel . "po")))) (seq (rel . "po") (seq (self isect (set . "REL") (set . "F")) (seq (rel . "po") (self isect (set . "W") (set . "Marked")))))))))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '(
             (let) (id . "ppo") (=) (id . "bob") (union) (obrack) (id . "Marked") (cbrack) (seq) (oparen) (id . "dep") (union) (id . "coi") (union) (id . "fri") (cparen) (seq) (obrack) (id . "W") (isect) (id . "Marked") (cbrack)
             )))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "ppo" (union (rel . "bob") (seq (self set . "Marked") (seq (union (rel . "dep") (union (rel . "coi") (rel . "fri"))) (self isect (set . "W") (set . "Marked"))))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '(
             (let) (id . "WRF-ppo") (=) (id . "po") (seq) (obrack) (id . "REL") (isect) (id . "F") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "W") (isect) (id . "Plain") (cbrack) (union) (obrack) (id . "Marked") (cbrack) (seq) (oparen) (id . "ctrl") (union) (id . "addr") (cparen) (seq) (obrack) (id . "W") (isect) (id . "Plain") (cbrack)
             )))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "WRF-ppo" (union (seq (rel . "po") (seq (self isect (set . "REL") (set . "F")) (seq (rel . "po") (self isect (set . "W") (set . "Plain"))))) (seq (self set . "Marked") (seq (union (rel . "ctrl") (rel . "addr")) (self isect (set . "W") (set . "Plain"))))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '(
             (let) (id . "hb") (=) (id . "ppo") (union) (id . "WRF-ppo") (union) (id . "rfe") (union) (id . "fre") (union) (id . "coe") (acyclic) (id . "hb"))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "hb" (union (rel . "ppo") (union (rel . "WRF-ppo") (union (rel . "rfe") (union (rel . "fre") (rel . "coe"))))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '((let) (id . "implied") (=) (id . "ppo") (isect) (id . "W") (cart) (id . "R"))))
       (r (stmt-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(let "implied" (isect (rel . "ppo") (cart (set . "W") (set . "R"))))
        (parse-result-semantic-value r)))

(test-end)
