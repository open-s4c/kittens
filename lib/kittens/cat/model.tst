; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme base)
        (kittens packrat)
        (kittens generator)
        (kittens test))

(include "expr.scm")
(include "stmt.scm")
(include "model.scm")

(test-begin "model")

(let* ((g (token-generator
           '((id . "TEST")
             (let) (id . hb) (=) (oparen) (set . A) (union) (set . B)
             (cparen) (cart) (set . C) (union) (rel . R))))
       (r (model-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(model (name "TEST")
                ((let hb (union (cart (union (set . A) (set . B))
                                      (set . C))
                                (rel . R)))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '((id . "TEST") (empty) (id . "rmw") (isect) (oparen) (id . "fre") (seq) (id . "coe") (cparen) (acyclic) (id . "co") (union) (id . "rf") (union) (id . "fr") (union) (id . "po-loc") (let) (id . "Marked") (=) (id . "RLX") (union) (id . "ACQ") (union) (id . "REL") (union) (id . "SC") (let) (id . "Plain") (=) (id . "Marked") (let) (id . "Acq") (=) (id . "ACQ") (union) (oparen) (id . "SC") (isect) (id . "R") (cparen) (let) (id . "Rel") (=) (id . "REL") (union) (oparen) (id . "SC") (isect) (id . "W") (cparen) (let) (id . "dep") (=) (id . "ctrl") (union) (id . "addr") (union) (id . "data") (let) (id . "bob") (=) (obrack) (id . "ACQ") (cbrack) (seq) (id . "po") (union) (id . "po") (seq) (obrack) (id . "REL") (cbrack) (union) (obrack) (id . "SC") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "SC") (cbrack) (union) (id . "po") (seq) (obrack) (id . "SC") (isect) (id . "F") (cbrack) (seq) (id . "po") (union) (obrack) (id . "R") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "ACQ") (isect) (id . "F") (cbrack) (seq) (id . "po") (union) (id . "po") (seq) (obrack) (id . "REL") (isect) (id . "F") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "W") (isect) (id . "Marked") (cbrack) (let) (id . "ppo") (=) (id . "bob") (union) (obrack) (id . "Marked") (cbrack) (seq) (oparen) (id . "dep") (union) (id . "coi") (union) (id . "fri") (cparen) (seq) (obrack) (id . "W") (isect) (id . "Marked") (cbrack) (let) (id . "WRF-ppo") (=) (id . "po") (seq) (obrack) (id . "REL") (isect) (id . "F") (cbrack) (seq) (id . "po") (seq) (obrack) (id . "W") (isect) (id . "Plain") (cbrack) (union) (obrack) (id . "Marked") (cbrack) (seq) (oparen) (id . "ctrl") (union) (id . "addr") (cparen) (seq) (obrack) (id . "W") (isect) (id . "Plain") (cbrack) (let) (id . "hb") (=) (id . "ppo") (union) (id . "WRF-ppo") (union) (id . "rfe") (union) (id . "fre") (union) (id . "coe") (acyclic) (id . "hb"))
           ))
       (r (model-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))
  (test '(model (name "TEST") ((empty (isect (rel . "rmw") (seq (rel . "fre") (rel . "coe")))) (acyclic (union (rel . "co") (union (rel . "rf") (union (rel . "fr") (rel . "po-loc"))))) (let "Marked" (union (set . "RLX") (union (set . "ACQ") (union (set . "REL") (set . "SC"))))) (let "Plain" (set . "Marked")) (let "Acq" (union (set . "ACQ") (isect (set . "SC") (set . "R")))) (let "Rel" (union (set . "REL") (isect (set . "SC") (set . "W")))) (let "dep" (union (rel . "ctrl") (union (rel . "addr") (rel . "data")))) (let "bob" (union (seq (self set . "ACQ") (rel . "po")) (union (seq (rel . "po") (self set . "REL")) (union (seq (self set . "SC") (seq (rel . "po") (self set . "SC"))) (union (seq (rel . "po") (seq (self isect (set . "SC") (set . "F")) (rel . "po"))) (union (seq (self set . "R") (seq (rel . "po") (seq (self isect (set . "ACQ") (set . "F")) (rel . "po")))) (seq (rel . "po") (seq (self isect (set . "REL") (set . "F")) (seq (rel . "po") (self isect (set . "W") (set . "Marked"))))))))))) (let "ppo" (union (rel . "bob") (seq (self set . "Marked") (seq (union (rel . "dep") (union (rel . "coi") (rel . "fri"))) (self isect (set . "W") (set . "Marked")))))) (let "WRF-ppo" (union (seq (rel . "po") (seq (self isect (set . "REL") (set . "F")) (seq (rel . "po") (self isect (set . "W") (set . "Plain"))))) (seq (self set . "Marked") (seq (union (rel . "ctrl") (rel . "addr")) (self isect (set . "W") (set . "Plain")))))) (let "hb" (union (rel . "ppo") (union (rel . "WRF-ppo") (union (rel . "rfe") (union (rel . "fre") (rel . "coe")))))) (acyclic (rel . "hb"))))
        (parse-result-semantic-value r)))

(let* ((g (token-generator
           '((id . "TEST") (empty) (id . "rmw") (isect) (id . "W") (cart) (id . "R"))))
       (r (model-parser (base-generator->results g))))
  (test-assert (parse-result-successful? r))

  (test '(model (name "TEST") ((empty (isect (rel . "rmw") (cart (set . "W") (set . "R"))))))
        (parse-result-semantic-value r)))

(test-end)


