; -*- scheme -*-

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
  (test '(seq (self . (set . "FAA")) (rel . "rfx"))
        (parse-result-semantic-value r)))


(test-end)
