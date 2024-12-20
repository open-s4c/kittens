; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------

(import (scheme small)
        (kittens cat validator)
        (kittens test)
        (kittens debug))

(test-group
 "validate"

 (test-assert (not (seq-valid '(seq (self set . "R")
                                    (self set . "W")))))

 (test-assert (seq-valid '(seq (self set . "W")
                               (self set . "W"))))

 (test-assert (not (seq-valid '(seq (self set . "W")
                                    (seq (self set . "W")
                                         (self set . "R"))))))

 (test-assert (not (seq-valid '(seq (seq (self set . "W")
                                         (self set . "W"))
                                    (self set . "R")))))

 (test-assert (not (seq-valid '(seq (seq (self set . "W")
                                         (self set . "R"))
                                    (self set . "W")))))

 (test-assert (not (isect-valid
                    '(seq (rel . "po")
                          (seq (rel . "po")
                               (isect (self set . "W")
                                      (self set . "ACQ")))))))
 (test-assert (not (isect-valid
                    '(seq (rel . "po")
                          (seq (isect (self set . "REL")
                                      (self set . "F"))
                               (seq (rel . "po")
                                    (isect (self set . "W")
                                           (self set . "ACQ"))))))))
 )
