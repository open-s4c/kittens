; ------------------------------------------------------------------------------
; Copyright (C) Huawei Technologies Co., Ltd. 2024. All rights reserved.
; SPDX-License-Identifier: 0BSD
; ------------------------------------------------------------------------------
(import (scheme base)
        (kittens test)
        (kittens utils))

(test-group
 "unique"
 (test '(1 2 3 4) (unique '(1 1 2 3 2 4 4 1))))
