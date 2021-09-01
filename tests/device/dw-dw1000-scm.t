;; -*- scheme -*-

;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices decawave dw1000))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device dw1000 #:multi-items '(reserved)))
