;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test validate-device)
             (chip-remote devices decawave dw1000))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (validate-device dw1000 #:multi-items '(reserved)))
