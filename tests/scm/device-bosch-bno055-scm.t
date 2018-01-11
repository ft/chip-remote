;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test validate-device)
             (chip-remote devices bosch bno055))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (validate-device bno055))
