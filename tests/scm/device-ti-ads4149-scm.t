;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test validate-device)
             (chip-remote devices texas-instruments ads4149))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (validate-device ads4149
                  #:multi-items '(reserved)))
