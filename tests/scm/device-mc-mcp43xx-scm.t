;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test validate-device)
             (chip-remote devices microchip mcp43xx))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (validate-device mcp43xx #:multi-items '(reserved)))
