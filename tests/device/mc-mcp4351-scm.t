;; -*- scheme -*-

;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices microchip mcp4351))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device mcp4351 #:multi-items '(reserved)))
