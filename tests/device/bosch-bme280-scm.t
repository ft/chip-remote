;; -*- scheme -*-

;; Copyright (c) 2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices bosch bme280))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device bme280 #:multi-items '(reserved)))
