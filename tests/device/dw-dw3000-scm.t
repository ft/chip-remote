;; -*- scheme -*-

;; Copyright (c) 2023 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices decawave dw3000))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device dw3000
                  #:multi-items '(reserved)
                  #:allow-holes? #t))
