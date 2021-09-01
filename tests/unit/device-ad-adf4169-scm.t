;; -*- scheme -*-

;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices analog-devices adf4169))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device adf4169
                  #:multi-items '(address reserved)))
