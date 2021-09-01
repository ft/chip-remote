;; -*- scheme -*-

;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices analog-devices adf4158))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device adf4158
                  #:multi-items '(address reserved)))
