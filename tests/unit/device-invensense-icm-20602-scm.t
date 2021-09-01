;; -*- scheme -*-

;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (test validate-device)
             (chip-remote devices invensense icm-20602))

(init-test-tap!)

(with-fs-test-bundle
 (validate-device icm-20602 #:multi-items '(reserved)))
