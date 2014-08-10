;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (chip-remote legacy ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 2)
 (define-test "set-device-power-down-bit"
   (pass-if-true (logbit? 11 (set-device-power-down-bit #x00000000))))
 (define-test "clear-device-power-down-bit"
   (pass-if-false (logbit? 11 (clear-device-power-down-bit #xffffffff)))))
