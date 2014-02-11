;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (chip-remote devices ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 2)
 (define-test "set-pll-power-down-bit"
   (pass-if-true (logbit? 23 (set-pll-power-down-bit #x00000000))))
 (define-test "clear-pll-power-down-bit"
   (pass-if-false (logbit? 23 (clear-pll-power-down-bit #xffffffff)))))
