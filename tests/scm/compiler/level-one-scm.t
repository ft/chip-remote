;; Copyright (c) 2016 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (ice-9 pretty-print)
             (chip-remote compiler level-one))

(primitive-load "tests/test-tap-cfg.scm")
(generate-level-one "tests/scm/compiler/fic007.table")

(with-fs-test-bundle
 (plan 4)

 ;; Test a few bits that the level one generator should create. Most of the
 ;; test on the contents is done by the "register-map-scm.t" test suite.
 (define-test "Is register-map defined?"
   (pass-if-true (defined? 'register-map)))
 (define-test "Is set-n-divider-bits defined?"
   (pass-if-true (defined? 'set-n-divider-bits)))
 (define-test "Is get-power-save-bits defined?"
   (pass-if-true (defined? 'get-power-save-bits)))
 (define-test "Is get-read-paper-bits defined?"
   (pass-if-true (defined? 'get-read-paper-bits))))
