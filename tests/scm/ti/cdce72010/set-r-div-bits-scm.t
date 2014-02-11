;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (ice-9 format)
             (test tap)
             (chip-remote devices ti cdce72010 prg))
(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 4)
 (define-test "set-bits-rdiv primary: set"
   (pass-if-true (logbit? 4 (set-bits-rdiv #x00000000 'primary #t))))

 (define-test "set-bits-rdiv secondary: set"
   (pass-if-true (logbit? 5 (set-bits-rdiv #x00000000 'secondary #t))))

 (define-test "set-bits-rdiv primary: unset"
   (pass-if-false (logbit? 4 (set-bits-rdiv #xffffffff 'primary #f))))

 (define-test "set-bits-rdiv secondary: unset"
   (pass-if-false (logbit? 5 (set-bits-rdiv #xffffffff 'secondary #f)))))
