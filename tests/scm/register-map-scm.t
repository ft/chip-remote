;; Copyright (c) 2011-2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (bitops)
             (chip-remote register-map))

(primitive-load "tests/test-tap-cfg.scm")

;; Define a register map for our fictional ‘foo-bar’ device. It has four
;; eight-bit registers. Not all bits are used for configuration.
(define-register-map foo-bar
  (#x0 (default-value #x54)
       (contents (m-divider 0 4)
                 (n-divider 4 4)))
  (#x1 (default-value #xd0)
       (contents (power-save 0 1)
                 (reference-divider 4 4)))
  (#x2 (default-value #xf0)
       (contents (gain 0 6)
                 (rate 6 2)))
  (#xf (default-value #xc0)
       (contents (self-destruct 0 1)
                 (deploy-coffee 2 3)
                 (read-paper 7 1))))

(with-fs-test-bundle
 (no-plan)
 (define-test "register-default finds first address"
   (pass-if-no-exception (register-default foo-bar-register-map #x0)))
 (define-test "register-default first default-value is correct"
   (pass-if-= (register-default foo-bar-register-map #x0)
              #x54))
 (define-test "register-default finds third address"
   (pass-if-no-exception (register-default foo-bar-register-map #x2)))
 (define-test "register-default third default-value is correct"
   (pass-if-= (register-default foo-bar-register-map #x2)
              #xf0)))
