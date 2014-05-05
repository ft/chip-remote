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
  (#xf (default-value #xc8)
       (contents (self-destruct 0 1)
                 (deploy-coffee 2 3)
                 (read-paper 7 1))))

(with-fs-test-bundle
 (plan 19)
 (define-test "register-default finds first address"
   (pass-if-no-exception (register-default foo-bar-register-map #x0)))
 (define-test "register-default first default-value is correct"
   (pass-if-= (register-default foo-bar-register-map #x0)
              #x54))
 (define-test "register-default finds third address"
   (pass-if-no-exception (register-default foo-bar-register-map #x2)))
 (define-test "register-default third default-value is correct"
   (pass-if-= (register-default foo-bar-register-map #x2)
              #xf0))

 (let ((reg-0 (register-default foo-bar-register-map #x0)))
   (define-test "#x0: m-divider: #x4"
     (pass-if-= (get-m-divider-bits reg-0)
                #x4))
   (define-test "#x0: n-divider: #x5"
     (pass-if-= (get-n-divider-bits reg-0)
                #x5))
   )

 (let ((reg-1 (register-default foo-bar-register-map #x1)))
   (define-test "#x1: power-safe: #x0"
     (pass-if-= (get-power-save-bits reg-1)
                #x0))
   (define-test "#x1: reference-divider: #xd"
     (pass-if-= (get-reference-divider-bits reg-1)
                #xd))
   )

 (let ((reg-2 (register-default foo-bar-register-map #x2)))
   (define-test "#x2: gain: #x30"
     (pass-if-= (get-gain-bits reg-2)
                #x30))
   (define-test "#x2: rate: #x3"
     (pass-if-= (get-rate-bits reg-2)
                #x3))
   )

 (let ((reg-f (register-default foo-bar-register-map #xf)))
   (define-test "#xf: self-destruct: #x0"
     (pass-if-= (get-self-destruct-bits reg-f)
                #x0))
   (define-test "#xf: deploy-coffee: #x2"
     (pass-if-= (get-deploy-coffee-bits reg-f)
                #x2))
   (define-test "#xf: read-paper: #x1"
     (pass-if-= (get-read-paper-bits reg-f)
                #x1)))

 (let ((regval-0 #x00)
       (regval-1 #xff))
   (define-test "set-gain: works #1"
     (pass-if-= (get-gain-bits (set-gain-bits regval-0 #x3f))
                #x3f))
   (define-test "set-gain: works #2"
     (pass-if-= (get-gain-bits (set-gain-bits regval-1 #x3f))
                #x3f))
   (define-test "set-gain: works #3"
     (pass-if-= (set-gain-bits regval-0 #x3f)
                #x3f))
   (define-test "set-gain: works #4"
     (pass-if-= (set-gain-bits regval-1 #x3f)
                #xff))
   (define-test "set-gain: works #5"
     (pass-if-= (set-gain-bits regval-0 #xaf)
                #x2f))
   (define-test "set-gain: works #6"
     (pass-if-= (set-gain-bits regval-1 #xaf)
                #xef))))
