;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map))

(primitive-load "tests/test-tap-cfg.scm")

(define *register-one* (generate-register #:contents
                                          (foo 0 4)
                                          (bar 4 4)))
(define *register-two* (generate-register #:contents
                                          (baz 0 4)
                                          (boz 4 4)))

(define pp (@@ (ice-9 pretty-print) pretty-print))

(with-fs-test-bundle
 (plan 1)

 (define-test "generate-register, call structure works"
   (pass-if-true (register-map? (generate-register-map
                                 #:table
                                 (0 (#:contents (thing 0 4) (fish 4 4)))
                                 #:table*
                                 (1 *register-one*)
                                 (2 *register-two*))))))
