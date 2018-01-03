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
 (plan 6)

 (define-test "generate-register, call structure works"
   (pass-if-true (register-map? (generate-register-map
                                 #:table
                                 (0 (#:contents (thing 0 4) (fish 4 4)))
                                 #:table*
                                 (1 *register-one*)
                                 (2 *register-two*)))))

 (let ((rm (generate-register-map #:table
                                  (0 (#:contents (thing 0 4) (fish 4 4)))
                                  #:table*
                                  (1 *register-one*)
                                  (2 *register-two*))))
   (define-test "register-map-address reg1 works"
     (pass-if-equal? (register-map-address rm 1)
                     *register-one*))

   (define-test "register-map-address item by name works"
     (pass-if-eq? (item-name (register-map-address rm 1 'bar))
                  'bar))
   (define-test "register-map-address item by index works"
     (pass-if-eq? (item-name (register-map-address rm 1 0))
                  'foo))
   (define-test "register-map-address item by name and index works"
     (pass-if-eq? (item-name (register-map-address rm 1 'foo 0))
                  'foo))
   (define-test "register-map-ref works"
     (pass-if-eq? (item-name (register-map-ref rm 'boz))
                  'boz))))
