;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote device)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote page-map))

(primitive-load "tests/test-tap-cfg.scm")

(define pp (@@ (ice-9 pretty-print) pretty-print))

(with-fs-test-bundle
 (plan 1)

 (define-test "generate-page-map, call structure works"
   (pass-if-true
    (device?
     (generate-device #:name 'thing2000
                      #:page-map
                      (#xff #:table
                            (0 (#:contents (thing 0 4) (fish 4 4)))
                            (1 (#:contents (more 0 4) (stuff 4 4)))))))))
