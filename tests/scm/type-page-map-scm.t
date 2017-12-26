;; -*- scheme -*-

;; Copyright (c) 2017 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote page-map))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 1)

 (define-test "generate-page-map, call structure works"
   (pass-if-true (page-map? (generate-page-map
                             (0 #:table
                                (0 (#:contents (thing 0 4) (fish 4 4)))
                                (1 (#:contents (more 0 4) (stuff 4 4)))))))))
