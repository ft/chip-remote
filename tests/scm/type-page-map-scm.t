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
 (plan 8)

 (define-test "generate-page-map, call structure works"
   (pass-if-true (page-map? (generate-page-map
                             (0 #:table
                                (0 (#:contents (thing 0 4) (fish 4 4)))
                                (1 (#:contents (more 0 4) (stuff 4 4))))))))

 (let ((pm (generate-page-map (0 #:table
                                 (0 (#:default #x80
                                     #:contents (thing 0 4) (fish 4 4)))
                                 (1 (#:contents (more 0 4) (stuff 4 4))))
                              (1 #:table
                                 (0 (#:contents (thing* 0 4) (fish* 4 4)))
                                 (1 (#:contents (more* 0 4) (stuff* 4 4)))
                                 (2 (#:contents (stuff 0 8 #:default 666)))))))
   (define-test "page-map-address by page-address works #1"
     (pass-if-= (length (register-map-table (page-map-address pm 0)))
                2))
   (define-test "page-map-address by page-address works #2"
     (pass-if-= (length (register-map-table (page-map-address pm 1)))
                3))
   (define-test "page-map-address by page- and register address works"
     (pass-if-= (length (register-items (page-map-address pm 1 1)))
                2))
   (define-test "page-map-address by page-, reg-, and item address (name) works"
     (pass-if-eq? (item-name (page-map-address pm 1 2 'stuff))
                  'stuff))
   (define-test "page-map-address by page-, reg-, and item address (int) works"
     (pass-if-eq? (item-name (page-map-address pm 1 1 1))
                  'stuff*))
   (define-test "page-map-address by page-, reg-addr, name and count works"
     (pass-if-eq? (item-name (page-map-address pm 1 1 'more* 0))
                  'more*))
   (define-test "page-map-default works"
     (pass-if-equal? (page-map-default pm)
                     '((128 0) (0 0 666))))))
