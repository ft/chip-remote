;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote page-map))

(init-test-tap!)

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
                                 (2 (#:contents (stuff 0 8 #:default 111)))))))
   (define-test "page-map-ref by page-address works #1"
     (pass-if-= (length (register-map-table (page-map-ref pm 0)))
                2))
   (define-test "page-map-ref by page-address works #2"
     (pass-if-= (length (register-map-table (page-map-ref pm 1)))
                3))
   (define-test "page-map-ref by page- and register address works"
     (pass-if-= (length (register-items (page-map-ref pm 1 1)))
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
                     '((0 . ((0 . 128)
                             (1 .   0)))
                       (1 . ((0 .   0)
                             (1 .   0)
                             (2 . 111))))))))
