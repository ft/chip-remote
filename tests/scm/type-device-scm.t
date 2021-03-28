;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (chip-remote device)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote page-map))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
    (plan 18)

  (define-test "generate-page-map, call structure works"
    (pass-if-true
     (device?
      (generate-device #:name 'thing2000
                       #:page-map
                       (#xff #:table
                             (0 (#:contents (thing 0 4) (fish 4 4)))
                             (1 (#:contents (more 0 4) (stuff 4 4))))))))

  (let* ((p0 (generate-register-map #:table (0 (#:contents (foo 0 4)))))
         (p1 (generate-register-map #:table (0 (#:contents (bar 0 4)))))
         (pm (generate-page-map (0 p0) (1 p1)))
         (dev (generate-device #:name 'thing #:page-map* pm)))
    (define-test "page-map table length correct with #:page-map*"
      (pass-if-= 2 (length (page-map-table (device-page-map dev))))))

  (let* ((dev (generate-device
               #:page-map
               (0 #:table
                  (0 (#:default 128
                                #:contents (thing 0 4) (fish 4 4)))
                  (1 (#:contents (more 0 4) (stuff 4 4))))
               (1 #:table
                  (0 (#:contents (thing* 0 4) (fish* 4 4)))
                  (1 (#:contents (more* 0 4) (stuff* 4 4)))
                  (2 (#:contents (stuff 0 8 #:default 111))))
               (10 #:table
                   (8    (#:contents (there 0 4) (really 4 4)))
                   (200  (#:contents (is 0 4) (lots 4 4)))
                   (1025 (#:contents (more-things 0 8 #:default 101))))))
         (dev-default (device-default dev)))
    (define-test "device-address by page-address works #1"
      (pass-if-= (length (register-map-table (device-address dev 0)))
                 2))
    (define-test "device-address by page-address works #2"
      (pass-if-= (length (register-map-table (device-address dev 1)))
                 3))
    (define-test "device-address by page- and register address works"
      (pass-if-= (length (register-items (device-address dev 1 1)))
                 2))
    (define-test "device-address by page-, reg-, and item address (name) works"
      (pass-if-eq? (item-name (device-address dev 1 2 'stuff))
                   'stuff))
    (define-test "device-address by page-, reg-, and item address (int) works"
      (pass-if-eq? (item-name (device-address dev 1 1 1))
                   'stuff*))
    (define-test "device-address by page-, reg-addr, name and count works"
      (pass-if-eq? (item-name (device-address dev 1 1 'more* 0))
                   'more*))
    (define-test "device-default works"
      (pass-if-equal? dev-default '((128 0) (0 0 111) (0 0 101))))
    (define-test "Extract value by page-address"
      (pass-if-equal? (device-value-address dev dev-default 1) '(0 0 111)))
    (define-test "Extract value by page-address with holes"
      (pass-if-equal? (device-value-address dev dev-default 10) '(0 0 101)))
    (define-test "Extract value by page-address and register-address"
      (pass-if-equal? (device-value-address dev dev-default 1 2) 111))
    (define-test "Extract value by page-address and register-address with holes"
      (pass-if-equal? (device-value-address dev dev-default 10 1025) 101))
    (define-test "Extract value by page-address and register-address and item-address"
      (pass-if-equal? (device-value-address dev dev-default 10 1025 2) 101))
    (define-test "Extract value by page-address and register-address and name+index"
      (pass-if-equal? (device-value-address dev dev-default 10 1025 'is 1) 101))
    (define-test "device-ref->address works"
      (pass-if-equal? (map (lambda (name) (device-ref->address dev name))
                           '(thing fish stuff* lots more-things))
                      '((0 0 0) (0 0 1) (1 1 1) (10 200 1) (10 1025 0)))))
  (define-test "Single combination parses correctly"
    (pass-if-equal?
     (device-combinations (generate-device #:combinations '(a 1 2)))
     '((a ~ 1 2))))
  (define-test "Multiple combinations parse correctly"
    (pass-if-equal?
     (device-combinations (generate-device #:combinations '(a 1) '(b 3 4)))
     '((a 1) (b ~ 3 4)))))
