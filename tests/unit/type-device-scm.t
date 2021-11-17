;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote device)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote page-map))

(init-test-tap!)

(with-fs-test-bundle
    (plan 12)

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
    (define-test "device-ref by page-address works #1"
      (pass-if-= (length (register-map-table (device-ref dev 0)))
                 2))
    (define-test "device-ref by page-address works #2"
      (pass-if-= (length (register-map-table (device-ref dev 1)))
                 3))
    (define-test "device-ref by page- and register address works"
      (pass-if-= (length (register-items (device-ref dev 1 1)))
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
      (pass-if-equal? dev-default '((0  . ((   0 . 128)
                                           (   1 .   0)))
                                    (1  . ((   0 .   0)
                                           (   1 .   0)
                                           (   2 . 111)))
                                    (10 . ((   8 .   0)
                                           ( 200 .   0)
                                           (1025 . 101))))))
    (define-test "device-canonical works"
      (pass-if-equal? (map (lambda (name) (device-canonical dev name))
                           '(thing* fish stuff* lots more-things))
                      '((1 0 0) (0 0 1) (1 1 1) (10 200 1) (10 1025 0)))))
  (define-test "Single combination parses correctly"
    (pass-if-equal?
     (device-combinations (generate-device #:combinations '(a 1 2)))
     '((a 1 2))))
  (define-test "Multiple combinations parse correctly"
    (pass-if-equal?
     (device-combinations (generate-device #:combinations '(a 1) '(b 3 4)))
     '((a 1) (b 3 4)))))
