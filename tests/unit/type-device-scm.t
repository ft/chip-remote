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
             (chip-remote page-map)
             (chip-remote utilities))

(init-test-tap!)

(with-fs-test-bundle
  (plan 12)

  (define-test "generate-page-map, call structure works"
    (pass-if-true
     (device?
      (device (name 'thing2000)
              (page-map
               (pm→ (table
                     (↔ (#xff (rm→ (table
                                    (↔ (0 († (‣ thing 0 4)
                                             (‣ fish 4 4)))
                                       (1 († (‣ more 0 4)
                                             (‣ stuff 4 4)))))))))))))))

  (let* ((p0 (rm→ (table (↔ (0 († (‣ foo 0 4)))))))
         (p1 (rm→ (table (↔ (0 († (‣ bar 0 4)))))))
         (pm (pm→ (table (↔ (0 p0) (1 p1)))))
         (dev (device (name 'thing) (page-map pm))))
    (define-test "page-map table length correct"
      (pass-if-= 2 (length (page-map-table (device-page-map dev))))))

  (let* ((dev (device
               (page-map
                (pm→
                 (table
                  (↔ ( 0 (rm→
                          (table
                           (↔ (   0 († (‣ thing 0 4)
                                       (‣ fish 4 4 (default 8))))
                              (   1 († (‣ more 0 4)
                                       (‣ stuff 4 4)))))))
                     ( 1 (rm→
                          (table (↔ (   0 († (‣ thing* 0 4)
                                             (‣ fish* 4 4)))
                                    (   1 († (‣ more* 0 4)
                                             (‣ stuff* 4 4)))
                                    (   2 († (‣ stuff 0 8 (default 111))))))))
                     (10 (rm→
                          (table (↔ (   8 († (‣ there 0 4)
                                             (‣ really 4 4)))
                                    ( 200 († (‣ is 0 4)
                                             (‣ lots 4 4)))
                                    (1025 († (‣ more-things 0 8 (default 101))))))))))))))
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
                      '((1 0 0) (0 0 1) (1 1 1) (10 200 1) (10 1025 0))))
    (define-test "Single combination parses correctly"
      (pass-if-equal?
       (device-combinations (device (inherit dev)
                                    (combinations '((a 1 2)))))
       '((a 1 2))))
    (define-test "Multiple combinations parse correctly"
      (pass-if-equal?
       (device-combinations (device (inherit dev)
                                    (combinations '((a 1) (b 3 4)))))
       '((a 1) (b 3 4))))))
