;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote utilities)
             (chip-remote page-map))

(init-test-tap!)

(define-page-map test-map-a
  (table (↔ (0 (rm→ (table (↔ (0 († (‣ thing 0 4)
                                    (‣ fish 4 4)))
                              (1 († (‣ more 0 4)
                                    (‣ stuff 4 4))))))))))

(define-page-map test-map-b
  (table (↔ (0 (rm→ (table (↔ (0 († (‣ thing 0 4)
                                    (‣ fish 4 4 (default #x8))))
                              (1 († (‣ more 0 4)
                                    (‣ stuff 4 4)))))))
            (1 (rm→ (table (↔ (0 († (‣ thing* 0 4)
                                    (‣ fish* 4 4)))
                              (1 († (‣ more* 0 4)
                                    (‣ stuff* 4 4)))
                              (2 († (‣ stuff 0 8 (default 111)))))))))))

(with-fs-test-bundle
  (plan 8)

  (define-test "new page-map works"
    (pass-if-true (page-map? test-map-a)))
  (define-test "page-map-ref by page-address works #1"
    (pass-if-= (length (register-map-table (page-map-ref test-map-b 0)))
               2))
  (define-test "page-map-ref by page-address works #2"
    (pass-if-= (length (register-map-table (page-map-ref test-map-b 1)))
               3))
  (define-test "page-map-ref by page- and register address works"
    (pass-if-= (length (register-items (page-map-ref test-map-b 1 1)))
               2))
  (define-test "page-map-address by page-, reg-, and item address (name) works"
    (pass-if-eq? (item-name (page-map-address test-map-b 1 2 'stuff))
                 'stuff))
  (define-test "page-map-address by page-, reg-, and item address (int) works"
    (pass-if-eq? (item-name (page-map-address test-map-b 1 1 1))
                 'stuff*))
  (define-test "page-map-address by page-, reg-addr, name and count works"
    (pass-if-eq? (item-name (page-map-address test-map-b 1 1 'more* 0))
                 'more*))
  (define-test "page-map-default works"
    (pass-if-equal? (page-map-default test-map-b)
                    '((0 . ((0 . 128)
                            (1 .   0)))
                      (1 . ((0 .   0)
                            (1 .   0)
                            (2 . 111)))))))
