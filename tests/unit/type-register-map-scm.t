;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote utilities))

(init-test-tap!)

(define *register-one* († (‣ foo 0 4)
                          (‣ bar 4 4)))
(define *register-two* († (‣ baz 0 4 (default #b0110))
                          (‣ boz 4 4)))

(define-register-map test-map (table (↔ (0 († (‣ thing 0 4)
                                              (‣ fish 4 4)))
                                        (1 *register-one*)
                                        (2 *register-two*))))

(with-fs-test-bundle
  (plan 7)

  (define-test "generate-register, call structure works"
    (pass-if-true (register-map? test-map)))
  (define-test "register-map-ref reg1 works"
    (pass-if-equal? (register-map-ref test-map 1)
                    *register-one*))
  (define-test "register-map-address item by name works"
    (pass-if-eq? (item-name (register-map-address test-map 1 'bar))
                 'bar))
  (define-test "register-map-ref item by index works"
    (pass-if-eq? (item-name (register-map-ref test-map 1 0))
                 'foo))
  (define-test "register-map-address item by name and index works"
    (pass-if-eq? (item-name (register-map-address test-map 1 'foo 0))
                 'foo))
  (define-test "register-map-address by name works"
    (pass-if-eq? (item-name (register-map-address test-map 'boz))
                 'boz))
  (define-test "register-map-default works"
    (pass-if-equal? (register-map-default test-map)
                    '((0 . 0) (1 . 0) (2 . 6)))))
