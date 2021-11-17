;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (chip-remote item)
             (chip-remote register)
             (chip-remote register-map))

(init-test-tap!)

(define *register-one* (generate-register #:contents
                                          (foo 0 4)
                                          (bar 4 4)))
(define *register-two* (generate-register #:contents
                                          (baz 0 4 #:default #b0110)
                                          (boz 4 4)))

(define pp (@@ (ice-9 pretty-print) pretty-print))

(with-fs-test-bundle
 (plan 7)

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
   (define-test "register-map-ref reg1 works"
     (pass-if-equal? (register-map-ref rm 1)
                     *register-one*))

   (define-test "register-map-address item by name works"
     (pass-if-eq? (item-name (register-map-address rm 1 'bar))
                  'bar))
   (define-test "register-map-ref item by index works"
     (pass-if-eq? (item-name (register-map-ref rm 1 0))
                  'foo))
   (define-test "register-map-address item by name and index works"
     (pass-if-eq? (item-name (register-map-address rm 1 'foo 0))
                  'foo))
   (define-test "register-map-address by name works"
     (pass-if-eq? (item-name (register-map-address rm 'boz))
                  'boz))
   (define-test "register-map-default works"
     (pass-if-equal? (register-map-default rm)
                     '((0 . 0) (1 . 0) (2 . 6))))))
