;; -*- scheme -*-

;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (srfi srfi-1)
             (chip-remote decode)
             (chip-remote device)
             (chip-remote devices linear-technology ltc6603)
             (chip-remote item)
             (chip-remote page-map)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote semantics))

(primitive-load "tests/test-tap-cfg.scm")

(with-fs-test-bundle
 (plan 7)

 (let* ((reg (device-register ltc6603))
        (items (register-items reg)))
   (define-test "Device's register has the right number of items"
     (pass-if-= (length items) 5))
   (let ((boolean-item (car items)))
     (define-test (format #f "Device's first item is boolean (~a)"
                          (item-name boolean-item))
       (pass-if-eq? (semantics-type (item-semantics boolean-item)) 'boolean))
     (define-test "Decoding a boolean item works (enabled)"
       (pass-if-equal? (decode boolean-item 1) '(enable-output? . enabled)))
     (define-test "Decoding a boolean item works (disabled)"
       (pass-if-equal? (decode boolean-item 0) '(enable-output? . disabled))))
   (let ((tableitem (fourth items)))
     (define-test (format #f "Device'e first item is a table-lookup (~a)"
                          (item-name tableitem))
       (pass-if-eq? (semantics-type (item-semantics tableitem)) 'table-lookup))
     (define-test "Decoding a lookup item works (div-by-32)"
       (pass-if-equal? (decode tableitem 1) '(low-pass-cfg . div-by-32)))
     (define-test "Decoding a lookup item works (div-by-512)"
       (pass-if-equal? (decode tableitem 0) '(low-pass-cfg . div-by-512))))))
