;; -*- scheme -*-

;; Copyright (c) 2017-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(use-modules (test tap)
             (test setup)
             (srfi srfi-1)
             (chip-remote decode)
             (chip-remote decode to-text)
             (chip-remote device)
             (chip-remote devices linear-technology ltc6603)
             (chip-remote item)
             (chip-remote page-map)
             (chip-remote register)
             (chip-remote register-map)
             (chip-remote semantics))

(init-test-tap!)

(with-fs-test-bundle
 (plan 9)

 (let* ((reg (device-ref ltc6603 #f #f))
        (items (register-items reg)))
   (define-test "Device's register has the right number of items"
     (pass-if-= (length items) 5))
   (let ((boolean-item (car items)))
     (define-test (format #f "Device's first item is boolean (~a)"
                          (item-name boolean-item))
       (pass-if-eq? (semantics-name (item-semantics boolean-item)) 'boolean))
     (define-test "Decoding a boolean item works (enabled)"
       (pass-if-equal? (decode boolean-item 1) '(enable-output? . enabled)))
     (define-test "Decoding a boolean item works (disabled)"
       (pass-if-equal? (decode boolean-item 0) '(enable-output? . disabled))))
   (let ((tableitem (fourth items)))
     (define-test (format #f "Device'e first item is a table-lookup (~a)"
                          (item-name tableitem))
       (pass-if-eq? (car (semantics-range (item-semantics tableitem)
                                          (item-width tableitem)))
                    'table))
     (define-test "Decoding a lookup item works (div-by-32)"
       (pass-if-equal? (decode tableitem 1) '(low-pass-cfg . div-by-32)))
     (define-test "Decoding a lookup item works (div-by-512)"
       (pass-if-equal? (decode tableitem 0) '(low-pass-cfg . div-by-512))))

   ;; Test some regressions that were cause by the internal value rework.
   (define-test "Decoding a complete device works (minimal)"
     (pass-if-no-exception (decode ltc6603 (device-default ltc6603))))
   (define-test "Decoding a complete device works (to-text)"
     (pass-if-no-exception (decode-to-text ltc6603 (device-default ltc6603))))))
