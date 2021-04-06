;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate has-register)
  #:use-module (test tap)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:export (has-register/count
            has-register/check))

(define (has-register/check dev cfg)
  (let-syntax ((define-test* (syntax-rules ()
                               ((_ dev fmt e0 e* ...)
                                (define-test (format #f fmt (device-name dev))
                                  e0 e* ...)))))
    (define-test* dev "Device ~a has a page-map"
      (pass-if-true (page-map? (device-page-map dev))))
    (define-test* dev "Device ~a page-map has a table"
      (pass-if-true (list? (page-map-table (device-page-map dev)))))
    (define-test* dev "Device ~a page-map table is not empty"
      (pass-if-true (> (length (page-map-table (device-page-map dev)))
                       0)))
    (define-test* dev "Device ~a page-map has a table"
      (pass-if-true (list? (page-map-table (device-page-map dev)))))
    (define-test* dev "And there is a register-map in ~a's page-map"
      (pass-if-true (register-map?
                     (cdar (page-map-table (device-page-map dev))))))
    (define-test* dev "The first register-map in ~a's page-map has a table"
      (pass-if-true (list (register-map-table
                           (cdar (page-map-table (device-page-map dev)))))))
    (define-test* dev "The first reg-map's table in ~a's page-map is not empty"
      (pass-if-true (> (length (register-map-table
                                (cdar (page-map-table (device-page-map dev)))))
                       0)))
    (define-test* dev
      "The first entry in that table of ~a's page-map is a register"
      (pass-if-true (register?
                     (cdar
                      (register-map-table
                       (cdar (page-map-table (device-page-map dev))))))))))

(define (has-register/count dev cfg)
  8)
