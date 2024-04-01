;; Copyright (c) 2018-2021 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (test validate item-defaults-work)
  #:use-module (test tap)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote device)
  #:use-module (chip-remote item)
  #:use-module (chip-remote register)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote device)
  #:use-module (chip-remote semantics)
  #:export (item-defaults-work/check
            item-defaults-work/count))

(define (device-items dev)
  (apply append
         (map register-items
              (apply append
                     (map register-map-registers
                          (page-map-register-maps (device-page-map dev)))))))

(define-tap-test (pass-if-unsigned-int x)
  (and (integer? x)
       (>= x 0)))

(define (perform-test item)
  (let ((default (catch #t
                   (lambda () (item-default-raw item))
                   (lambda (k . a)
                     (cons* 'exception k a)))))
    (define-test (format #f "default for item ~a encodes to ~a"
                         (item-name item) default)
      (pass-if-unsigned-int default))))

(define (item-defaults-work/check dev cfg)
  (map perform-test (device-items dev)))

(define (item-defaults-work/count dev cfg)
  (length (device-items dev)))
