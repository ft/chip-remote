;; Copyright (c) 2017-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote decode)
  #:use-module (srfi srfi-1)
  #:use-module (chip-remote codecs)
  #:use-module (chip-remote decode types)
  #:use-module (chip-remote interpreter)
  #:use-module (chip-remote device)
  #:use-module (chip-remote page-map)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote register)
  #:use-module (chip-remote item)
  #:use-module (chip-remote semantics)
  #:export (decode decode* ?))

(define (decode* desc value)
  (cond ((item? desc)
         (let ((item-decoded (item-decode desc value)))
           (make-item/decoder item-decoded value desc)))
        ((register? desc)
         (make-register/decoder
          value
          (map (lambda (x)
                 (let ((getter (item-get x)))
                   (decode* x (getter value))))
               (register-items desc))
          desc))
        ((register-map? desc)
         (make-register-map/decoder
          value
          (map (lambda (r v)
                 (cons (car r)
                       (decode* (cdr r) v)))
               (register-map-table desc)
               value)
          desc))
        ((page-map? desc)
         (make-page-map/decoder
          value
          (map (lambda (rm v)
                 (cons (car rm)
                       (decode* (cdr rm) v)))
               (page-map-table desc)
               value)
          desc))
        ((device? desc)
         (make-device/decoder
          value
          (let ((pm (device-page-map desc)))
            (make-page-map/decoder
             value
             (map (lambda (rm v)
                    (cons (car rm)
                          (decode* (cdr rm) v)))
                  (page-map-table pm)
                  value)
             pm))
          desc))
        (else (throw 'invalid-data-type desc))))

(define (? thing)
  (cond ((decoder-item? thing)
         (cons (item-name (decoder-item-description thing))
               (decoder-item-decoded thing)))
        ((decoder-register? thing)
         (map ? (decoder-register-items thing)))
        ((decoder-register-map? thing)
         (map (lambda (x) (cons (car x) (? (cdr x))))
              (decoder-register-map-registers thing)))
        ((decoder-page-map? thing)
         (map (lambda (x)
                (cons (car x)
                      (map ? (decoder-register-map-registers (cdr x)))))
              (decoder-page-map-register-maps thing)))
        ((decoder-device? thing)
         (? (decoder-device-page-map thing)))
        ((list? thing)
         (map ? thing))
        ((pair? thing)
         (cons (? (car thing))
               (? (cdr thing))))
        (else thing)))

(define (decode description value)
  (? (decode* description value)))
