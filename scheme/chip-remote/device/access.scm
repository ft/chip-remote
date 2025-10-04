;; Copyright (c) 2018-2025 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote device access)
  #:use-module (chip-remote protocol)
  #:use-module (data-structures records)
  #:use-module (data-structures records utilities)
  #:export (device-operations
            make-device-operations
            device-operations?
            device-operations-make-setup
            device-operations-read
            device-operations-read-parse
            device-operations-write
            device-operations-write-parse
            access-not-implemented
            device-bus
            device-bus?
            device-bus-compatible?
            device-bus-interface
            device-bus-type
            device-bus-setup
            device-bus-xfer
            make-device-bus
            make-i2c
            make-spi
            fake-spi))

(define-record-type* <device-operations>
  device-operations make-device-operations
  device-operations? this-device-operations
  (setup        device-operations-make-setup)
  (read         device-operations-read)
  (read-parse   device-operations-read-parse)
  (write        device-operations-write)
  (write-parse  device-operations-write-parse))

(define niy (lambda _ (throw 'not-implemented-yet)))

(define access-not-implemented
  (device-operations
   (setup        niy)
   (read         niy)
   (read-parse   niy)
   (write        niy)
   (write-parse  niy)))

(define-record-type* <device-bus>
  device-bus make-device-bus device-bus? this-device-bus
  (type      device-bus-type  (sanitize (need 'type symbol?)))
  (setup     device-bus-setup (default cr:setup-interface!))
  (xfer      device-bus-xfer)
  (interface device-bus-interface))

(define (make-spi ifc)
  (device-bus (type      'spi)
              (xfer      cr:spi-transceive!)
              (interface ifc)))

(define (make-i2c ifc)
  (device-bus (type      'i2c)
              (xfer      cr:i2c-transceive!)
              (interface ifc)))

(define fake-spi
  (device-bus
   (type 'spi)
   (interface 'spi-0)
   (setup (lambda (c ifc . lst)
            (format #t "setup: ~a~%" lst)))
   (xfer  (lambda (c ifc lst)
            (format #t "xfer: ~a~%" lst)
            (make-list (length lst) 0)))))

(define (device-bus-compatible? c b)
  (let ((ctrl (proto-get-ifc-ctrl! c (device-bus-interface b))))
    (eq? (device-bus-type b)
         (assq-ref ctrl 'type))))
