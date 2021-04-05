;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote device access)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote device spi)
  #:use-module (chip-remote device transmit)
  #:use-module (chip-remote protocol)
  #:export (access-bus->proc
            make-device-access
            device-access?
            da-bus
            da-transmit
            da-read
            da-write))

(define (default-read c p r)
  (if (not c)
      0
      (transmit c r)))

(define (default-write c p r v)
  (if (not c)
      0
      (transmit c v)))

(define-record-type <device-access>
  (make-device-access* bus transmit read write)
  device-access?
  (bus da-bus)
  (transmit da-transmit)
  ;; These shoud be interpreter script or scheme procedures too. If they are
  ;; scheme procedures, they take four arguments: connection, page-address,
  ;; register-address and value. They directly call the protocol primitives
  ;; that talk to a device.
  ;;
  ;; The interpreter script variant isn't implemented yet. The plan is to put
  ;; the connection into the interpreter's environment and then only take the
  ;; three latter arguments that the scheme functions do.
  (read da-read)
  (write da-write))

(define* (make-device-access #:key
                             (bus (make-device-access-spi))
                             (transmit (make-device-transmit))
                             (read default-read)
                             (write default-write))
  (make-device-access* bus transmit read write))

(define (access-bus->proc bus)
  (cond ((device-access-spi? bus)
         (lambda (conn port-idx)
           (let ((set* (lambda (k v) (set conn port-idx k v))))
             (set* 'frame-length (spi-frame-width bus))
             (set* 'bit-order (spi-bit-order bus))
             (set* 'rate (spi-bit-rate bus))
             (set* 'clk-phase-delay (spi-clk-phase-delay bus))
             (set* 'clk-polarity (spi-clk-polarity bus))
             (set* 'cs-polarity (spi-cs-polarity bus)))
           (init conn port-idx)))
        (else (throw 'unknown-bus-description bus))))
