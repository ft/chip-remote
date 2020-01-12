;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote device access)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote device spi)
  #:use-module (chip-remote device transmit)
  #:use-module (chip-remote protocol)
  #:use-module (chip-remote pretty-print)
  #:export (access-bus->proc
            make-device-access
            device-access?
            da-bus
            da-transmit
            da-read
            da-write))

(define-record-type <device-access>
  (make-device-access* bus transmit read write)
  device-access?
  (bus da-bus)
  (transmit da-transmit)
  ;; These shoud be interpreter script or scheme procedures too.
  (read da-read)
  (write da-write))

(define (pp-device-access da)
  `(wrap "#<" ">"
         (type device-access) (newline)
         (indent complex
                 (key bus) (space ,(da-bus da)) (newline)
                 (key transmit) (space ,(da-transmit da)) (newline)
                 (key read) (space ,(da-read da)) (newline)
                 (key write) (space ,(da-write da)) (newline))))

(set-record-type-printer! <device-access>
  (lambda (da port)
    (pp-eval port (pp-device-access da))))

(define* (make-device-access #:key
                             (bus (make-device-access-spi))
                             (transmit (make-device-transmit))
                             (read (lambda (pa ra) ra))
                             (write (lambda (pa ra value) value)))
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
