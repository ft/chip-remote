;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote device transmit)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 optargs)
  #:export (make-device-transmit
            device-transmit?
            transmit-type
            transmit-variant
            transmit-transform))

(define-record-type <device-transmit>
  (make-device-transmit* type variant transform)
  device-transmit?
  ;; read-write write-only
  (type transmit-type)
  ;; single-register full-table
  (variant transmit-variant)
  ;; Maybe a full-table transmit needs a specific order for the single
  ;; registers to be transmitred. This should be a scheme procedure or an
  ;; interpreter script, although currently the interpreter language is
  ;; probably not too useful to implement these.
  (transform transmit-transform))

(define* (make-device-transmit #:key
                               (type 'read-write)
                               (variant 'single-register)
                               (transform identity))
  (make-device-transmit* type variant transform))
