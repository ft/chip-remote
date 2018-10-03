;; Copyright (c) 2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote device transfer)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 optargs)
  #:export (make-device-transfer
            device-transfer?
            transfer-type
            transfer-variant
            transfer-transform))

(define-record-type <device-transfer>
  (make-device-transfer* type variant transform)
  device-transfer?
  ;; read-write write-only
  (type transfer-type)
  ;; single-register full-table
  (variant transfer-variant)
  ;; Maybe a full-table transfer needs a specific order for the single
  ;; registers to be transferred. This should be a scheme procedure or an
  ;; interpreter script, although currently the interpreter language is
  ;; probably not too useful to implement these.
  (transform transfer-transform))

(define* (make-device-transfer #:key
                               (type 'read-write)
                               (variant 'single-register)
                               (transform identity))
  (make-device-transfer* type variant transform))
