;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in doc/LICENCE.

(define-module (chip-remote devices fhac tiadc registers)
  #:use-module (chip-remote devices fhac tiadc tables)
  #:use-module (chip-remote register-map)
  #:use-module (chip-remote bit-decoders)
  #:use-module (chip-remote assemble)
  #:export (tiadc-register-width))

(define tiadc-register-width 8)

(define (decode-length n o w value)
  (format #f "~d block~:p" value))

(define-register-map tiadc
  (0 (default-value 0)
     (contents (readout-mask 0 3 => readout-mask-table)))
  (1 (default-value 0)
     (contents (readout-mode 0 1 => readout-mode-table)))
  (2 (default-value 0)
     (contents (readout-length 0 8 => decode-length)))
  (3 (default-value 0)
     (contents (buffer-a 0 8 => literal-hex)))
  (4 (default-value 0)
     (contents (buffer-b 0 8 => literal-hex)))
  (5 (default-value 0)
     (contents (buffer-c 0 8 => literal-hex)))
  (6 (default-value 0)
     (contents (buffer-d 0 8 => literal-hex)))
  ;; This is actually a special purpose register. It excepts extended commands,
  ;; such as resetting the device, reading registers, triggering transfers and
  ;; the like. It is a read-only register, listed here only for completeness.
  (7 (default-value 0)
     (contents (extended-cmd-word 0 5 => extended-command-map)
               (extended-cmd-data 5 3))))

(define (combine-buffer data)
  (combine-value-width-pairs data '(buffer-d buffer-c buffer-b buffer-a)))

(define (finally-buffer name value)
  (literal-hex name 0 32 value))

(define-register-interconns tiadc
  (combine (buffer-d buffer-c buffer-b buffer-a)
           #:into buffer
           #:combine combine-buffer
           #:finally finally-buffer))
