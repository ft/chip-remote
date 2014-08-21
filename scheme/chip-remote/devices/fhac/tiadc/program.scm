;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices fhac tiadc program)
  #:use-module (chip-remote assemble)
  #:use-module (chip-remote devices fhac tiadc registers)
  #:use-module (chip-remote devices fhac tiadc tables)
  #:export (set-buffer-a
            set-buffer-b
            set-buffer-c
            set-buffer-d
            set-extended-cmd-data
            set-extended-cmd-word
            set-readout-length
            set-readout-mask
            set-readout-mode))

(define (check-eight-bit-range value)
  (with-constraints (value (>= 0) (<= #b11111111))
    value))

(define (set-buffer-a regval value)
  (set-buffer-a-bits regval (check-eight-bit-range value)))

(define (set-buffer-b regval value)
  (set-buffer-b-bits regval (check-eight-bit-range value)))

(define (set-buffer-c regval value)
  (set-buffer-c-bits regval (check-eight-bit-range value)))

(define (set-buffer-d regval value)
  (set-buffer-d-bits regval (check-eight-bit-range value)))

(define (set-readout-length regval value)
  (set-readout-length-bits regval (check-eight-bit-range value)))

(define (set-readout-mask regval value)
  (set-readout-mask-bits regval (value->bits readout-mask-table value)))

(define (set-readout-mode regval value)
  (set-readout-mode-bits regval (value->bits readout-mode-table value)))

(define (set-extended-cmd-data regval value)
  (with-constraints (value (>= 0) (<= #b111))
    (set-extended-cmd-data-bits regval value)))

(define (set-extended-cmd-word regval value)
  (set-extended-cmd-word-bits regval (value->bits extended-command-map value)))
