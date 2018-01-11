;; Copyright (c) 2014-2018 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti lmk04828 conversions)
  #:use-module (chip-remote assemble)
  #:export (analog-delay->bits
            bits->analog-delay

            clkout-divider->bits
            bits->clkout-divider

            digital-delay-cnt->bits
            bits->digital-delay-cnt

            sysout-analog-delay->bits
            bits->sysout-analog-delay))

(define (clkout-divider->bits value)
  (with-constraints (value (>= 1) (<= 32))
    (if (= value 32) 0 value)))

(define (bits->clkout-divider bits)
  (if (zero? bits) 32 bits))

(define (digital-delay-cnt->bits value)
  (with-constraints (value (>= 1) (<= 16))
    (if (= value 16) 0 value)))

(define (bits->digital-delay-cnt bits)
  (if (zero? bits) 16 bits))

(define (multiple-of-25? x)
  (zero? (modulo x 25)))

(define (analog-delay->bits value)
  (with-constraints (value (>= 0)
                           (<= (* 23 25))
                           (multiple-of-25?))
    (/ value 25)))

(define (bits->analog-delay bits)
  (* 25 bits))

(define sdadly-offset 600)

(define (sdadly-valid-step-size? x)
  (or (zero? x)
      (zero? (modulo (- x sdadly-offset)
                     150))))

(define (sysout-analog-delay->bits value)
  (with-constraints (value (>= 0)
                           (<= (+ sdadly-offset (* 14 150)))
                           (sdadly-valid-step-size?))
    (if (zero? value)
        0
        (+ 1 (/ (- value sdadly-offset) 150)))))

(define (bits->sysout-analog-delay bits)
  (if (zero? bits)
      0
      (+ sdadly-offset (* (- bits 1) 150))))
