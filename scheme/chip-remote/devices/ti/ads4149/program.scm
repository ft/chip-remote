;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149 program)
  #:use-module (chip-remote assemble)
  #:use-module (chip-remote devices ti ads4149 registers)
  #:use-module (chip-remote devices ti ads4149 tables)
  #:export (disable-clkout-fall-control
            disable-clkout-rise-control
            disable-freeze-offset-correction
            disable-gain
            disable-high-performance-mode-2
            disable-low-latency
            disable-offset-correction
            disable-power-down-global
            disable-power-down-outputs
            disable-readout
            disable-standby
            enable-clkout-fall-control
            enable-clkout-rise-control
            enable-freeze-offset-correction
            enable-gain
            enable-high-performance-mode-2
            enable-low-latency
            enable-offset-correction
            enable-power-down-global
            enable-power-down-outputs
            enable-readout
            enable-standby
            reset-device
            set-clkout-fall-posn
            set-clkout-rise-posn
            set-cmos-clkout-strength
            set-custom-pattern-high
            set-custom-pattern-low
            set-data-format
            set-enable-lvds-swing
            set-gain
            set-high-performance-mode-1
            set-low-speed
            set-lvds-clkout-strength
            set-lvds-cmos
            set-lvds-data-strength
            set-lvds-swing
            set-offset-correction-time-constant
            set-offset-pedestal
            set-test-pattern))

(define (disable-clkout-fall-control regval)
  (unset-logic-active-high set-enable-clkout-fall-bits regval))

(define (disable-clkout-rise-control regval)
  (unset-logic-active-high set-enable-clkout-rise-bits regval))

(define (disable-freeze-offset-correction regval)
  (unset-logic-active-high set-freeze-offset-correction-bits regval))

(define (disable-gain regval)
  (unset-logic-active-low set-disable-gain-bits regval))

(define (disable-high-performance-mode-2 regval)
  (unset-logic-active-high set-high-performance-mode-2-bits regval))

(define (disable-low-latency regval)
  (unset-logic-active-low set-disable-low-latency-bits regval))

(define (disable-offset-correction regval)
  (unset-logic-active-high set-enable-offset-correction-bits regval))

(define (disable-power-down-global regval)
  (unset-logic-active-high set-power-down-global-bits regval))

(define (disable-power-down-outputs regval)
  (unset-logic-active-high set-power-down-outputs-bits regval))

(define (disable-readout regval)
  (unset-logic-active-high set-readout-bits regval))

(define (disable-standby regval)
  (unset-logic-active-high set-standby-bits regval))

(define (enable-clkout-fall regval)
  (set-logic-active-high set-enable-clkout-fall-bits regval))

(define (enable-clkout-rise regval)
  (set-logic-active-high set-enable-clkout-rise-bits regval))

(define (enable-freeze-offset-correction regval)
  (set-logic-active-high set-freeze-offset-correction-bits regval))

(define (enable-gain regval)
  (set-logic-active-low set-disable-gain-bits regval))

(define (enable-high-performance-mode-2 regval)
  (set-logic-active-high set-high-performance-mode-2-bits regval))

(define (enable-low-latency regval)
  (set-logic-active-low set-disable-low-latency-bits regval))

(define (enable-offset-correction regval)
  (set-logic-active-high set-enable-offset-correction-bits regval))

(define (enable-power-down-global regval)
  (set-logic-active-high set-power-down-global-bits regval))

(define (enable-power-down-outputs regval)
  (set-logic-active-high set-power-down-outputs-bits regval))

(define (enable-readout regval)
  (set-logic-active-high set-readout-bits regval))

(define (enable-standby regval)
  (set-logic-active-high set-standby-bits regval))

(define (reset-device regval)
  (set-logic-active-high set-reset-bits regval))

(define (set-clkout-fall-posn regval mode value)
  (with-constraints (mode (memq '(cmos lvds)))
    (set-clkout-fall-posn-bits regval (value->bits (if (eq? mode cmos)
                                                       clkout-pos-fall-cmos
                                                       clkout-pos-fall-lvds)
                                                   value))))

(define (set-clkout-rise-posn regval mode value)
  (with-constraints (mode (memq '(cmos lvds)))
    (set-clkout-rise-posn-bits regval (value->bits (if (eq? mode cmos)
                                                       clkout-pos-rise-cmos
                                                       clkout-pos-rise-lvds)
                                                   value))))

(define (set-cmos-clkout-strength regval value)
  (set-cmos-clkout-strength-bits regval (value->bits lvds-strength-map value)))

(define (set-custom-pattern-high regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-custom-pattern-high-bits regval value)))

(define (set-custom-pattern-low regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-custom-pattern-low-bits regval value)))

(define (set-data-format regval value)
  (set-data-format-bits regval (value->bits data-format-map value)))

(define (set-enable-lvds-swing regval value)
  (set-enable-lvds-swing-bits regval (value->bits lvds-swing-control-map value)))

(define (set-gain regval value)
  "Set the device's gain to VALUE. The valid range is from 0 to 6dB in half-dB
steps. VALUE has to be an exact number (i.e. an integer or an exact rational
number)."
  (set-gain-bits regval (value->bits gain-map value)))

(define (set-high-performance-mode-1 regval value)
  (set-high-performance-mode-1-bits regval (value->bits high-performace-mode-map value)))

(define (set-low-speed regval value)
  (set-low-speed-bits regval (value->bits low-speed-map value)))

(define (set-lvds-clkout-strength regval value)
  (set-lvds-clkout-strength-bits regval (value->bits lvds-strength-map value)))

(define (set-lvds-cmos regval value)
  (set-lvds-cmos-bits regval (value->bits lvds-cmos-select-map value)))

(define (set-lvds-data-strength regval value)
  (set-lvds-data-strength-bits regval (value->bits lvds-strength-map value)))

(define (set-lvds-swing regval value)
  (set-lvds-swing-bits regval (value->bits lvds-swing-map value)))

(define (set-offset-correction-time-constant regval value)
  (set-offset-correction-time-constant-bits regval (value->bits correction-time-map value)))

(define (set-offset-pedestal regval value)
  (set-offset-pedestal-bits regval (value->twos-complement value)))

(define (set-test-pattern regval value)
  (set-test-pattern-bits regval (value->bits test-pattern-map value)))
