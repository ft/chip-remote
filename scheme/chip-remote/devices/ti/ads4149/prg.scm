;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149 prg)
  #:use-module (chip-remote assemble)
  #:use-module (chip-remote devices ti ads4149 tables)
  #:use-module (chip-remote devices ti ads4149 registers))
  #:export (disable-clkout-fall-control
            disable-clkout-rise-control
            disable-freeze-offset-correction
            disable-gain
            disable-high-performance-1
            disable-high-performance-2
            disable-low-latency
            disable-lvds-swing-control
            disable-offset-correction
            disable-powerdown-global
            disable-powerdown-outputs
            disable-serial-readout
            disable-standby
            enable-clkout-fall-control
            enable-clkout-rise-control
            enable-freeze-offset-correction
            enable-gain
            enable-high-performance-1
            enable-high-performance-2
            enable-low-latency
            enable-lvds-swing-control
            enable-offset-correction
            enable-powerdown-global
            enable-powerdown-outputs
            enable-serial-readout
            enable-standby
            reset-device
            set-clkout-fall-pos-cmos
            set-clkout-fall-pos-lvds
            set-clkout-lvds-strength
            set-clkout-rise-pos-cmos
            set-clkout-rise-pos-lvds
            set-cmos-clkout-strength
            set-custom-pattern-high
            set-custom-pattern-low
            set-data-format
            set-data-lvds-strength
            set-gain
            set-low-speed-mode
            set-lvds-cmos-select
            set-lvds-swing
            set-offset-correction-time-constant
            set-offset-pedestal
            set-test-pattern))

(define (disable-clkout-fall-control regval)
  (unset-logic-active-high set-enable-clkout-fall-bits))

(define (disable-clkout-rise-control regval)
  (unset-logic-active-high set-enable-clkout-rise-bits))

(define (disable-freeze-offset-correction regval)
  (unset-logic-active-high set-freeze-offset-correction-bits regval))

(define (disable-gain regval)
  (unset-logic-active-low set-disable-gain-bits regval))

(define (disable-high-performance-1 regval)
  (set-high-performance-mode-1-bits regval #b00))

(define (disable-high-performance-2 regval)
  (unset-logic-active-high set-high-performance-mode-2-bits regval))

(define (disable-low-latency regval)
  (unset-logic-active-low set-disable-low-latency-bits regval))

(define (disable-lvds-swing-control regval)
  (set-enable-lvds-swing-bits regval #b00))

(define (disable-offset-correction regval value)
  (unset-logic-active-high set-enable-offset-correction-bits))

(define (disable-powerdown-global regval)
  (unset-logic-active-high set-power-down-global regval))

(define (disable-powerdown-outputs regval)
  (unset-logic-active-high set-power-down-outputs regval))

(define (disable-serial-readout regval)
  (unset-logic-active-high set-readout-bits regval))

(define (disable-standby regval)
  (unset-logic-active-high set-standby-bits regval))

(define (enable-clkout-fall-control regval)
  (set-logic-active-high set-enable-clkout-fall-bits))

(define (enable-clkout-rise-control regval)
  (set-logic-active-high set-enable-clkout-rise-bits))

(define (enable-freeze-offset-correction regval)
  (set-logic-active-high set-freeze-offset-correction-bits regval))

(define (enable-gain regval)
  (set-logic-active-low set-disable-gain-bits regval))

(define (enable-powerdown-global regval)
  (set-logic-active-high set-power-down-global regval))

(define (enable-powerdown-outputs regval)
  (set-logic-active-high set-power-down-outputs regval))

(define (enable-high-performance-1 regval)
  (set-high-performance-mode-1-bits regval #b11))

(define (enable-high-performance-2 regval)
  (set-logic-active-high set-high-performance-mode-2-bits regval))

(define (enable-lvds-swing-control regval)
  (set-enable-lvds-swing-bits regval #b11))

(define (enable-low-latency regval)
  (set-logic-active-low set-disable-low-latency-bits regval))

(define (enable-offset-correction regval value)
  (set-logic-active-high set-enable-offset-correction-bits))

(define (enable-serial-readout regval)
  (set-logic-active-high set-readout-bits regval))

(define (enable-standby regval)
  (set-logic-active-high set-standby-bits regval))

(define (reset-device regval)
  (set-logic-active-high set-reset-bits regval))

(define (set-clkout-cmos-strength regval value)
  (set-cmos-clkout-strength regval (value->bits cmos-clkout-strength-map
                                                    value)))

(define (set-clkout-lvds-strength regval value)
  (set-lvds-clkout-strength regval (value->bits lvds-strength-map value)))

(define (set-clkout-rise-pos-lvds regval value)
  (set-clkout-rise-posn-bits regval (value->bits clkout-pos-rise-lvds
                                                     value)))

(define (set-clkout-rise-pos-cmos regval value)
  (set-clkout-rise-posn-bits regval (value->bits clkout-pos-rise-cmos
                                                     value)))

(define (set-clkout-fall-pos-lvds regval value)
  (set-clkout-fall-posn-bits regval (value->bits clkout-pos-fall-lvds
                                                     value)))

(define (set-clkout-fall-pos-cmos regval value)
  (set-clkout-fall-posn-bits regval (value->bits clkout-pos-fall-cmos

(define (set-custom-pattern-high regval value)
  (with-constraints (value (>= 0) (<= #b11111111))
    (set-custom-pattern-high-bits regval value)))

(define (set-custom-pattern-low regval value)
  (with-constraints (value (>= 0) (<= #b111111))
    (set-custom-pattern-low-bits regval value)))

(define (set-data-format regval value)
  (set-data-format-bits regval (value->bits data-format-map value)))

(define (set-data-lvds-strength regval value)
  (set-lvds-data-strength regval (value->bits lvds-strength-map value)))

(define (set-lvds-swing regval level)
  (set-lvds-swing-bits regval (value->bits lvds-swing-map level)))

(define (set-gain regval value)
  "Set the device's gain to VALUE. The valid range is from 0 to 6dB in half-dB
steps. VALUE has to be an exact number (i.e. an integer or an exact rational
number)."
  (set-gain-bits regval (value->bits gain-map value)))

(define (set-low-speed-mode regval value)
  (set-low-speed-bits regval (value->bits low-speed-map value)))
                                                     value)))

(define (set-lvds-cmos-select regval value)
  (set-lvds-cmos-bits regval (value->bits lvds-cmos-select-map value)))

(define (set-offset-pedestal regval value)
  (set-offset-pedestal-bits regval (value->twos-complement value 6)))

(define (set-offset-correction-time-constant regval value)
  (set-offset-correction-time-constant-bits (value->bits correction-time-map
                                                             value)))

(define (set-test-pattern regval value)
  (set-test-pattern-bits regval (value->bits test-pattern-map value)))
