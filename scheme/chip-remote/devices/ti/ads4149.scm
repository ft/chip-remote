;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149)
  #:use-module (ice-9 optargs)
  #:use-module (chip-remote level-3)
  #:use-module (chip-remote protocol)
  #:use-module ((chip-remote devices ti ads4149 program)
                #:renamer (symbol-prefix-proc 'lvl2/))
  #:use-module (chip-remote devices ti ads4149 registers)
  #:export (decode-device
            decode-register
            decode-register-value
            read-register
            setup-connection
            write-register))

(define (setup-connection conn index)
  (throw 'cr-setup-connection-not-implemented-yet))

(define (read-register conn addr)
  (throw 'cr-read-register-not-implemented-yet))

(define (write-register conn addr value)
  (throw 'cr-write-register-not-implemented-yet))

(define* (decode-register-value addr value #:key (colour? (to-tty?)))
  (value-decoder ads4149-register-map register-width addr value colour?))

(define* (decode-register conn addr #:key (colour? (to-tty?)))
  (decode-register-value (read-register conn addr) addr #:colour? colour?))

(define* (decode-device conn #:key (colour? (to-tty?)))
  (device-decoder ads4149-register-map decoder-register conn colour?))

(define-bit-field-frontends
  (disable-clkout-fall-control register-address-enable-clkout-fall
                               lvl2/disable-clkout-fall-control)
  (disable-clkout-rise-control register-address-enable-clkout-rise
                               lvl2/disable-clkout-rise-control)
  (disable-freeze-offset-correction register-address-freeze-offset-correction
                                    lvl2/disable-freeze-offset-correction)
  (disable-gain register-address-disable-gain
                lvl2/disable-gain)
  (disable-high-performance-mode-2 register-address-high-performance-mode-2
                                   lvl2/disable-high-performance-mode-2)
  (disable-low-latency register-address-disable-low-latency
                       lvl2/disable-low-latency)
  (disable-lvds-swing-control register-address-enable-lvds-swing
                              lvl2/disable-lvds-swing-control)
  (disable-offset-correction register-address-enable-offset-correction
                             lvl2/disable-offset-correction)
  (disable-power-down-global register-address-power-down-global
                             lvl2/disable-power-down-global)
  (disable-power-down-outputs register-address-power-down-outputs
                              lvl2/disable-power-down-outputs)
  (disable-readout register-address-readout
                   lvl2/disable-readout)
  (disable-standby register-address-standby
                   lvl2/disable-standby)
  (enable-freeze-offset-correction register-address-freeze-offset-correction
                                   lvl2/enable-freeze-offset-correction)
  (enable-gain register-address-disable-gain
               lvl2/enable-gain)
  (enable-high-performance-mode-2 register-address-high-performance-mode-2
                                  lvl2/enable-high-performance-mode-2)
  (enable-low-latency register-address-disable-low-latency
                      lvl2/enable-low-latency)
  (enable-lvds-swing-control register-address-enable-lvds-swing
                             lvl2/enable-lvds-swing-control)
  (enable-offset-correction register-address-enable-offset-correction
                            lvl2/enable-offset-correction)
  (enable-power-down-global register-address-power-down-global
                            lvl2/enable-power-down-global)
  (enable-power-down-outputs register-address-power-down-outputs
                             lvl2/enable-power-down-outputs)
  (enable-readout register-address-readout
                  lvl2/enable-readout)
  (enable-standby register-address-standby
                  lvl2/enable-standby)
  (reset-device register-address-reset
                lvl2/reset-device)
  (set-clkout-fall-posn register-address-clkout-fall-posn
                        lvl2/set-clkout-fall-posn (mode value))
  (set-clkout-rise-posn register-address-clkout-rise-posn
                        lvl2/set-clkout-rise-posn (mode value))
  (set-cmos-clkout-strength register-address-cmos-clkout-strength
                            lvl2/set-cmos-clkout-strength (value))
  (set-custom-pattern-high register-address-custom-pattern-high
                           lvl2/set-custom-pattern-high (value))
  (set-custom-pattern-low register-address-custom-pattern-low
                          lvl2/set-custom-pattern-low (value))
  (set-data-format register-address-data-format
                   lvl2/set-data-format (value))
  (set-gain register-address-gain
            lvl2/set-gain (value))
  (set-low-speed register-address-low-speed
                 lvl2/set-low-speed (value))
  (set-lvds-clkout-strength register-address-lvds-clkout-strength
                            lvl2/set-lvds-clkout-strength (value))
  (set-lvds-cmos register-address-lvds-cmos
                 lvl2/set-lvds-cmos (value))
  (set-lvds-data-strength register-address-lvds-data-strength
                          lvl2/set-lvds-data-strength (value))
  (set-lvds-swing register-address-lvds-swing
                  lvl2/set-lvds-swing (value))
  (set-offset-correction-time-constant register-address-offset-correction-time-constant
                                       lvl2/set-offset-correction-time-constant (value))
  (set-offset-pedestal register-address-offset-pedestal
                       lvl2/set-offset-pedestal (value))
  (set-test-pattern register-address-test-pattern
                    lvl2/set-test-pattern (value)))
