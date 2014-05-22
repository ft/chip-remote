;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti ads4149 registers)
  #:use-module (chip-remote bit-decoders)
  #:use-module (chip-remote devices ti ads4149 tables)
  #:use-module (chip-remote register-map)
  #:export (register-width
            valid-register-address?))

(define (valid-register-address? a)
  (member a (map car ads4149-register-map)))

(define (posn-decode width value get cmos lvds)
  (let ((v (get value)))
    (list (cons 'lvds-mode (reverse-lookup lvds value))
          (cons 'cmos-mode (reverse-lookup cmos value)))))

(define (rise-posn-decode name offset width value)
  (posn-decode width
               value
               get-clkout-rise-posn-bits
               clkout-pos-rise-cmos
               clkout-pos-rise-lvds))

(define (fall-posn-decode name offset width value)
  (posn-decode width
               value
               get-clkout-fall-posn-bits
               clkout-pos-fall-cmos
               clkout-pos-fall-lvds))

(define register-width 8)

(define-register-map ads4149
  (#x0 (default-value #x0)
       (contents (readout 0 1 => logic-active-high)
                 (reset 1 1 => logic-active-high)))
  (#x1 (default-value #x0)
       (contents (lvds-swing 2 6 => lvds-swing-map)))
  (#x3 (default-value #x0)
       (contents (high-performance-mode-1 0 2 => high-performace-mode-map)))
  (#x25 (default-value #x0)
        (contents (test-pattern 0 3 => test-pattern-map)
                  (disable-gain 3 1 => logic-active-high)
                  (gain 4 4 => gain-map)))
  (#x26 (default-value #x0)
        (contents (lvds-data-strength 0 1 => lvds-strength-map)
                  (lvds-clkout-strength 1 1 => lvds-strength-map)))
  (#x3d (default-value #x0)
        (contents (enable-offset-correction 5 1 => logic-active-high)
                  (data-format 6 2 => data-format-map)))
  (#x3f (default-value #x0)
        (contents (custom-pattern-high 0 8)))
  (#x40 (default-value #x0)
        (contents (custom-pattern-low 2 6)))
  (#x41 (default-value #x0)
        (contents (enable-clkout-fall 0 1 => logic-active-high)
                  (clkout-rise-posn 1 2 => rise-posn-decode)
                  (enable-clkout-rise 3 1 => logic-active-high)
                  (cmos-clkout-strength 4 2 => lvds-strength-map)
                  (lvds-cmos 6 2 => lvds-cmos-select-map)))
  (#x42 (default-value #x0)
        (contents (standby 2 1 => logic-active-high)
                  (disable-low-latency 3 1 => logic-active-high)
                  (clkout-fall-posn 6 2 => fall-posn-decode)))
  (#x43 (default-value #x0)
        (contents (enable-lvds-swing 0 2 => lvds-swing-control-map)
                  (power-down-outputs 4 1 => logic-active-high)
                  (power-down-global 6 1 => logic-active-high)))
  (#x4a (default-value #x0)
        (contents (high-performance-mode-2 0 1 => logic-active-high)))
  (#xbf (default-value #x0)
        (contents (offset-pedestal 2 6 => twos-complement)))
  (#xcf (default-value #x0)
        (contents (offset-correction-time-constant 2 4 => correction-time-map)
                  (freeze-offset-correction 7 1 => logic-active-high)))
  (#xdf (default-value #x0)
        (contents (low-speed 4 2 => low-speed-map))))
