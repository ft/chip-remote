;; Copyright (c) 2014 chip-remote workers, All rights reserved.
;;
;; Terms for redistribution and use can be found in LICENCE.

(define-module (chip-remote devices ti lmk04828 tables)
  #:export (clkout-mux-map
            devclk-sysref-map
            oscout-format-map
            oscout-mux-map
            output-format-map
            output-polarity-map
            sdclkout-ddelay-map
            sysref-mux-map
            sysref-output-state-map
            vco-mux-map))

(define clkout-mux-map
  '((divider-only . #b00)
    (div+dcc+hs   . #b01)
    (bypass       . #b10)
    (analog+div   . #b11)))

(define devclk-sysref-map
  '((device-clock . #b0)
    (sysref       . #b1)))

(define sdclkout-ddelay-map
  '((reserved . #x0)
    (2  . #x1)
    (3  . #x2)
    (4  . #x3)
    (5  . #x4)
    (6  . #x5)
    (7  . #x6)
    (8  . #x7)
    (9  . #x8)
    (10 . #x9)
    (11 . #xa)
    (reserved . #xb)
    (reserved . #xc)
    (reserved . #xd)
    (reserved . #xe)
    (reserved . #xf)))

(define sysref-output-state-map
  '((normal . #b00)
    (active . #b01)
    (active . #b10)
    (vcm-voltage . #b11)))

(define output-polarity-map
  '((normal . #b0)
    (inverted . #b1)))

(define output-format-map
  '((powerdown     . #b000)
    (lvds          . #b001)
    (hsds-6mA      . #b010)
    (hsds-8mA      . #b011)
    (hsds-10mA     . #b100)
    (lvpecl-1600mV . #b101)
    (lvpecl-2000mV . #b110)
    (lvpecl        . #b111)))

(define vco-mux-map
  '((VCO-0        . #b00)
    (VCO-1        . #b01)
    (external-VCO . #b10)
    (reserver     . #b11)))

(define oscout-mux-map
  '((buffered-oscin . 0)
    (feedback-mux   . 1)))

(define oscout-format-map
  '((powerdown        . #x0)
    (lvds             . #x1)
    (reserved         . #x2)
    (reserved         . #x3)
    (lvpecl-1600mV    . #x4)
    (lvpecl-2000mV    . #x5)
    (lvcmos-norm-inv  . #x6)
    (lvcmos-inv-norm  . #x7)
    (lvcmos-norm-norm . #x8)
    (lvcmos-inv-inv   . #x9)
    (lvcmos-off-norm  . #xa)
    (lvcmos-off-inv   . #xb)
    (lvcmos-norm-off  . #xc)
    (lvcmos-inv-off   . #xd)
    (lvcmos-off-off   . #xe)
    (reserved         . #xf)))

(define sysref-mux-map
  '((normal-sync     . #b00)
    (re-clocked      . #b01)
    (sysref-pulser   . #b10)
    (sysref-contious . #b11)))
